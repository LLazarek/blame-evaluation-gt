#lang at-exp racket

(require "../mutate/type-api-mutators.rkt")

(struct mutated-interface-type (name original mutated mutation-type)
  #:transparent)

(struct difference (left right)
  #:transparent)

;; A B -> (or/c A (difference A B))
(define (compare left right)
  (if (equal? left right)
      left
      (difference left right)))

;; Dumb diff of s1 and s2, assuming both have the same length
;; sexp? sexp? -> sexp?
(define (sexp-diff s1 s2)
  (let loop ([result empty]
             [s1-remaining s1]
             [s2-remaining s2])
    (match* {s1-remaining s2-remaining}
      [{(cons head-1 tail-1)
        (cons head-2 tail-2)}
       (loop (cons (loop empty head-1 head-2)
                   result)
             tail-1
             tail-2)]
      [{'() '()}
       (reverse result)]
      [{atom1 atom2}
       (compare   atom1  atom2)])))

(module+ test
  (require ruinit)
  (test-begin
    #:name sexp-diff
    (test-equal? (sexp-diff 1 1)
                 1)
    (test-equal? (sexp-diff 1 2)
                 (difference 1 2))
    (test-equal? (sexp-diff '(1 2 3) '(1 2 3))
                 '(1 2 3))
    (test-equal? (sexp-diff '(1 2 3) '(1 3 2))
                 (list 1 (difference 2 3) (difference 3 2)))
    (test-equal? (sexp-diff '(1 2 (3 4) 5)
                            '(1 2 (4 3) 5))
                 `(1 2 (,(difference 3 4) ,(difference 4 3)) 5))
    (test-equal? (sexp-diff '(1 2 (3 4) (5 (6)))
                            '(1 2 (4 3) (5 (6))))
                 `(1 2 (,(difference 3 4) ,(difference 4 3)) (5 (6))))))

;; annotate-path-to-mutated-layer : sexp? -> sexp?
;; Annotates the path down the given sexp (viewed as a tree) to `difference` nodes,
;; so that the parent of any `difference` node is annotated `here` and the
;; intermediate nodes in the path up to the root are annotated `down`.
;; Example: '(1 (2 3 ((difference 4 5) 5)))
;; -> '(down (1 (down (2 3 (here ((difference 4 5) 5))))))
(struct path (expr) #:transparent)
(struct down path () #:transparent)
(struct here path () #:transparent)
(define (annotate-path-to-mutated-layer diff)
  (let loop ([layer diff])
    (match layer
      [(list _ ... (? difference?) _ ...)
       (here layer)]
      [(list sub-parts ...)
       (match (map loop sub-parts)
         [(and annotated-parts
               (list (? (negate path?)) ...
                     (? path?)
                     (? (negate path?)) ...))
          (down annotated-parts)]
         [pathless-parts pathless-parts])]
      [(? difference?) (here layer)]
      [atom atom])))


(define (mapping f)
  (λ (l) (map f l)))

;; type-diff
(struct td () #:transparent)
(struct td:->arg td (index-map) #:transparent)
(struct td:->result td (index-map) #:transparent)
(struct td:base td (original new) #:transparent)
(define (sexp->type-diff a-sexp-diff)
  (define (list->td-index-map a-list)
    (match a-list
      [(? td? single) `((0 ,single))]
      [mixed-list
       (for/list ([maybe-td (in-list mixed-list)]
                  [index    (in-naturals)]
                  #:when (td? maybe-td))
         (list index maybe-td))]))
  (match a-sexp-diff
    [(list* '-> (app (mapping sexp->type-diff)
                     (list #f ...
                           (or (? td? sub-tds)
                               (list* 'values
                                      (and (list _ ... (? td?) _ ...)
                                           sub-tds))))))
     (td:->result (list->td-index-map sub-tds))]
    [(list* '-> (app (mapping sexp->type-diff)
                     (and (list _ ... (? td?) _ ... #f)
                          sub-tds)))
     (td:->arg (list->td-index-map sub-tds))]
    [(difference original new)
     (td:base original new)]
    [(list* 'values (app (mapping sexp->type-diff)
                         result-tds))
     (and (ormap td? result-tds)
          (cons 'values result-tds))]
    [(? symbol?) #f]))

(module+ test
  (test-begin
    #:name sexp->type-diff
    (test-equal? (sexp->type-diff (sexp-diff 'A 'B))
                 (td:base 'A 'B))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A C)
                                             '(-> B C)))
                 (td:->arg `((0 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A Z Z Z C)
                                             '(-> B Z Z Z C)))
                 (td:->arg `((0 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z A Z C)
                                             '(-> Z Z B Z C)))
                 (td:->arg `((2 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> X Z A Z C)
                                             '(-> Y Z B Z C)))
                 (td:->arg `((0 ,(td:base 'X 'Y))
                             (2 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z A)
                                             '(-> Z Z Z B)))
                 (td:->result `((0 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values A))
                                             '(-> Z Z Z (values B))))
                 (td:->result `((0 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A))
                                             '(-> Z Z Z (values Z B))))
                 (td:->result `((1 ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A Z X))
                                             '(-> Z Z Z (values Z B Z Y))))
                 (td:->result `((1 ,(td:base 'A 'B))
                                (3 ,(td:base 'X 'Y)))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z (-> A C)))
                                             '(-> Z Z Z (values Z (-> B C)))))
                 (td:->result `((1 ,(td:->arg `((0 ,(td:base 'A 'B))))))))))

;; mutated-interface-type? -> contract?
(define (generate-adapter-ctc a-mutated-interface-type)
  (match-define (mutated-interface-type value-name
                                        original
                                        mutated
                                        mutation-type)
    a-mutated-interface-type)
  ;; Think about the diff sexp as layers. E.g.
  ;; (1 (2 3 (4 5)))
  ;; ^- ^--- ^--- innermost layer
  ;;  |    + middle layer
  ;;  + top layer
  ;;
  (define annotated-diff (annotate-path-to-mutated-layer (sexp-diff original mutated)))
  (define full-combinator
    (match mutation-type
      [(== type:base-type-substitution)
       (base-type-substitution-adapter annotated-diff)]
      ;; [type:function-arg-swap
      ;;  (function-arg-swap-adapter annotated-diff)]
      ;; [type:function-result-swap
      ;;  (function-result-swap-adapter annotated-diff)]
      ;; [type:vector-swap
      ;;  (vector-swap-adapter annotated-diff)]
      ;; [type:function-arg-drop
      ;;  (function-arg-drop-adapter annotated-diff)]
      ;; [type:function-result-drop
      ;;  (function-result-drop-adapter annotated-diff)]
      ;; [type:union-branch-drop
      ;;  (union-branch-drop-adapter annotated-diff)]
      )

    #;(let loop ([diff-layer (annotate-path-to-mutated-layer (sexp-diff original mutated))])
      (match diff-layer
        [(here (and mutated-layer
                    (list (or '-> '->*) _ ... (list 'values _ ... (? difference?) _ ...))))
         (make-adapter-combinator mutated-layer mutation-type)]

        ;; It's safe to assume that once we've found a layer with a difference,
        ;; that's the only layer with a difference -- since there's only one mutation,
        ;; and all the mutations only operate at a single layer of the type
        ;; (except for the return type mutations above)
        [(here (and mutated-layer
                    (list _ ... (? difference? diff) _ ...)))
         (make-adapter-combinator mutated-layer mutation-type)]
        [(down (and outer-layer
                    (list head before ... (? path? inner-layer) after ...)))
         (make-outer-combinator-layer outer-layer
                                      (loop inner-layer))])))
  full-combinator)

(define (base-type-substitution-adapter annotated-diff)
  (match annotated-diff
    [(or (here (? difference? diff))
         (? difference? diff))
     (make-value-adapter diff)]
    [(path (list (and head (or '-> '->*))
                 _ ...
                 (path (list 'values
                             before ...
                             (? (disjoin path? difference?) inner-diff)
                             after ...))))
     (make-delegating-layer (list head
                                  (append (list 'values)
                                          before
                                          (list (base-type-substitution-adapter inner-diff))
                                          after)))]
    [(path (list head before ... (? (disjoin path? difference?) inner-diff) after ...))
     (make-delegating-layer (append (list head)
                                    before
                                    (list (base-type-substitution-adapter inner-diff))
                                    after))]))

(require (for-syntax syntax/parse))
(define-match-expander with-default
  (syntax-parser
    [(_ pat
        [name:id default-val:expr] ...)
     #'(and pat
            (app (const default-val) name)
            ...)]))

;; (listof (or/c contract? sexp?)) -> contract?
(define (make-delegating-layer layer-with-adapters)
  (match layer-with-adapters
    [(list '->
           arguments ...
           (not (or (? adapter/c?)
                    (list 'values _ ... (? adapter/c?) _ ...))))
     (define adapter-index-pairs
       (for/list ([arg (in-list arguments)]
                  [i   (in-naturals)]
                  #:when (adapter/c? arg))
         (cons i arg)))
     (delegating-> #t
                   adapter-index-pairs)]
    [(list '->
           (not (? adapter/c?)) ...
           (or (list 'values results ...)
               (? adapter/c? results)))
     (define adapter-index-pairs
       (if (list? results)
           (for/list ([result (in-list results)]
                      [i   (in-naturals)]
                      #:when (adapter/c? result))
             (cons i result))
           (list (cons 0 results))))
     (delegating-> #f
                   adapter-index-pairs)]))


(define largest-possible-vector-size (expt 2 64))
(define (make-value-adapter base-type-diff)
  (match-define (difference original-type new-type) base-type-diff)
  (match base-type-diff
    [(difference 'Number 'Real) (transform/c real-part)]
    [(difference 'Real 'Number) (transform/c (λ (x) (+ x 0+1i)))]
    [(difference 'Real 'Integer) (transform/c round)]
    [(difference 'Integer 'Real) (transform/c (λ (x) (+ x 0.00001)))]
    [(or (difference 'Integer 'Natural)
         (difference 'Integer 'Index)
         (difference 'Exact-Rational 'Index)) (transform/c abs)]
    [(or (difference 'Natural 'Integer)
         (difference 'Index 'Integer)) (transform/c -)]
    [(or (difference 'Index 'Natural)
         (difference 'Index 'Exact-Rational)) (transform/c (const (add1 largest-possible-vector-size)))]
    [(difference 'Natural 'Index) (transform/c (λ (x)
                                                 (if (< x largest-possible-vector-size)
                                                     x
                                                     0)))]))

(struct adapter/c ())
(struct transform/c adapter/c (transformer)
  #:property prop:contract
  (build-contract-property
   #:name (const 'transform/c)
   #:late-neg-projection
   (λ (this)
     (define transform (transform/c-transformer this))
     (λ (blame)
       (λ (v neg-party)
         (transform v))))))
;; doesn't support keyword mutations
(struct delegating-> adapter/c (argument? index-ctc-pairs)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this)
            (cons (if (delegating->-argument? this)
                      'delegating->arg
                      'delegating->result)
                  (for/list ([{index ctc} (in-dict (delegating->-index-ctc-pairs this))])
                    (list index (contract-name ctc)))))
   #:late-neg-projection
   (λ (this)
     (define index-ctc-pairs (delegating->-index-ctc-pairs this))
     (define impersonator-wrapper
       (make-keyword-procedure
        (if (delegating->-argument? this)
            (λ (kws kw-args . args)
              (values kw-args
                      (apply-contracts-in-list args index-ctc-pairs)))
            (λ (kws kw-args . args)
              (values (λ results
                        (apply-contracts-in-list results index-ctc-pairs))
                      kw-args
                      args)))))
     (λ (blame)
       (λ (v neg-party)
         (impersonate-procedure v
                                impersonator-wrapper))))))

;; Returns `vs`, but with the elements at each index in `index-ctc-pairs`
;; contracted with the corresponding contract.
(define (apply-contracts-in-list vs index-ctc-pairs)
  (for/fold ([vs vs])
            ([{index ctc} (in-dict index-ctc-pairs)])
    (list-update vs
                 index
                 (λ (old-v)
                   (contract ctc old-v #f #f)))))

(module+ test
  (test-begin
    #:name value-adapter
    (test-equal? (contract-name
                  (generate-adapter-ctc (mutated-interface-type 'n
                                                                'Integer
                                                                'Real
                                                                type:base-type-substitution)))
                 'transform/c)
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type 'f
                                           '(-> Integer Integer)
                                           '(-> Real Integer)
                                           type:base-type-substitution)))
                 '(delegating->arg [0 transform/c]))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type 'f
                                           '(-> Integer Integer)
                                           '(-> Integer Real)
                                           type:base-type-substitution)))
                 '(delegating->result [0 transform/c]))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type 'f
                                           '(-> Integer (values Integer Real))
                                           '(-> Integer (values Integer Integer))
                                           type:base-type-substitution)))
                 '(delegating->result [1 transform/c]))))
