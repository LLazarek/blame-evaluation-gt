#lang at-exp racket

(require "../mutate/type-api-mutators.rkt"
         syntax/parse/define
         (for-syntax syntax/parse))

(struct mutated-interface-type (name original mutated mutation-type)
  #:transparent)

(struct difference (left right) #:transparent)
(struct nothing () #:transparent)

;; A B -> (or/c A (difference A B))
(define (compare left right)
  (if (equal? left right)
      left
      (difference left right)))

(define (=length? a b)
  (implies (and (list? a) (list? b))
           (= (length a) (length b))))

(define-simple-macro (assert c
                             {~alt {~optional message #:defaults ([message #'""])}
                                   {~optional {~seq #:name name} #:defaults ([name #'#f])}}
                             ...)
  (unless c
    (error (or name 'unknown-function) message)))

;; Dumb diff of s1 and s2, assuming both have the same length or at most one
;; element missing from one and not the other.
;; sexp? sexp? -> sexp?
(define (sexp-diff s1 s2)
  (let loop ([result empty]
             [s1-remaining s1]
             [s2-remaining s2])
    (cond [(=length? s1-remaining s2-remaining)
           (match* {s1-remaining s2-remaining}
             [{(cons head-1 tail-1)
               (cons head-2 tail-2)}
              #:when (=length? head-1 head-2)
              (loop (cons (loop empty head-1 head-2)
                          result)
                    tail-1
                    tail-2)]
             [{(cons (? list? head-1) tail-1)
               (cons (? list? head-2) tail-2)}
              ;; different length heads. Something was dropped.
              (define-values {head-1* head-2*} (mark-drop head-1 head-2))
              (assert (=length? head-1* head-2*) #:name 'sexp-diff
                      "given two sub-sexps that have more than one difference in length")
              (loop result
                    (cons head-1* tail-1)
                    (cons head-2* tail-2))]
             [{'() '()}
              (reverse result)]
             [{atom1 atom2}
              (compare   atom1  atom2)])]
          [else
           (define-values {s1* s2*} (mark-drop s1-remaining s2-remaining))
           (assert (=length? s1* s2*) #:name 'sexp-diff
                   "given two sexps that have more than one difference in length")
           (loop result
                 s1*
                 s2*)])))

;; Given two lists that are exactly the same, but one has at most a single element dropped,
;; return copies of the lists such that the dropped element now has the placeholder
;; `(nothing)`.
(define (mark-drop l1 l2)
  (assert (let ([l1-len (length l1)]
                [l2-len (length l2)])
            (or (= l1-len        l2-len)
                (= l1-len        (sub1 l2-len))
                (= (sub1 l1-len) l2-len)))
          #:name 'mark-drop
          @~a{given two lists whose length difference is != 1:
                    @~s[l1]
                    @~s[l2]})
  (match* {l1 l2}
    [{'() '()} (values l1 l2)]
    [{(cons one l1-rest)
      (cons two l2-rest)}
     #:when (equal? one two)
     (define-values {l1* l2*} (mark-drop l1-rest l2-rest))
     (values (cons one l1*)
             (cons two l2*))]
    [{(cons one l1-rest)
                (? list?)}
     #:when (equal? l1-rest l2)
     (values l1
             (cons (nothing) l2))]
    [{          (? list?)
      (cons two l2-rest)}
     #:when (equal? l1 l2-rest)
     (values (cons (nothing) l1)
             l2)]))

(module+ test
  (require ruinit)

  (define-test-syntax (test-equal?/2 actual-e expected1 expected2)
    #'(let-values ([{v1 v2} actual-e])
        (and/test (test-equal? v1 expected1)
                  (test-equal? v2 expected2))))

  (test-begin
    #:name mark-drop
    (test-equal?/2 (mark-drop empty empty)
                   empty empty)
    (test-equal?/2 (mark-drop '(1) empty)
                   '(1) (list (nothing)))
    (test-equal?/2 (mark-drop '(1 2) '(1))
                   '(1 2) (list 1 (nothing)))
    (test-equal?/2 (mark-drop '(2 1) '(1))
                   '(2 1) (list (nothing) 1))
    (test-equal?/2 (mark-drop '(1 2 3) '(1 3))
                   '(1 2 3) (list 1 (nothing) 3))
    (test-equal?/2 (mark-drop '(1 3) '(1 2 3))
                   (list 1 (nothing) 3) '(1 2 3)))

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
                 `(1 2 (,(difference 3 4) ,(difference 4 3)) (5 (6))))
    (test-equal? (sexp-diff '((1 2 3))
                            '((1 3)))
                 `((1 ,(difference 2 (nothing)) 3)))
    (test-equal? (sexp-diff '(1 2 3)
                            '(1 3))
                 `(1 ,(difference 2 (nothing)) 3))))

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

(define-match-expander binding
  (syntax-parser
    [(_ pat {~seq #:with [name:id val:expr]} ...)
     #'(and pat
            (app (const val) name)
            ...)]))

;; type-diff
(struct td () #:transparent)
(struct td:-> td (argument? index-map) #:transparent)
(struct td:base td (original new) #:transparent)
(define (sexp->type-diff a-sexp-diff)
  (define (list->td-index-map a-list)
    (match a-list
      [(? td? single) `((0 . ,single))]
      [mixed-list
       (for/list ([maybe-td (in-list mixed-list)]
                  [index    (in-naturals)]
                  #:when (td? maybe-td))
         (cons index maybe-td))]))
  (match a-sexp-diff
    [(list* '-> (app (mapping sexp->type-diff)
                     (or (binding (list #f ...
                                        (or (? td? sub-tds)
                                            (list* 'values
                                                   (and (list _ ... (? td?) _ ...)
                                                        sub-tds))))
                                  #:with [arg? #f])
                         (binding (and (list _ ... (? td?) _ ... #f)
                                       sub-tds)
                                  #:with [arg? #t]))))
     (td:-> arg? (list->td-index-map sub-tds))]
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
                 (td:-> #t `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A Z Z Z C)
                                             '(-> B Z Z Z C)))
                 (td:-> #t `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z A Z C)
                                             '(-> Z Z B Z C)))
                 (td:-> #t `((2 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> X Z A Z C)
                                             '(-> Y Z B Z C)))
                 (td:-> #t `((0 . ,(td:base 'X 'Y))
                             (2 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z A)
                                             '(-> Z Z Z B)))
                 (td:-> #f `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values A))
                                             '(-> Z Z Z (values B))))
                 (td:-> #f `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A))
                                             '(-> Z Z Z (values Z B))))
                 (td:-> #f `((1 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A Z X))
                                             '(-> Z Z Z (values Z B Z Y))))
                 (td:-> #f `((1 . ,(td:base 'A 'B))
                             (3 . ,(td:base 'X 'Y)))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z (-> A C)))
                                             '(-> Z Z Z (values Z (-> B C)))))
                 (td:-> #f `((1 . ,(td:-> #t `((0 . ,(td:base 'A 'B))))))))))

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
  (define type-diff (sexp->type-diff (sexp-diff original mutated)))
  (define full-combinator
    (match mutation-type
      [(== type:base-type-substitution)
       (base-type-substitution-adapter type-diff)]
      [(== type:function-arg-swap)
       (function-arg-swap-adapter type-diff)]
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
      ))
  full-combinator)

(define (base-type-substitution-adapter type-diff)
  (match type-diff
    [(td:base original new)
     (make-value-adapter original new)]
    [(td:-> arg? index-map)
     (delegating-> arg?
                   (for/list ([{i td} (in-dict index-map)])
                     (cons i (base-type-substitution-adapter td))))]))

(define largest-possible-vector-size (expt 2 64))
(define (make-value-adapter original-type new-type)
  (match (difference original-type new-type)
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

(define (function-arg-swap-adapter type-diff)
  (match type-diff
    [(td:-> arg? `((,i1 ,(difference t1-orig t1-new))
                   (,i2 ,(difference t2-orig t2-new))))
     (assert (and (equal? t1-orig t2-new)
                  (equal? t2-orig t1-new))
             #:name 'function-arg-swap-adapter
             @~a{Mutation type is function arg swap but diff doesn't look like a swap:
                          @t1-orig -> @t1-new
                          @t2-orig -> @t2-new})
     (swap-> arg? i1 i2)]
    [(td:base original new)
     (error 'function-arg-swap-adapter
            "Should be impossible: got a base type diff?")]))

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

(struct swap-> adapter/c (argument? i1 i2)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this)
            (list (if (delegating->-argument? this)
                      'swap->arg
                      'swap->result)
                  (swap->-i1 this)
                  (swap->-i2 this)))
   #:late-neg-projection
   (λ (this)
     (define i1 (swap->-i1 this))
     (define i2 (swap->-i2 this))
     (define impersonator-wrapper
       (make-keyword-procedure
        (if (delegating->-argument? this)
            (λ (kws kw-args . args)
              (values kw-args
                      (swap-in-list args i1 i2)))
            (λ (kws kw-args . args)
              (values (λ results
                        (swap-in-list results i1 i2))
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

(define (swap-in-list vs index1 index2)
  (define v1 (list-ref vs index1))
  (define v2 (list-ref vs index2))
  (list-set (list-set vs index1 v2)
            index2
            v1))

(module+ test
  (define-test-syntax (test-adapter-contract
                       [name:id test-value:expr
                                #:with-contract ctc:expr]
                       check:expr)
    #'(let ([name (contract ctc test-value #f #f)])
        check))

  (test-begin
    #:name value-adapter
    (test-equal? (contract-name
                  (generate-adapter-ctc (mutated-interface-type 'n
                                                                'Real
                                                                'Integer
                                                                type:base-type-substitution)))
                 'transform/c)
    (test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'n
                                                                      'Real
                                                                      'Integer
                                                                      type:base-type-substitution))]
     (test-= v 5))
    (test-adapter-contract
     [f (λ (x) x)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type 'f
                                                 '(-> Real Integer)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-= (f 5.2) 5))
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
                                           '(-> Integer Real)
                                           '(-> Integer Integer)
                                           type:base-type-substitution)))
                 '(delegating->result [0 transform/c]))
    (test-adapter-contract
     [f (λ _ 5.2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type 'f
                                                 '(-> Integer Real)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-= (f 2) 5))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type 'f
                                           '(-> Integer (values Integer Real))
                                           '(-> Integer (values Integer Integer))
                                           type:base-type-substitution)))
                 '(delegating->result [1 transform/c]))
    (test-adapter-contract
     [f (λ _ (values 2 5.2))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type 'f
                                                 '(-> Integer (values Integer Real))
                                                 '(-> Integer (values Integer Integer))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f 0)])
       (test-= v2 5)))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type 'f
                                           '(-> String
                                                (-> Integer Real)
                                                (values Integer Real))
                                           '(-> String
                                                (-> Integer Integer)
                                                (values Integer Real))
                                           type:base-type-substitution)))
                 '(delegating->arg [1 (delegating->arg [0 transform/c])]))
    (test-adapter-contract
     [f (λ (s g) (values 2 (g 0)))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type 'f
                                                 '(-> String
                                                      (-> Integer Real)
                                                      (values Integer Real))
                                                 '(-> String
                                                      (-> Integer Integer)
                                                      (values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (test-= v2 5))))

  #;(test-begin
    #:name ))
