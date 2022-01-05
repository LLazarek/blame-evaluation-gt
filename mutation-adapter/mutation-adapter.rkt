#lang at-exp racket

(provide generate-adapter-ctc
         ->stx
         generate-delegating-adapter-ctc

         (struct-out mutated-interface-type)

         make-base-type-adapter
         delegating->
         swap->
         delegating-struct
         swap-struct-field

         sexp-diff)

(require "../mutate/type-api-mutators.rkt"
         "sexp-diff.rkt"
         "util.rkt"
         syntax/parse/define
         (for-syntax syntax/parse)
         racket/struct)

(struct mutated-interface-type (original mutated mutation-type)
  #:transparent)

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
(struct td:vector td (index-map) #:transparent)
(struct td:struct td (index-map) #:transparent)
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
    [(list* 'Vector (and (list _ ... (? td?) _ ... #f)
                         sub-tds))
     (td:vector (list->td-index-map sub-tds))]
    [(list '#:struct name (list [list fields ': (app sexp->type-diff sub-tds)] ...))
     #;(define (indexes->fields index-map)
       (for/list ([{i td} (in-dict index-map)])
         (cons (field (list-ref fields i) i) td)))
     (td:struct (list->td-index-map sub-tds))]
    [(difference original new)
     (td:base original new)]
    [(list* 'values (app (mapping sexp->type-diff)
                         result-tds))
     (and (ormap td? result-tds)
          (cons 'values result-tds))]
    [(? symbol?) #f]))

(module+ test
  (require ruinit)
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
  (match-define (mutated-interface-type original
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
       (function-arg/result-swap-adapter type-diff)]
      [(== type:function-result-swap)
       (function-arg/result-swap-adapter type-diff)]
      [(== type:struct-field-swap)
       (struct-field-swap-adapter type-diff)]
      #;[type:vector-arg-swap
       (vector-swap-adapter type-diff)]
      ;; [type:function-arg-drop
      ;;  (function-arg-drop-adapter annotated-diff)]
      ;; [type:function-result-drop
      ;;  (function-result-drop-adapter annotated-diff)]
      ;; [type:union-branch-drop
      ;;  (union-branch-drop-adapter annotated-diff)]
      ))
  full-combinator)

(struct recur () #:transparent)
;; td? (td? -> (or/c contract? recur?)) -> contract?
;; Converts `td` into a contract from the bottom up using `instantiator`,
;; which decides where the "bottom" of the tree is by producing the base contract,
;; or asks to continue unpacking the tree by producing `(recur)`.
;; All layers above the base contract will simply delegate down to the base.
(define (type-diff->contract td instantiator)
  (let loop ([td td])
    (match* {(instantiator td) td}
      [{(and (not (recur)) result) _} result]
      [{(recur) (and (td:base _ _) base)}
       (error 'type-diff->contract
              @~a{given instantiator @~e[instantiator] asked to recur into td:base @~e[base]})]
      [{(recur) (td:-> arg? index-map)}
       (delegating-> arg?
                     (for/list ([{i td} (in-dict index-map)])
                       (cons i (loop td))))]
      [{(recur) (td:struct index-map)}
       (delegating-struct (for/list ([{field td} (in-dict index-map)])
                            (cons field (loop td))))])))

;; sexp? symbol? contract? -> contract?
;; Generate a contract that delegates on `name` to `ctc`.
(define (generate-delegating-adapter-ctc type name ctc)
  (define td (sexp->type-diff (sexp-diff type (replace-name-in-sexp type name (gensym name)))))
  (type-diff->contract td
                       (match-lambda [(td:base _ _) ctc]
                                     [else (recur)])))

(define (base-type-substitution-adapter type-diff)
  (type-diff->contract type-diff
                       (match-lambda [(td:base original new)
                                      (make-base-type-adapter original new)]
                                     [else (recur)])))

(define largest-possible-vector-size (expt 2 64))
(define round->exact (compose1 inexact->exact round))
(define realize-delta 0.00001)
(define (make-base-type-adapter original-type new-type)
  (let ([transform/c (λ (f) (transform/c f original-type new-type))])
    (match (difference original-type new-type)
      [(difference 'Number 'Real) (transform/c real-part)]
      [(difference 'Real 'Number) (transform/c (λ (x) (+ x 0+1i)))]
      [(difference 'Real 'Integer) (transform/c round->exact)]
      [(difference 'Integer 'Real) (transform/c (λ (x) (+ x realize-delta)))]
      [(or (difference 'Integer 'Natural)
           (difference 'Integer 'Index)) (transform/c abs)]
      [(difference 'Exact-Rational 'Index)
       (transform/c (compose1 abs round->exact))]
      [(or (difference 'Natural 'Integer)
           (difference 'Index 'Integer)) (transform/c -)]
      [(or (difference 'Index 'Natural)
           (difference 'Index 'Exact-Rational)) (transform/c (const (add1 largest-possible-vector-size)))]
      [(difference 'Natural 'Index) (transform/c (λ (x)
                                                   (if (< x largest-possible-vector-size)
                                                       x
                                                       0)))])))

(define (function-arg/result-swap-adapter type-diff)
  (type-diff->contract
   type-diff
   (match-lambda [(td:-> arg? `((,i1 . ,(td:base t1-orig t1-new))
                                (,i2 . ,(td:base t2-orig t2-new))))
                  (assert (and (equal? t1-orig t2-new)
                               (equal? t2-orig t1-new))
                          #:name 'function-arg-swap-adapter
                          @~a{Mutation type is function arg swap but diff doesn't look like a swap:
                                       @t1-orig -> @t1-new
                                       @t2-orig -> @t2-new})
                  (swap-> arg? i1 i2)]
                 [(td:base original new)
                  (error 'function-arg-swap-adapter
                         "Should be impossible: got a base type diff?")]
                 [else (recur)])))

(define (struct-field-swap-adapter type-diff)
  (match type-diff
    [(td:struct `((,i1 . ,(td:base t1-orig t1-new))
                   (,i2 . ,(td:base t2-orig t2-new))))
     (assert (and (equal? t1-orig t2-new)
                  (equal? t2-orig t1-new))
             #:name 'function-arg-swap-adapter
             @~a{Mutation type is struct field swap but diff doesn't look like a swap:
                          @t1-orig -> @t1-new
                          @t2-orig -> @t2-new})
     (swap-struct-field i1 i2)]
    [other
     (error 'struct-field-swap-adapter
            @~a{Should be impossible: got a td that isn't td:struct?: @~e[other]})]))


#;(define (vector-swap-adapter type-diff)
  (match type-diff
    [(td:vector `((,i1 . ,(td:base t1-orig t1-new))
                  (,i2 . ,(td:base t2-orig t2-new))))
     (assert (and (equal? t1-orig t2-new)
                  (equal? t2-orig t1-new))
             #:name 'vector-swap-adapter
             @~a{Mutation type is vector arg swap but diff doesn't look like a swap:
                          @t1-orig -> @t1-new
                          @t2-orig -> @t2-new})
     (swap-vector i1 i2)]
    [(td:base original new)
     (error 'vector-swap-adapter
            "Should be impossible: got a base type diff?")]))

(require racket/generic)
(define-generics adapted
  (->stx adapted))

(struct adapter/c ())
(struct transform/c adapter/c (transformer from-type to-type)
  #:property prop:contract
  (build-contract-property
   #:name (const 'transform/c)
   #:late-neg-projection
   (λ (this)
     (define transform (transform/c-transformer this))
     (λ (blame)
       (λ (v neg-party)
         (transform v)))))
  #:methods gen:adapted
  [(define (->stx this)
     #`(make-base-type-adapter '#,(transform/c-from-type this)
                           '#,(transform/c-to-type this)))])

;; This weird dance with this macro wrapping the adapt-arguments? expr in a thunk is only there to delay the reference to struct accessor ids until they are defined.
;; IOW those references must happen under the lambda of the projection, otherwise they're unbound.
(define-simple-macro (simple->adapter-projection make-arg/result-adapter:expr
                                                 adapt-arguments?:expr)
  (simple->adapter-projection-helper make-arg/result-adapter
                                     (λ _ adapt-arguments?)))
(define (simple->adapter-projection-helper make-arg/result-adapter
                                           get-adapt-arguments?)
  (λ (this)
    (define adapt-arguments? (get-adapt-arguments?))
    (define arg/result-adapter (make-arg/result-adapter this))
    (define impersonator-wrapper
      (make-keyword-procedure
       (if (adapt-arguments? this)
           (λ (kws kw-args . args)
             (define new-args (arg/result-adapter args))
             (apply values
                    (if (empty? kw-args)
                        new-args
                        (list kw-args new-args))))
           (λ (kws kw-args . args)
             (apply values
                    (λ results
                      (apply values
                             (arg/result-adapter results)))
                    (if (empty? kw-args)
                        args
                        (list kw-args args)))))))
    (λ (blame)
      (λ (v neg-party)
        (impersonate-procedure v
                               impersonator-wrapper)))))

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
   (simple->adapter-projection (λ (this)
                                 (define index-ctc-pairs (delegating->-index-ctc-pairs this))
                                 (λ (args/results) (apply-contracts-in-list args/results index-ctc-pairs)))
                               delegating->-argument?))
  #:methods gen:adapted
  [(define/generic generic->stx ->stx)
   (define (->stx this)
     #`(delegating-> #,(delegating->-argument? this)
                     (list #,@(for/list ([{i ctc} (in-dict (delegating->-index-ctc-pairs this))])
                                #`(cons #,i #,(generic->stx ctc))))))])

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
   (simple->adapter-projection (λ (this)
                                 (define i1 (swap->-i1 this))
                                 (define i2 (swap->-i2 this))
                                 (λ (args/results) (swap-in-list args/results i1 i2)))
                               swap->-argument?))
  #:methods gen:adapted
  [(define (->stx this)
     #`(swap-> #,(swap->-argument? this) #,(swap->-i1 this) #,(swap->-i2 this)))])

(define (transform-struct-fields v transform)
  ;; lltodo: this is a mess and wrong.

  ;; problem getting struct type info
  ;; Let's assume that all structs must be prefab and mutable.
  ;; ll: this actually can't be done with `impersonate-struct`, even with those assumptions.
  ;; Because the redirector proc can't know the index that the original accessor proc got.
  ;; and the accessor can't be wrapped in a function that communicates that info to the redirector, due to the ctc of `impersonate-struct`.
  (define fields (struct->list v))
  (define key (prefab-struct-key v))
  (define struct-type (prefab-key->struct-type key (length fields)))
  (define-values {name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?}
    (struct-type-info struct-type))
  #;(impersonate-struct v
                        accessor-proc
                        ???)
  ;; so all we can do is make a new one. This is *wrong!* if anything uses eq?, but maybe none of the benchmarks check struct eq?
  ;; at least the simple ones don't.
  ;; this approach at least doesn't demand structs to be mutable.
  (apply (struct-type-make-constructor struct-type)
         (transform fields)))

(struct delegating-struct adapter/c (index-ctc-pairs)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this) (list 'delegating-struct
                          (for/list ([{field ctc} (in-dict (delegating-struct-index-ctc-pairs this))])
                            (cons field (contract-name ctc)))))
   #:late-neg-projection
   (λ (this)
     (define index-map (delegating-struct-index-ctc-pairs this))
     (λ (blame)
       (λ (v neg-party)
         (transform-struct-fields v
                                  (λ (fields)
                                    (apply-contracts-in-list fields
                                                             index-map)))))))
  #:methods gen:adapted
  [(define/generic generic->stx ->stx)
   (define (->stx this)
     #`(delegating-struct
        (list #,@(for/list ([{f ctc} (in-dict (delegating-struct-index-ctc-pairs this))])
                   #`(cons #,f #,(generic->stx ctc))))))])

(struct swap-struct-field adapter/c (i1 i2)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this) (list 'delegating-struct
                          (for/list ([{field ctc} (in-dict (delegating-struct-index-ctc-pairs this))])
                            (cons field (contract-name ctc)))))
   #:late-neg-projection
   (λ (this)
     (define i1 (swap-struct-field-i1 this))
     (define i2 (swap-struct-field-i2 this))
     (λ (blame)
       (λ (v neg-party)
         (transform-struct-fields v (λ (fields) (swap-in-list fields i1 i2)))))))
  #:methods gen:adapted
  [(define (->stx this)
     #`(swap-struct-field #,(swap-struct-field-i1 this) #,(swap-struct-field-i2 this)))])

#;(struct swap-vector adapter/c (i1 i2)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this) (list 'vector-swap (swap-vector-i1 this) (swap-vector-i2 this)))
   #:late-neg-projection
   (λ (this)
     (define i1 (swap-vector-i1 this))
     (define i2 (swap-vector-i2 this))
     (define impersonator-procedure
       (λ _ (error)))
     (λ (blame)
       (λ (v neg-party)
         (impersonate-vector v
                             impersonator-procedure))))))

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
                  (generate-adapter-ctc (mutated-interface-type 'Real
                                                                'Integer
                                                                type:base-type-substitution)))
                 'transform/c)
    (test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Real
                                                                      'Integer
                                                                      type:base-type-substitution))]
     (test-equal? v 5))
    (test-adapter-contract
     [v 5/2
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Exact-Rational
                                                                      'Index
                                                                      type:base-type-substitution))]
     (test-equal? v (round 5/2)))
    (test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Integer
                                                                      'Real
                                                                      type:base-type-substitution))]
     (and/test (not/test (test-equal? v 5))
               (test-within v 5 realize-delta))))

  (test-begin
   #:name delegating->
    (test-adapter-contract
     [f (λ (x) x)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Real Integer)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-equal? (f 5.2) 5))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer Integer)
                                           '(-> Real Integer)
                                           type:base-type-substitution)))
                 '(delegating->arg [0 transform/c]))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer Real)
                                           '(-> Integer Integer)
                                           type:base-type-substitution)))
                 '(delegating->result [0 transform/c]))
    (test-adapter-contract
     [f (λ _ 5.2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer Real)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-equal? (f 2) 5))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer (values Integer Real))
                                           '(-> Integer (values Integer Integer))
                                           type:base-type-substitution)))
                 '(delegating->result [1 transform/c]))
    (test-adapter-contract
     [f (λ _ (values 2 5.2))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer (values Integer Real))
                                                 '(-> Integer (values Integer Integer))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f 0)])
       (test-equal? v2 5)))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> String
                                                (-> Integer Real)
                                                (values Integer Real))
                                           '(-> String
                                                (-> Integer Integer)
                                                (values Integer Real))
                                           type:base-type-substitution)))
                 '(delegating->arg [1 (delegating->result [0 transform/c])]))
    (test-adapter-contract
     [f (λ (s g) (values 2 (g 0)))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> String
                                                      (-> Integer Real)
                                                      (values Integer Real))
                                                 '(-> String
                                                      (-> Integer Integer)
                                                      (values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (test-equal? v2 5))))

  (test-begin
    #:name swap->
    (test-adapter-contract
     [f (λ (a b) (values a b))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer
                                                      String
                                                      (values Integer String))
                                                 '(-> String
                                                      Integer
                                                      (values Integer String))
                                                 type:function-arg-swap))]
     (let-values ([{v1 v2} (f 1 "two")])
       (and/test (test-equal? v1 "two")
                 (test-equal? v2 1))))
    (test-adapter-contract
     [f (λ (a b) (values a b))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer
                                                      String
                                                      (values Integer String))
                                                 '(-> Integer
                                                      String
                                                      (values String Integer))
                                                 type:function-result-swap))]
     (let-values ([{v1 v2} (f 1 "two")])
       (and/test (test-equal? v1 "two")
                 (test-equal? v2 1)))))

  (test-begin
   #:name delegating-struct
   (ignore (struct temp (x y z) #:prefab))
   (test-adapter-contract
     [t (temp 5.5 "hello" 2.3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '[#:struct temp ([x : Real] [y : String] [z : Real])]
                                                 '[#:struct temp ([x : Integer] [y : String] [z : Real])]
                                                 type:base-type-substitution))]
     (and/test (temp? t)
               (test-equal? (temp-x t) (round->exact 5.5))
               (test-equal? (temp-y t) "hello")
               (test-equal? (temp-z t) 2.3))))
  (test-begin
   #:name swap-struct-fields
   (ignore (struct temp (x y z) #:prefab))
   (test-adapter-contract
     [t (temp 5.5 "hello" 2.3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '[#:struct temp ([x : Integer] [y : String] [z : Real])]
                                                 '[#:struct temp ([y : String] [x : Integer] [z : Real])]
                                                 type:struct-field-swap))]
     (and/test (temp? t)
               (test-equal? (temp-y t) 5.5)
               (test-equal? (temp-x t) "hello")
               (test-equal? (temp-z t) 2.3))))
  )

