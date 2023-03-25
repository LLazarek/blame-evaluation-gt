#lang at-exp racket

(provide type:base-type-substitution
         type:complex-type->Any
         type:function-arg-swap
         type:function-result-swap
         type:struct-field-swap
         type:class-field-swap
         active-mutation-types ;; add a new type to this list when providing it!

         base-type->Any
         complex-type->Any
         function-arg-swap
         function-result-swap
         struct-field-swap
         class-field-swap
         ;; ll: write tests for the others before providing/using them!

         select-anything-but-specially-handle-get/setters-for-field-swaps)

(require mutate/define
         mutate/low-level
         syntax/parse
         syntax/parse/define)

;; todos:
;; - add union branch, always do False
;; - also perhaps we can make something where there's a mistake in the name of fields in a json object / hash table
;; - drop fields from a struct, then the adaptor produces False? <-- not well typed, but perhaps something similar
;;   ^ forgetting to document a field, or adding a field that isn't there

;; This is the set of base types that appear on the benchmark interfaces
#;'(Symbol
    Natural
    String
    Boolean
    Index
    Integer
    Real
    Exact-Rational
    Float
    Nonnegative-Integer
    Positive-Integer)
(define type:base-type-substitution "base-type->Any")
(define-id-mutator base-type->Any
  #:type type:base-type-substitution
  ;; Alternative, leveraging the complexity of the numeric tower types
  ;; we decided not to go this route to better match the simplicity of
  ;; popular languages' numeric types, contrast with Racket's.
  ;;
  ;; Pattern: generalize, then restrict
  ;; [Real #:-> Number]
  ;; [Real #:-> Integer]
  ;; [Real #:-> String]

  ;; [Integer #:-> Number]
  ;; [Integer #:-> Natural]
  ;; [Integer #:-> String]

  ;; [Natural #:-> Number]
  ;; [Natural #:-> One]
  ;; [Natural #:-> String]

  ;; [Nonnegative-Integer #:-> Number]
  ;; [Nonnegative-Integer #:-> Positive-Integer]
  ;; [Nonnegative-Integer #:-> String]

  ;; [Positive-Integer #:-> Number]
  ;; [Positive-Integer #:-> One]
  ;; [Positive-Integer #:-> String]

  ;; [Index #:-> Number]
  ;; [Index #:-> One]
  ;; [Index #:-> String]

  ;; [Exact-Rational #:-> Number]
  ;; [Exact-Rational #:-> Integer]
  ;; [Exact-Rational #:-> String]

  ;; [Float #:-> Number]
  ;; [Float #:-> Integer]
  ;; [Float #:-> String]

  ;; [Symbol #:-> String]
  ;; [String #:-> Symbol]
  ;; [Boolean #:-> Natural]


  [Real #:-> Any]
  [Integer #:-> Any]
  [Natural #:-> Any]
  [Nonnegative-Integer #:-> Any]
  [Positive-Integer #:-> Any]
  [Index #:-> Any]
  [Exact-Rational #:-> Any]
  [Float #:-> Any]

  [Symbol #:-> Any]
  [String #:-> Any]
  [Boolean #:-> Any]
  )

(define known-special-form?
  (syntax-parser
    [({~or {~datum struct} {~datum struct:}} _ ...) #t]
    [([field:id {~datum :} type ...] ...) #t]
    [({~or {~datum field} {~datum init-field}} _ ...) #t]
    [[field-name:id {~datum :} type ...] #t]
    [[method-name:id ({~or {~datum ->} {~datum ->*}} _ ...)] #t]
    [({~or {~datum values} {~datum Values}} _ ...) #t]
    [else #f]))
(define type:complex-type->Any "complex-type->Any")
(define-simple-mutator (complex-type->Any stx)
  #:type type:complex-type->Any
  #:pattern (_ ...)
  #:when (not (known-special-form? stx))
  #'Any)

(define (in-swaps l [swap-ok? (const #t)])
  (for*/stream ([i (in-range (length l))]
                [k (in-range (add1 i) (length l))]

                [I (in-value (list-ref l i))]
                [K (in-value (list-ref l k))]
                #:when (swap-ok? I K))
    (list-set (list-set l i K)
              k
              I)))
(module+ test
  (require ruinit
           mutate/tests/testing-util)

  (test-begin
    #:name in-swaps
    (test-equal? (stream->list (in-swaps '()))
                 '[])
    (test-equal? (stream->list (in-swaps '(1)))
                 '[])
    (test-equal? (stream->list (in-swaps '(1 2)))
                 '[(2 1)])
    (test-equal? (stream->list (in-swaps '(1 2 3)))
                 '[(2 1 3)
                   (3 2 1)
                   (1 3 2)])
    (test-equal? (stream->list (in-swaps '(1 2 3 4)))
                 '[(2 1 3 4)
                   (3 2 1 4)
                   (4 2 3 1)
                   (1 3 2 4)
                   (1 4 3 2)
                   (1 2 4 3)])
    (test-equal? (stream->list (in-swaps '(1 2 3 4)
                                         (λ (a b)
                                           (not (or (= a 2)
                                                    (= b 2))))))
                 '[(3 2 1 4)
                   (4 2 3 1)
                   (1 2 4 3)])))

(define-simple-macro (define-swapping-mutator name type
                       pattern
                       attribute-to-swap #:-> swapped-attribute-name
                       {~optional {~seq #:guard guard-f}}
                       re-pattern)
  (define name
    (make-stream-mutator
     #:type type
     (syntax-parser
       [pattern
        (for/stream ([rearranged-stxs (in-swaps (attribute attribute-to-swap)
                                                {~? guard-f})])
          (syntax-parse rearranged-stxs
            [[swapped-attribute-name (... ...)]
             (quasisyntax/loc this-syntax
               re-pattern)]))]
       [else empty-stream]))))


(define type:function-arg-swap "function-arg-swap")
(define-swapping-mutator ->-arg-swap type:function-arg-swap
  ({~and {~datum ->} head} e ... range)
  e #:-> new-e
  (head new-e ... range))
(define-swapping-mutator ->*-mandatory-arg-swap type:function-arg-swap
  ({~and {~datum ->*} head} (e ...) . rest)
  e #:-> new-e
  (head (new-e ...) . rest))
(define-swapping-mutator ->*-optional-arg-swap type:function-arg-swap
  ({~and {~datum ->*} head} mandatory (e ...) . rest)
  e #:-> new-e
  #:guard (λ left+right
            (not (ormap (compose1 keyword? syntax->datum) left+right)))
  (head mandatory (new-e ...) . rest))
(define function-arg-swap (mutator (compose-mutators ->-arg-swap
                                                     ->*-mandatory-arg-swap
                                                     ->*-optional-arg-swap)
                                   type:function-arg-swap))


(define type:function-result-swap "function-result-swap")
(define-swapping-mutator function-result-swap type:function-result-swap
  ({~and head {~or {~datum ->} {~datum ->*}}} arg ... ({~and {~datum values} values} e ...))
  e #:-> new-e
  (head arg ... (values new-e ...)))

(define type:struct-field-swap "struct-field-swap")
(define-swapping-mutator struct-field-swap type:struct-field-swap
  ({~and {~or* {~datum struct} {~datum struct:}} head}
      {~and name* {~or name:id (name:id parent:id)}}
      ([field-name:id {~datum :} field-t] ...)
      extras ...)
  field-t #:-> new-field-t
  (head name* ([field-name : new-field-t] ...) extras ...))

(define type:class-field-swap "class-field-swap")
(define-swapping-mutator class-field-swap type:class-field-swap
  ({~and {~or {~datum init-field}
              {~datum field}}
         field}
   [field-id:id field-type:expr]
   ...)
  field-type #:-> new-field-type
  #:guard (const (swapping-enabled-for-this-class?))
  (field [field-id new-field-type]
         ...))

(define-syntax-class get/setter-method
  #:commit
  (pattern name:id
           #:when (regexp-match? #rx"^(g|s)et-field:" (~a (syntax->datum #'name)))))
(define swapping-enabled-for-this-class? (make-parameter #t))
(define select-anything-but-specially-handle-get/setters-for-field-swaps
  (syntax-parser
    [[get/setter-method t]
     #:when (regexp-match? #rx"^(g|s)et-field:" (~a (syntax->datum #'get/setter-method)))
     #f]
    [({~datum Class} {~alt [g/s:get/setter-method _]
                           _}
                     ...)
     (define swapping-ok?
       (and (attribute g/s)
            (ormap values (attribute g/s))))
     (list this-syntax
           values
           `((,swapping-enabled-for-this-class? . ,swapping-ok?)))]
    [else
     (list this-syntax
           values
           empty)]))


(define active-mutation-types
  (list type:base-type-substitution
        type:complex-type->Any
        type:function-arg-swap
        type:function-result-swap
        type:struct-field-swap
        type:class-field-swap))


(module+ test
  (test-begin
    #:name class-field-swap
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* class-field-swap
                     #`(#,field-name
                        [a T1]
                        [b T2]
                        [c T3])
                     (list #`(#,field-name
                              [a T2]
                              [b T1]
                              [c T3])
                           #`(#,field-name
                              [a T3]
                              [b T2]
                              [c T1])
                           #`(#,field-name
                              [a T1]
                              [b T3]
                              [c T2])
                           #`(#,field-name
                              [a T1]
                              [b T2]
                              [c T3])))
      @~a{Field: @field-name}))))

(module+ test
  (test-begin
    #:name function-arg-swap
    (test-mutator* function-arg-swap
                   #'(-> A B C D)
                   (list #'(-> B A C D)
                         #'(-> C B A D)
                         #'(-> A C B D)

                         #'(-> A B C D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B C) D)
                   (list #'(->* (B A C) D)
                         #'(->* (C B A) D)
                         #'(->* (A C B) D)

                         #'(->* (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* () (A B C) D)
                   (list #'(->* () (B A C) D)
                         #'(->* () (C B A) D)
                         #'(->* () (A C B) D)

                         #'(->* () (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B) (A B C) D)
                   (list #'(->* (B A) (A B C) D)
                         #'(->* (A B) (B A C) D)
                         #'(->* (A B) (C B A) D)
                         #'(->* (A B) (A C B) D)

                         #'(->* (A B) (A B C) D)
                         ))
    (test-mutator* function-arg-swap
                   #'(->* (A B) (A B #:c C) D)
                   (list #'(->* (B A) {A B #:c C} D)
                         #'(->* (A B) (B A #:c C) D)
                         #'(->* (A B) (C B #:c A) D)
                         #'(->* (A B) (A C #:c B) D)

                         #'(->* (A B) (A B #:c C) D)
                         )))
  (test-begin
    #:name function-result-swap
    (test-mutator* function-result-swap
                   #'(-> A B C D)
                   (list #'(-> A B C D)))
    (test-mutator* function-result-swap
                   #'(-> X (values A B C D))
                   (list #'(-> X (values B A C D))
                         #'(-> X (values C B A D))
                         #'(-> X (values D B C A))
                         #'(-> X (values A C B D))
                         #'(-> X (values A D C B))
                         #'(-> X (values A B D C))

                         #'(-> X (values A B C D))
                         )))

  (test-begin
    #:name struct-field-swap
    (test-mutator* struct-field-swap
                   #'(struct: foo ([x : Number]
                                   [y : String]
                                   [z : Bar])
                       #:prefab
                       #:type-name Foo)
                   (list #'(struct: foo ([x : String]
                                         [y : Number]
                                         [z : Bar])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Bar]
                                         [y : String]
                                         [z : Number])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Number]
                                         [y : Bar]
                                         [z : String])
                             #:prefab
                             #:type-name Foo)
                         #'(struct: foo ([x : Number]
                                         [y : String]
                                         [z : Bar])
                             #:prefab
                             #:type-name Foo))))

  (test-begin
    #:name complex-type->Any
    (test-mutator* complex-type->Any
                   #'(-> A B C)
                   (list #'Any
                         #'(-> A B C)))
    (test-mutator* complex-type->Any
                   #'(Listof String)
                   (list #'Any
                         #'(Listof String)))
    (test-mutator* complex-type->Any
                   #'Natural
                   (list #'Natural))
    (test-mutator* complex-type->Any
                   #'(struct stream ([head : Number]
                                     [tail : stream]))
                   (list #'(struct stream ([head : Number]
                                           [tail : stream]))))))
