#lang at-exp racket

(provide type:base-type-substitution
         type:complex-type->Any
         type:function-arg-swap
         type:function-result-swap
         type:struct-field-swap

         base-type->Any
         complex-type->Any
         function-arg-swap
         function-result-swap
         struct-field-swap
         ;; ll: write tests for the others before providing/using them!
         )

(require "logger.rkt"
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
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

;; This will create a whole lotta garbage (syntax errors), but that's ok since
;; we weed those out easily and quickly in the first step of mutation analysis.
(define type:complex-type->Any "complex-type->Any")
(define-simple-mutator (complex-type->Any stx)
  #:type type:complex-type->Any
  #:pattern (_ ...)
  #'Any)

(define (in-swaps l)
  (for*/stream ([i (in-range (length l))]
                [k (in-range (add1 i) (length l))])
    (define I (list-ref l i))
    (define K (list-ref l k))
    (list-set (list-set l i K)
              k
              I)))
(module+ test
  (require ruinit
           "mutate-test-common.rkt")

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
                   (1 2 4 3)])))

(define-simple-macro (define-swapping-mutator name type
                       pattern
                       attribute-to-swap #:-> swapped-attribute-name
                       re-pattern)
  (define name
    (make-stream-mutator
     #:type type
     (syntax-parser
       [pattern
        (for/stream ([rearranged-stxs (in-swaps (attribute attribute-to-swap))])
          (syntax-parse rearranged-stxs
            [[swapped-attribute-name (... ...)]
             (quasisyntax/loc this-syntax
               re-pattern)]))]
       [else empty-stream]))))


(define type:function-arg-swap "function-arg-swap")
(define-swapping-mutator function-arg-swap type:function-arg-swap
  ({~and {~datum ->} head} e ... range)
  e #:-> new-e
  (head new-e ... range))

(define type:function-result-swap "function-result-swap")
(define-swapping-mutator function-result-swap type:function-result-swap
  ({~and {~datum ->} head} arg ... ({~and {~datum values} values} e ...))
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
       [field-id:id {~datum :} field-type:expr]
       ...)
  field-type #:-> new-field-type
  (field [field-id : new-field-type]
         ...))

(module+ test
  (test-begin
    #:name class-field-swap
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* class-field-swap
                     #`(#,field-name
                        [a : T1]
                        [b : T2]
                        [c : T3])
                     (list #`(#,field-name
                              [a : T2]
                              [b : T1]
                              [c : T3])
                           #`(#,field-name
                              [a : T3]
                              [b : T2]
                              [c : T1])
                           #`(#,field-name
                              [a : T1]
                              [b : T3]
                              [c : T2])
                           #`(#,field-name
                              [a : T1]
                              [b : T2]
                              [c : T3])))
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
                   (list #'Natural))))
