#lang at-exp racket

(provide type:base-type-substitution
         type:function-arg-swap
         type:function-result-swap
         type:struct-field-swap
         type:vector-arg-swap

         base-type-gen/restr
         function-arg-swap
         function-result-swap
         struct-field-swap
         vector-arg-swap
         ;; ll: write tests for the others before providing/using them!
         )

(require "logger.rkt"
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         syntax/parse)

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
(define type:base-type-substitution "known-type-generalization-restriction")
(define-id-mutator base-type-gen/restr
  #:type type:base-type-substitution
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

(define type:function-arg-swap "function-arg-swap")
(define-mutator (function-arg-swap stx mutation-index counter) #:type [type type:function-arg-swap]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum ->} head} e ... range)
     (define e-stxs (attribute e))
     (mdo* (def rearranged-e-stxs (rearrange-in-seq e-stxs
                                                    mutation-index
                                                    counter))
           [return
            (quasisyntax/loc stx
              (head #,@rearranged-e-stxs range))])]
    [else
     (no-mutation stx mutation-index counter)]))

(define type:function-result-swap "function-result-swap")
(define-mutator (function-result-swap stx mutation-index counter) #:type [type type:function-result-swap]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum ->} head} arg ... ({~and {~datum values} values} e ...))
     (define e-stxs (attribute e))
     (mdo* (def rearranged-e-stxs (rearrange-in-seq e-stxs
                                                    mutation-index
                                                    counter))
           [return
            (quasisyntax/loc stx
              (head arg ... (values #,@rearranged-e-stxs)))])]
    [else
     (no-mutation stx mutation-index counter)]))

(define type:vector-arg-swap "vector-arg-swap")
(define-mutator (vector-arg-swap stx mutation-index counter) #:type [type type:vector-arg-swap]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum Vector} head} e ...)
     (define e-stxs (attribute e))
     (mdo* (def rearranged-e-stxs (rearrange-in-seq e-stxs
                                                    mutation-index
                                                    counter))
           [return
            (quasisyntax/loc stx
              (head #,@rearranged-e-stxs))])]
    [else
     (no-mutation stx mutation-index counter)]))

(define type:struct-field-swap "struct-field-swap")
(define-mutator (struct-field-swap stx mutation-index counter) #:type [type type:struct-field-swap]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~or* {~datum struct} {~datum struct:}} head}
      {~and name* {~or name:id (name:id parent:id)}}
      ([field-name:id {~datum :} field-t] ...)
      extras ...)
     (define field-t-stxs (attribute field-t))
     (mdo* (def rearranged-field-stxs (rearrange-in-seq field-t-stxs
                                                        mutation-index
                                                        counter))
           [return
            (with-syntax ([[new-field-t ...] rearranged-field-stxs])
              (quasisyntax/loc stx
                (head name* ([field-name : new-field-t] ...) extras ...)))])]
    [else
     (no-mutation stx mutation-index counter)]))

(struct ll-this-is-definitely-unique:dropped ())
(define-simple-mutator (drop stx)
  #:pattern _
  #'ll-this-is-definitely-unique:dropped)
(define (dropped? x)
  (and (identifier? x)
       (free-identifier=? x #'ll-this-is-definitely-unique:dropped)))

(define-mutator (function-arg-drop stx mutation-index counter) #:type [type "drop-function-arg"]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum ->} head} e ... range)
     (define e-stxs (attribute e))
     (mdo* (def mutated-e-stxs (mutate-in-seq e-stxs
                                              mutation-index
                                              counter
                                              drop))
           [return
            (quasisyntax/loc stx
              (head #,@(filter-not dropped? mutated-e-stxs) range))])]
    [else
     (no-mutation stx mutation-index counter)]))

(define-mutator (function-result-drop stx mutation-index counter) #:type [type "drop-function-arg"]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum ->} head} arg ... ({~datum values} e ...))
     (define e-stxs (attribute e))
     (mdo* (def mutated-e-stxs (mutate-in-seq e-stxs
                                              mutation-index
                                              counter
                                              drop))
           [return
            (quasisyntax/loc stx
              (head arg ... (values #,@(filter-not dropped? mutated-e-stxs))))])]
    [else
     (no-mutation stx mutation-index counter)]))

(define-mutator (union-branch-drop stx mutation-index counter) #:type [type "drop-union-branch"]
  (log-mutation-type type)
  (syntax-parse stx
    [({~and {~datum U} head} e ...)
     (define e-stxs (attribute e))
     (mdo* (def mutated-e-stxs (mutate-in-seq e-stxs
                                              mutation-index
                                              counter
                                              drop))
           [return
            (quasisyntax/loc stx
              (head #,@(filter-not dropped? mutated-e-stxs)))])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (require ruinit
           "mutate-test-common.rkt")

  (test-begin
    #:name function-arg-swap
    (test-mutator* function-arg-swap
                   #'(-> A B C D)
                   (list #'(-> B A C D)
                         #'(-> A B C D)))
    (test-mutator* function-arg-swap
                   #'(-> A B C D E)
                   (list #'(-> B A C D E)
                         #'(-> A B D C E)
                         #'(-> A B C D E))))
  (test-begin
    #:name function-result-swap
    (test-mutator* function-result-swap
                   #'(-> A B C D)
                   (list #'(-> A B C D)))
    (test-mutator* function-result-swap
                   #'(-> A (values B C D))
                   (list #'(-> A (values C B D))
                         #'(-> A (values B C D))))
    (test-mutator* function-result-swap
                   #'(-> A (values B C D E))
                   (list #'(-> A (values C B D E))
                         #'(-> A (values B C E D))
                         #'(-> A (values B C D E)))))

  (test-begin
    #:name vector-arg-swap
    (test-mutator* vector-arg-swap
                   #'(Vector A B C D)
                   (list #'(Vector B A C D)
                         #'(Vector A B D C)
                         #'(Vector A B C D)))
    (test-mutator* vector-arg-swap
                   #'(Vector A B C D E)
                   (list #'(Vector B A C D E)
                         #'(Vector A B D C E)
                         #'(Vector A B C D E))))

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
                         #'(struct: foo ([x : Number]
                                         [y : String]
                                         [z : Bar])
                             #:prefab
                             #:type-name Foo)))
    (test-mutator* struct-field-swap
                   #'(struct foo ([x : Number]
                                  [y : String]
                                  [z : Bar]
                                  [e : Woozle]))
                   (list #'(struct foo ([x : String]
                                        [y : Number]
                                        [z : Bar]
                                        [e : Woozle]))
                         #'(struct foo ([x : Number]
                                        [y : String]
                                        [z : Woozle]
                                        [e : Bar]))
                         #'(struct foo ([x : Number]
                                        [y : String]
                                        [z : Bar]
                                        [e : Woozle]))))))
