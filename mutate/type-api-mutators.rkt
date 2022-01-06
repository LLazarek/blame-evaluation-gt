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
         vector-arg-swap)

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

(define type:base-type-substitution "known-type-generalization-restriction")
(define-id-mutator base-type-gen/restr
  #:type type:base-type-substitution
  [Number #:<-> Real]
  [Real #:<-> Integer]
  [Integer #:<-> Natural]
  [Index #:<-> Integer]
  [Index #:<-> Natural]
  [Index #:<-> Exact-Rational])

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
    [({~and {~datum ->} head} arg ... (values e ...))
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
    [({~and #:struct head} name:id ... (field-spec ...))
     (define field-stxs (attribute field-spec))
     (mdo* (def rearranged-field-stxs (rearrange-in-seq field-stxs
                                                    mutation-index
                                                    counter))
           [return
            (quasisyntax/loc stx
              (head name ... (#,@rearranged-field-stxs)))])]
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

;; lltodo: tests for mutators

