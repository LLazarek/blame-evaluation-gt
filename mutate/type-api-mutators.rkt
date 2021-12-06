#lang at-exp racket

(provide count-type-mutations)

(require "logger.rkt"
         "mutate-expr.rkt"
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "mutate-program.rkt"
         syntax/parse)

;; todos:
;; - add union branch, always do False
;; - also perhaps we can make something where there's a mistake in the name of fields in a json object / hash table
;; - drop fields from a struct, then the adaptor produces False? <-- not well typed, but perhaps something similar
;;   ^ forgetting to document a field, or adding a field that isn't there

(define-id-mutator base-type-gen/restr
  #:type "known-type-generalization-restriction"
  [Number #:<-> Real]
  [Real #:<-> Integer]
  [Real #:<-> Complex]
  [Integer #:<-> Natural]
  [Index #:<-> Integer]
  [Index #:<-> Natural]
  [Index #:<-> Exact-Rational])

(define-mutator (function-arg-swap stx mutation-index counter) #:type [type "function-arg-swap"]
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

(define-mutator (vector-arg-swap stx mutation-index counter) #:type [type "vector-arg-swap"]
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

(struct ll-this-is-definitely-unique:dropped ())
(define-simple-mutator (drop stx)
  #:pattern _
  #'ll-this-is-definitely-unique:dropped)
(define (dropped? x)
  (and (identifier? x)
       (free-identifier=? x #'ll-this-is-definitely-unique:dropped)))

(define-mutator (drop-function-arg stx mutation-index counter) #:type [type "drop-function-arg"]
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

(define-mutator (drop-union-branch stx mutation-index counter) #:type [type "drop-union-branch"]
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


(define mutate-type-expr (make-expr-mutator (compose-mutators base-type-gen/restr
                                                              function-arg-swap
                                                              drop-function-arg
                                                              drop-union-branch
                                                              vector-arg-swap)))

(struct t+r (type reconstructor))
(define (parse-name+types name+type-pairs)
  (for/list ([pair (in-list name+type-pairs)])
    (syntax-parse pair
      [[name:id type]
       (t+r (attribute type)
            (λ (new-type)
              (quasisyntax/loc this-syntax
                [name #,new-type])))]
      [[#:opaque name:id pred]
       (t+r #'() (const pair))]
      [[#:struct struct-name:id ([field-name:id {~datum :} field-type] ...)]
       (t+r (attribute field-type)
            (λ (new-field-types)
              (with-syntax ([[new-field-type ...] new-field-types])
                (syntax/loc this-syntax
                  [#:struct struct-name ([field-name : new-field-type] ...)]))))]
      [other
       (raise-user-error 'parse-name+types
                         @~a{Missing handler for r/t/c /p case: @~s[(syntax->datum #'other)]})])))

(define mutate-type/list
  (make-program-mutator
   mutate-type-expr
   (syntax-parser
     [({~and {~datum require/typed/check/provide}
             r/t/c/p}
       mod-path
       name+type ...)
      (match-define (list (t+r types name+type-reconstructors) ...)
        (parse-name+types (attribute name+type)))
      (values types
              (syntax->datum #'mod-path)
              (λ (new-types)
                (quasisyntax/loc this-syntax
                  (r/t/c/p mod-path
                           #,@(map (λ (f stx) (f stx))
                                   name+type-reconstructors
                                   new-types)))))]
     [({~and {~datum define-type} d-t} name:id type)
      (values (list (attribute type))
              (syntax->datum #'name)
              (λ (new-type)
                (quasisyntax/loc this-syntax
                  (d-t name #,new-type))))])))

(define (mutate-type a-type index)
  (mutate-type/list (datum->syntax #f (list a-type)) index))
(define (count-type-mutations a-type)
  (let next ([i 0])
    (with-handlers ([mutation-index-exception? (λ _ i)])
      (mutate-type a-type i)
      (next (add1 i)))))
