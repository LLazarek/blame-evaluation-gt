#lang at-exp racket/base

(provide mutate-type-interface
         count-type-mutations)

(require racket/function
         syntax/parse
         racket/format
         racket/match
         "../../mutate/type-api-mutators.rkt"
         "../../mutate/mutate-expr.rkt"
         "../../mutate/mutate-program.rkt"
         "../../mutate/mutator-lib.rkt")

(define mutate-type-expr (make-expr-mutator (compose-mutators base-type-gen/restr
                                                              function-arg-swap
                                                              function-result-swap
                                                              struct-field-swap
                                                              ;; function-arg-drop
                                                              ;; function-result-drop
                                                              ;; union-branch-drop
                                                              vector-arg-swap
                                                              )))

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

(define select-interface-types
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
                 (d-t name #,new-type))))]))

(define mutate-type-interface (make-program-mutator mutate-type-expr select-interface-types))


(define (count-type-mutations a-type)
  (define (mutate-type a-type index)
    (mutate-type-interface (datum->syntax #f (list a-type)) index))

  (let next ([i 0])
    (with-handlers ([mutation-index-exception? (λ _ i)])
      (mutate-type a-type i)
      (next (add1 i)))))
