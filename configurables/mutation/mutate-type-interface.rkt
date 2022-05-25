#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names

         count-type-mutations)

(require racket/function
         syntax/parse
         racket/format
         racket/match
         "../../mutate/type-api-mutators.rkt"
         "../../mutate/mutate-expr.rkt"
         "../../mutate/mutate-program.rkt"
         "../../mutate/mutator-lib.rkt")

(define active-mutators (list base-type-gen/restr
                              function-arg-swap
                              function-result-swap
                              struct-field-swap
                              ;; function-arg-drop
                              ;; function-result-drop
                              ;; union-branch-drop
                              vector-arg-swap
                              ))
(define active-mutator-names (map mutator-type active-mutators))

(define mutate-type-expr (make-expr-mutator (apply compose-mutators active-mutators)))

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
       (t+r this-syntax identity)]
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
                 (d-t name #,@new-type))))]
    [_ (values #f #f #f)]))

(define mutate-type-interface (make-program-mutator mutate-type-expr select-interface-types))
(define (mutate-benchmark module-body-stxs mutation-index #:program [_ignored #f])
  (mutate-type-interface module-body-stxs mutation-index))


(define (count-type-mutations a-type)
  (define (mutate-type a-type index)
    (mutate-type-interface (datum->syntax #f (list a-type)) index))

  (let next ([i 0])
    (with-handlers ([mutation-index-exception? (λ _ i)])
      (mutate-type a-type i)
      (next (add1 i)))))

(module+ test
  (require ruinit
           "../../mutate/mutate-test-common.rkt")
  (define-test (test-mutation index orig-prog new-prog
                              [mutate-syntax (syntax-only mutate-benchmark)])
    (with-handlers ([mutation-index-exception?
                     (λ _
                       (fail "Mutation index exceeded"))])
      (test-programs-equal?
       (mutate-syntax orig-prog index)
       new-prog)))
  (define-test (test-mutation/sequence orig-program expects
                                       [mutate-syntax (syntax-only mutate-benchmark)])
    (for/and/test ([expect (in-list expects)])
                  (match-define (list mutation-index expected) expect)
                  (define result (test-mutation mutation-index
                                                orig-program
                                                expected
                                                mutate-syntax))
                  (extend-test-message result
                                       " (mutation index: ~v)"
                                       mutation-index)))
  (test-begin
    #:name mutate-type-expr
    (test-mutator* mutate-type-expr
                   #'[#:struct stream ([first : Natural] [rest : (-> stream)])]
                   (list #'[#:struct stream ([first : (-> stream)] [rest : Natural])]
                         #'[#:struct stream ([first : Integer] [rest : (-> stream)])]
                         #'[#:struct stream ([first : Index] [rest : (-> stream)])]
                         #'[#:struct stream ([first : Natural] [rest : (-> stream)])])))

  (test-begin
    #:name mutate-benchmark
    (test-mutation/sequence
     #'[(require "../../../utilities/require-typed-check-provide.rkt")

        (require/typed/check/provide "streams.rkt"
                                     [#:struct stream ([first : Natural]
                                                       [rest : (-> stream)])]
                                     [make-stream (-> Natural (-> stream) stream)]
                                     [stream-unfold (-> stream (values Natural stream))]
                                     [stream-get (-> stream Natural Natural)]
                                     [stream-take (-> stream Natural (Listof Natural))])
        ]
     `(#;[0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

                (require/typed/check/provide "streams.rkt"
                                             [#:struct stream ([first : Natural]
                                                               [rest : (-> stream)])]
                                             [make-stream (-> Natural (-> stream) stream)]
                                             [stream-unfold (-> stream (values Natural stream))]
                                             [stream-get (-> stream Natural Natural)]
                                             [stream-take (-> stream Natural (Listof Natural))])
                ]]
       [0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : (-> stream)]
                                                             [rest : Natural])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [1 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Integer]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [3 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> (-> stream) Natural stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [4 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Integer (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [6 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values stream Natural))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [7 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Integer stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [9 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> Natural stream Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [10 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Integer Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [12 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Integer)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [14 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [#:struct stream ([first : Natural]
                                                             [rest : (-> stream)])]
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> Natural stream (Listof Natural))])
              ]]
       [15 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (require/typed/check/provide "streams.rkt"
                                            [#:struct stream ([first : Natural]
                                                              [rest : (-> stream)])]
                                            [make-stream (-> Natural (-> stream) stream)]
                                            [stream-unfold (-> stream (values Natural stream))]
                                            [stream-get (-> stream Natural Natural)]
                                            [stream-take (-> stream Integer (Listof Natural))])
               ]]
       [17 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (require/typed/check/provide "streams.rkt"
                                            [#:struct stream ([first : Natural]
                                                              [rest : (-> stream)])]
                                            [make-stream (-> Natural (-> stream) stream)]
                                            [stream-unfold (-> stream (values Natural stream))]
                                            [stream-get (-> stream Natural Natural)]
                                            [stream-take (-> stream Natural (Listof Integer))])
               ]]))

    (test-mutation/sequence
     #'[(require "../../../utilities/require-typed-check-provide.rkt")

        (define-type BoxQuad (List* 'box QuadAttrs QuadList))
        (define-type RunQuad (List* 'run QuadAttrs QuadList))
        (require/typed/check/provide "streams.rkt"
                                     [make-stream (-> Natural (-> stream) stream)])
        ]
     `(#;[0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (define-type BoxQuad (List* 'box QuadAttrs QuadList))
              (define-type RunQuad (List* 'run QuadAttrs QuadList))
              ]]
       [0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (define-type BoxQuad (List* 'box QuadAttrs QuadList))
              (define-type RunQuad (List* 'run QuadAttrs QuadList))
              (require/typed/check/provide "streams.rkt"
                                            [make-stream (-> (-> stream) Natural stream)])
              ]]))))
