#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names

         count-type-mutations)

(require racket/function
         syntax/parse
         racket/format
         racket/match
         racket/list
         mutate/define
         mutate/low-level
         mutate/traversal
         "type-api-mutators.rkt")

(define active-mutators (list base-type->Any
                              complex-type->Any
                              function-arg-swap
                              function-result-swap
                              struct-field-swap
                              class-field-swap))
(define active-mutator-names (map mutator-type active-mutators))

(define mutate-type-expr (make-expr-mutator (apply compose-mutators active-mutators)
                                            #:select select-anything-but-specially-handle-get/setters-for-field-swaps))

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
      [[#:struct {~or* struct-name:id (struct-name:id _:id)} ([field-name:id {~datum :} field-type] ...)]
       ;; See related comment in generate-adapters.rkt::r/t/p-entry->name+type
       #;(raise-user-error 'parse-name+types
                         @~a{#:struct clauses in require/typed/check are not supported, use prefab structs instead})
       (t+r #'() (const pair))]
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
     (list types
           (syntax->datum #'mod-path)
           (λ (new-types)
             (quasisyntax/loc this-syntax
               (r/t/c/p mod-path
                        #,@(map (λ (f stx) (f stx))
                                name+type-reconstructors
                                new-types)))))]
    [({~and {~datum define-type} d-t} name:id type)
     (list (list (attribute type))
           (syntax->datum #'name)
           (λ (new-type)
             (quasisyntax/loc this-syntax
               (d-t name #,@new-type))))]
    [({~and {~or* {~datum struct} {~datum struct:}} struct} {~or name:id (name:id parent:id)} ([field-name:id {~datum :} field-type] ...)
                                                            #:prefab
                                                            {~optional {~seq #:type-name type-name:id}})
     (list (list this-syntax)
           (syntax->datum #'name)
           first)]
    [_ #f]))

(define mutate-type-interface (make-program-mutator mutate-type-expr #:select select-interface-types))
(define (mutate-benchmark module-body-stxs
                          mutation-index
                          #:program [_ignored #f]
                          #:transform [transformer identity])
  ((transformer mutate-type-interface) module-body-stxs mutation-index))


(define (count-type-mutations a-type)
  (define (mutate-type a-type index)
    (mutate-type-interface (datum->syntax #f (list a-type)) index))

  (let next ([i 0])
    (if (equal? (mutate-type a-type i) no-more-mutations-flag)
        i
        (next (add1 i)))))

(module+ test
  (require ruinit
           mutate/tests/testing-util)
  (define mutate-syntax (curry mutate-benchmark #:transform syntax-only))
  (define-test (test-mutation index orig-prog new-prog)
    (define mutated (mutate-syntax orig-prog index))
    (if (equal? mutated no-more-mutations-flag)
        (fail "Mutation index exceeded")
        (test-programs-equal?
         mutated
         new-prog)))
  (define-test (test-mutation/sequence orig-program expects)
    (for/and/test ([expect (in-list expects)])
                  (match-define (list mutation-index expected) expect)
                  (define result (test-mutation mutation-index
                                                orig-program
                                                expected))
                  (extend-test-message result
                                       " (mutation index: ~v)"
                                       mutation-index)))
  (test-begin
    #:name mutate-type-expr
    (test-mutator* mutate-type-expr
                   #'(struct stream ([first : Natural] [rest : (-> stream)]) #:prefab)
                   (list #'(struct stream ([first : (-> stream)] [rest : Natural]) #:prefab)
                         #'(struct stream ([first : Any] [rest : (-> stream)]) #:prefab)
                         #'(struct stream ([first : Natural] [rest : Any]) #:prefab)
                         #'(struct stream ([first : Natural] [rest : (-> stream)]) #:prefab))))

  (test-begin
    #:name mutate-benchmark
    (test-mutation/sequence
     #'[(require "../../../utilities/require-typed-check-provide.rkt")

        (struct stream ([first : Natural]
                        [rest : (-> stream)])
          #:prefab)
        (require/typed/check/provide "streams.rkt"
                                     [make-stream (-> Natural (-> stream) stream)]
                                     [stream-unfold (-> stream (values Natural stream))]
                                     [stream-get (-> stream Natural Natural)]
                                     [stream-take (-> stream Natural (Listof Natural))])
        ]
     `(#;[0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

                (struct stream ([first : Natural]
                                [rest : (-> stream)])
                  #:prefab)
                (require/typed/check/provide "streams.rkt"
                                             [make-stream (-> Natural (-> stream) stream)]
                                             [stream-unfold (-> stream (values Natural stream))]
                                             [stream-get (-> stream Natural Natural)]
                                             [stream-take (-> stream Natural (Listof Natural))])
                ]]
       [0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : (-> stream)]
                              [rest : Natural])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [1 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Any] [rest : (-> stream)]) #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [2 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural] [rest : Any]) #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [3 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream Any]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [4 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> (-> stream) Natural stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [5 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Any (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [6 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural Any stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [7 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold Any]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [8 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values stream Natural))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [9 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Any stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [10 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get Any]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [11 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (struct stream ([first : Natural]
                              [rest : (-> stream)])
                #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> Natural stream Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [12 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Any Natural)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [13 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Any)]
                                           [stream-take (-> stream Natural (Listof Natural))])
              ]]
       [14 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take Any])
              ]]
       [15 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)]
                                           [stream-unfold (-> stream (values Natural stream))]
                                           [stream-get (-> stream Natural Natural)]
                                           [stream-take (-> Natural stream (Listof Natural))])
              ]]
       [16 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
               (require/typed/check/provide "streams.rkt"
                                            [make-stream (-> Natural (-> stream) stream)]
                                            [stream-unfold (-> stream (values Natural stream))]
                                            [stream-get (-> stream Natural Natural)]
                                            [stream-take (-> stream Any (Listof Natural))])
               ]]
       [17 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
               (require/typed/check/provide "streams.rkt"
                                            [make-stream (-> Natural (-> stream) stream)]
                                            [stream-unfold (-> stream (values Natural stream))]
                                            [stream-get (-> stream Natural Natural)]
                                            [stream-take (-> stream Natural Any)])
               ]]
       [18 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

               (struct stream ([first : Natural]
                               [rest : (-> stream)])
                 #:prefab)
               (require/typed/check/provide "streams.rkt"
                                            [make-stream (-> Natural (-> stream) stream)]
                                            [stream-unfold (-> stream (values Natural stream))]
                                            [stream-get (-> stream Natural Natural)]
                                            [stream-take (-> stream Natural (Listof Any))])
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
                (require/typed/check/provide "streams.rkt"
                                             [make-stream (-> Natural (-> stream) stream)])]]
       [0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (define-type BoxQuad Any)
              (define-type RunQuad (List* 'run QuadAttrs QuadList))
              (require/typed/check/provide "streams.rkt"
                                           [make-stream (-> Natural (-> stream) stream)])]]
       [1 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

                (define-type BoxQuad (List* Any QuadAttrs QuadList))
                (define-type RunQuad (List* 'run QuadAttrs QuadList))
                (require/typed/check/provide "streams.rkt"
                                             [make-stream (-> Natural (-> stream) stream)])]]
       [2 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

                (define-type BoxQuad (List* 'box QuadAttrs QuadList))
                (define-type RunQuad Any)
                (require/typed/check/provide "streams.rkt"
                                             [make-stream (-> Natural (-> stream) stream)])]]
       [3 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

                (define-type BoxQuad (List* 'box QuadAttrs QuadList))
                (define-type RunQuad (List* Any QuadAttrs QuadList))
                (require/typed/check/provide "streams.rkt"
                                             [make-stream (-> Natural (-> stream) stream)])]]
       [4 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (define-type BoxQuad (List* 'box QuadAttrs QuadList))
              (define-type RunQuad (List* 'run QuadAttrs QuadList))
              (require/typed/check/provide "streams.rkt"
                                            [make-stream Any])
              ]]))

    (test-mutation/sequence
     #'[(require "../../../utilities/require-typed-check-provide.rkt")

        (require/typed/check/provide "streams.rkt"
                                     [a (Class (field [x Integer]
                                                      [y String]))])
        ]
     `([0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [a Any])
              ]]
       [1 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [a (Class (field Any
                                                            [y String]))])
              ]]
       [2 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [a (Class (field [x Any]
                                                            [y String]))])
              ]]
       [3 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [a (Class (field [x Integer]
                                                            Any))])
              ]]
       [4 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

              (require/typed/check/provide "streams.rkt"
                                           [a (Class (field [x Integer]
                                                            [y Any]))])
              ]]))
    (test-mutation/sequence
     #'[(require "../../../utilities/require-typed-check-provide.rkt")

        (require/typed/check/provide "streams.rkt"
                                     [a (Class (field [x Integer]
                                                      [y String])
                                               [get-field:x G]
                                               [get-field:y G]
                                               [set-field:x S]
                                               [set-field:y S])])
        ]
     `([0 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

        (require/typed/check/provide "streams.rkt"
                                     [a Any])
        ]]
       ;; Now swap is enabled since there are get/setters
       [1 ,#'[(require "../../../utilities/require-typed-check-provide.rkt")

        (require/typed/check/provide "streams.rkt"
                                     [a (Class (field [x String]
                                                      [y Integer])
                                               [get-field:x G]
                                               [get-field:y G]
                                               [set-field:x S]
                                               [set-field:y S])])
        ]]))))
