#lang at-exp racket

(require "instrument-modules-and-insert-interface-adapter-module.rkt"
         "../../util/program.rkt"
         "../../util/path-utils.rkt"
         mutate/logger
         "../../mutation-adapter/generate-adapters.rkt"
         "../../runner/instrumented-runner.rkt"
         "instrument-program.rkt")

(require ruinit
         racket/runtime-path
         syntax/parse
         "../mutation/type-api-mutators.rkt"
         "../configurables.rkt")

(test-begin
  #:name functional
  (ignore (define test:mod->resolved-module
            (match-lambda
              [(mod path stx)
               (resolved-module path #f #f #f stx)]))
          (define-test (test-program p main-name main-test others-name-test-dict)
            (define main (program-main p))
            (define others (program-others p))
            (and/test/message
             [(test-equal? (resolved-module-path-string main) main-name)
              "main module names differ:"]
             [(main-test (resolved-module-stx main))
              "main module syntax check fails:"]

             [(parameterize ([test-equal?-diff-values #f])
                (test-equal? (sort (map resolved-module-path-string others) string<?)
                             (sort (dict-keys others-name-test-dict) string<?)))
              "`program-others` are different:"]

             [(for/and/test ([other (in-list (sort others string<? #:key resolved-module-path-string))]
                             [key (in-list (sort (dict-keys others-name-test-dict) string<?))])
               (define test (dict-ref others-name-test-dict key))
               (extend-test-message
                (test (resolved-module-stx other))
                #:append? #f
                @~a{module @key :}))
              "an other module syntax check fails:"]))
          (define-test (test-interface-mutation-adapter #:program-location mod-path-prefix
                                                        #:original-interface-body interface-original-body
                                                        #:mutated-interface-body interface-mutated-body
                                                        #:type mutation-type
                                                        #:logged logged-stxs

                                                        #:check-adapter-mod-body check-adapter-mod-body
                                                        . mod-name+body-stxs)
            (define (make-mod-body-stx body-stx)
              #`(module test-module racket
                  (#%module-begin . #,body-stx)))
            (define (make-interface-mod-body-stx body-stx)
              #`(module type-interface typed/racket
                  (#%module-begin
                   (require "../../../utilities/require-typed-check-provide.rkt")
                   . #,body-stx)))
            (define mods
              (append (for/list ([name+body (in-list mod-name+body-stxs)])
                        (match-define (list name body-stx) name+body)
                        (mod (~a mod-path-prefix name) (make-mod-body-stx body-stx)))
                      (list (mod (~a mod-path-prefix type-interface-file-name)
                                 (make-interface-mod-body-stx interface-original-body)))))
            (define instrumentor
              (compose1 test:mod->resolved-module
                        (match-lambda
                          [(mod (== (~a mod-path-prefix type-interface-file-name)) _)
                           (log-mutation (first logged-stxs) (second logged-stxs) mutation-type)
                           (mod (~a mod-path-prefix type-interface-file-name)
                                (make-interface-mod-body-stx interface-mutated-body))]
                          [other other])))
            (match-define (list* (list main-mod-name main-mod-body-stx)
                                 other-mods)
              mod-name+body-stxs)
            (test-program
             (instrument-program (program (first mods) (rest mods)) instrumentor)
             (~a mod-path-prefix main-mod-name)
             (λ (stx) (test-equal? (syntax->datum stx)
                                   (syntax->datum (make-mod-body-stx main-mod-body-stx))))
             (hash-set* (for/hash ([name+body (in-list other-mods)])
                          (match-define (list name body-stx) name+body)
                          (values (~a mod-path-prefix name)
                                  (λ (stx)
                                    (test-equal? (syntax->datum stx)
                                                 (syntax->datum (make-mod-body-stx body-stx))))))

                        (~a mod-path-prefix type-interface-file-name)
                        (syntax-parser
                          [({~literal module}
                            {~datum mutation-adapter}
                            {~datum typed/racket}
                            ({~literal #%module-begin}
                             . body))
                           (check-adapter-mod-body #'body)]
                          [something-else
                           (fail @~a{
                                     adapter module is malformed at the top level:
                                     @pretty-format[(syntax->datum #'something-else)]
                                     })])

                        (~a mod-path-prefix type-interface-file-rename)
                        (λ (stx)
                          (test-equal? (syntax->datum stx)
                                       (syntax->datum (make-interface-mod-body-stx interface-mutated-body))))))))

  (test-interface-mutation-adapter
   #:program-location "/"
   (list "client.rkt"
         #'{(require "type-interface.rkt")
            (+ x 5)})
   (list "library.rkt"
         #'{(provide x)
            (define x 5)})
   #:original-interface-body #'{(require/typed/check/provide "library.rkt"
                                                             [x Integer])}
   #:mutated-interface-body #'{(require/typed/check/provide "library.rkt"
                                                            [x String])}
   #:type type:base-type-substitution
   #:logged (list #'Integer #'String)

   #:check-adapter-mod-body
   (λ (body-stx)
     (test-match
      (syntax->datum body-stx)
      `{(module contracted racket
          (require (file ,_))
          (require "original-type-interface.rkt")
          (provide (except-out (all-from-out "original-type-interface.rkt")
                               x))
          (begin
            (define ,x-gensym (contract #;(make-base-type-adapter 'Integer 'String) (sealing-adapter)
                                        x
                                        #f
                                        #f))
            (provide (rename-out [,x-gensym x]))))
        (require "../../../utilities/require-typed-check-provide.rkt") ;; inserted unconditionally
        (require "../../../utilities/require-typed-check-provide.rkt") ;; copied over from original
        (require/typed/check/provide 'contracted
                                     [x Integer])})))

  (test-interface-mutation-adapter
   #:program-location "/"
   (list "client.rkt"
         #'{(require "type-interface.rkt")
            (f 2 "hello")})
   (list "library.rkt"
         #'{(provide f)
            (define (f a s) (+ a (string-length s)))})
   #:original-interface-body #'{(require/typed/check/provide "library.rkt"
                                                             [f (-> Integer String Integer)])}
   #:mutated-interface-body #'{(require/typed/check/provide "library.rkt"
                                                            [f (-> String String Integer)])}
   #:type type:base-type-substitution
   #:logged (list #'Integer #'String)

   #:check-adapter-mod-body
   (λ (body-stx)
     (test-match
      (syntax->datum body-stx)
      `{(module contracted racket
          (require (file ,_))
          (require "original-type-interface.rkt")
          (provide (except-out (all-from-out "original-type-interface.rkt")
                               f))
          (begin
            (define ,f-gensym (contract (delegating->
                                         2
                                         (list (cons 0 #;(make-base-type-adapter 'Integer 'String)
                                                     (sealing-adapter)))
                                         (any/c-adapter)
                                         (list))
                                        f
                                        #f
                                        #f))
            (provide (rename-out [,f-gensym f]))))
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require/typed/check/provide 'contracted
                                     [f (-> Integer String Integer)])})))

  (test-interface-mutation-adapter
   #:program-location "/"
   (list "client.rkt"
         #'{(require "type-interface.rkt")
            (f 2 "hello")})
   (list "library.rkt"
         #'{(provide f g)
            (define (f a s) (+ a (string-length s)))
            (define (g x) x)})
   #:original-interface-body #'{(require/typed/check/provide "library.rkt"
                                                             [f (-> Integer String Integer)]
                                                             [g (-> Any Any)])}
   #:mutated-interface-body #'{(require/typed/check/provide "library.rkt"
                                                            [f (-> String Integer Integer)]
                                                            [g (-> Any Any)])}
   #:type type:function-arg-swap
   #:logged (list #'(-> Integer String Integer) #'(-> String Integer Integer))

   #:check-adapter-mod-body
   (λ (body-stx)
     (test-match
      (syntax->datum body-stx)
      `{(module contracted racket
          (require (file ,_))
          (require "original-type-interface.rkt")
          (provide (except-out (all-from-out "original-type-interface.rkt")
                               f))
          (begin
            (define ,f-gensym (contract (swap-> #t 0 1)
                                        f
                                        #f
                                        #f))
            (provide (rename-out [,f-gensym f]))))
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require/typed/check/provide 'contracted
                                     [f (-> Integer String Integer)]
                                     [g (-> Any Any)])})))

  (test-interface-mutation-adapter
   #:program-location "/"
   (list "client.rkt"
         #'{(require "type-interface.rkt")
            (f 2 "hello")})
   (list "library.rkt"
         #'{(provide f g)
            (define (f a s) (+ a (string-length s)))
            (define (g x) x)})
   #:original-interface-body #'{(define-type Foo Real)
                                (struct stream ([head : Natural]
                                                [rest : (-> stream)]))
                                (provide Foo
                                         (struct-out stream))
                                (require/typed/check/provide "library.rkt"
                                                             [f (-> Integer stream Integer)]
                                                             [g (-> Any Any)])}
   #:mutated-interface-body #'{(define-type Foo Real)
                               (struct stream ([head : Any]
                                               [rest : (-> stream)]))
                               (provide Foo
                                         (struct-out stream))
                               (require/typed/check/provide "library.rkt"
                                                            [f (-> Integer stream Integer)]
                                                            [g (-> Any Any)])}
   #:type type:base-type-substitution
   #:logged (list #'Natural #'Any)

   #:check-adapter-mod-body
   (λ (body-stx)
     (test-match
      (syntax->datum body-stx)
      `{(module contracted racket
          (require (file ,_))
          (require "original-type-interface.rkt")
          (define stream
            (recursive-contract
             (delegating-struct #f
                                'stream
                                2
                                (list (cons 0 (sealing-adapter))
                                      (cons 1 (delegating->
                                               0
                                               (list)
                                               (any/c-adapter)
                                               (list (cons 0 stream))))))))
          (provide (except-out (all-from-out "original-type-interface.rkt")
                               f))
          (begin
            (define ,f-gensym (contract (delegating-> 2
                                                      (list (cons 1 stream))
                                                      (any/c-adapter)
                                                      (list))
                                        f
                                        #f
                                        #f))
            (provide (rename-out [,f-gensym f]))))
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require "../../../utilities/require-typed-check-provide.rkt")
        (provide Foo (struct-out stream))
        (define-type Foo Real)
        (struct stream ([head : Natural]
                        [rest : (-> stream)]))
        (require/typed/check/provide 'contracted
                                     [f (-> Integer stream Integer)]
                                     [g (-> Any Any)])}))))
