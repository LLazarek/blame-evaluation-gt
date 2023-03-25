#lang at-exp racket

(provide (contract-out
          [instrument-program instrument-program/c]

          [instrument-program/adapter-generator
           (program/c
            (mod/c . -> . resolved-module?)
            (syntax? syntax? string? string? . -> . syntax?)
            . -> .
            instrumented-program/c)])
         type-interface-file-name
         type-interface-file-rename)

(require "../../util/program.rkt"
         "../../util/path-utils.rkt"
         "../../util/logging.rkt"
         mutate/logger
         "../../mutation-adapter/generate-adapters.rkt"
         "../../runner/instrumented-runner.rkt"
         "instrument-program.rkt"
         (rename-in "just-instrument-modules.rkt"
                    [instrument-program instrument-all-the-modules])
         "type-interface-module-names.rkt")

(define type-interface-adapter-temporary-name "type-interface-adapter.rkt")

(define (instrument-program a-program make-instrumented-module)
  (instrument-program/adapter-generator a-program
                                        make-instrumented-module
                                        generate-adapter-module-for-mutation))

(define (instrument-program/adapter-generator a-program
                                              make-instrumented-module
                                              generate-adapter-module-for-mutation)
  (define interface-mod (program->interface-mod a-program))
  (unless interface-mod
    (error 'instrument-modules-and-insert-interface-adapter-module:instrument-program
           @~a{
               expected to be called with a program that has a type-interface module,
               but given one that does not.
               }))

  (define-values {instrumented-program mutation-type}
    (instrument-program/get-mutation-type a-program make-instrumented-module))
  (define original-interface-mod-stx (mod-stx interface-mod))
  (define mutated-interface-mod-stx (instrumented-program->interface-mod-stx instrumented-program))
  (define adapter-mod-stx
    (generate-adapter-module-for-mutation original-interface-mod-stx
                                          mutated-interface-mod-stx
                                          mutation-type
                                          type-interface-file-rename))
  (insert-adapter-mod instrumented-program
                      adapter-mod-stx
                      make-instrumented-module))

(define (instrument-program/get-mutation-type a-program make-instrumented-module)
  (define-values {mutation-log-messages instrumented-program}
    (with-collected-log-messages mutate-logger 'info 'mutate
      (match-lambda [(vector _
                             _
                             (list type before-stx after-stx)
                             _)
                     (list type before-stx after-stx)]
                    [other #f])
      (thunk (instrument-all-the-modules a-program make-instrumented-module))))
  (define mutation-type (match (filter-not false? mutation-log-messages)
                          [(list (list type _ _)) type]
                          [else
                           (error 'instrument-program/get-mutation-type
                                  "instrumenting the program did not log a mutation")]))
  (values instrumented-program mutation-type))

;; program/c . -> . (or/c mod/c #f)
(define (program->interface-mod a-program)
  (match (program->mods a-program)
    [(list _ ... (and im (mod (? (path-ends-with type-interface-file-name)) _)) _ ...) im]
    [else #f]))

;; (listof resolved-module?) -> (values resolved-module? (listof resolved-module?))
(define split-interface-mod/rest
  (match-lambda [(list-no-order (and interface-mod
                                     (resolved-module (? (path-ends-with type-interface-file-name))
                                                      _
                                                      _
                                                      _
                                                      _))
                                others ...)
                 (values interface-mod others)]))

(define (instrumented-program->interface-mod-stx an-instrumented-program)
  (define-values {interface-mod others}
    (split-interface-mod/rest (cons (program-main an-instrumented-program)
                                    (program-others an-instrumented-program))))
  (resolved-module-stx interface-mod))

(define (insert-adapter-mod an-instrumented-program
                            adapter-mod-stx
                            make-instrumented-module)
  (match-define (program main non-main) an-instrumented-program)
  (define-values {interface-mod others} (split-interface-mod/rest non-main))
  (define interface-mod-path (resolved-module-path-string interface-mod))
  (define instrumented-adapter-mod
    (make-instrumented-module
     (mod (path-replace-filename interface-mod-path
                                 type-interface-adapter-temporary-name)
          adapter-mod-stx)))
  (program main
           (list* (relocate-resolved-module instrumented-adapter-mod
                                            (simple-form-path interface-mod-path))
                  (relocate-resolved-module interface-mod
                                            (path-replace-filename interface-mod-path
                                                                   type-interface-file-rename))
                  others)))

(define (relocate-resolved-module a-resolved-module new-path)
  (mod->resolved-module (mod new-path
                             (resolved-module-stx a-resolved-module))))

(module+ test
  (require ruinit
           racket/runtime-path
           "../mutation/type-api-mutators.rkt"
           "../configurables.rkt")

  (define-runtime-path type-api-mutators.rkt "../../mutation-adapter/mutation-adapter.rkt")
  (define-runtime-path interface-adapter-test-config.rkt
    "interface-adapter-test-config.rkt")

  (define (make-test-resolved-mod path stx)
    (resolved-module path #f #f #f stx))
  (define ((make-test-mod-instrumentor mutation-type new-interface-stx) a-mod)
    (match a-mod
      [(mod (== type-interface-file-name) _)
       (log-mutation 1 2 mutation-type)
       (make-test-resolved-mod type-interface-file-name new-interface-stx)]
      [(mod path stx)
       (make-test-resolved-mod path stx)]))
  (define (resolved-module->datum rm)
    (struct-copy resolved-module rm [stx (syntax->datum (resolved-module-stx rm))]))
  (define (program->datum p)
    (program (resolved-module->datum (program-main p))
             (map resolved-module->datum (program-others p))))

  (test-begin
    #:name instrument-program/get-mutation-type
    (let-values ([{instrumented-program type}
                  (instrument-program/get-mutation-type
                   (program (mod "main.rkt" #'(module main racket
                                                (#%module-begin
                                                 (+ 2 2))))
                            (list (mod type-interface-file-name
                                       #'(module main racket
                                           (#%module-begin
                                            (require/typed/check/provide
                                             "something"
                                             [f (-> Number Real String)]))))))
                   (make-test-mod-instrumentor type:function-arg-swap
                                               #'(module main racket
                                                   (#%module-begin
                                                    (require/typed/check/provide
                                                     "something"
                                                     [f (-> Real Number String)])))))])
      (test-equal? type type:function-arg-swap))

    ;; Now test some real mutation logging
    (ignore (install-configuration! interface-adapter-test-config.rkt)
            (define the-program
              (program
               (mod "main.rkt" #'(module main racket
                                   (#%module-begin
                                    (+ 2 2))))
               (list
                (mod type-interface-file-name
                     #'(module type-interface typed/racket
                         (#%module-begin
                          (require "../../../utilities/require-typed-check-provide.rkt")
                          (struct stream ((first : Natural) (rest : (-> stream))) #:prefab)
                          (require/typed/check/provide
                           "streams.rkt"
                           (make-stream (-> Natural (-> stream) stream))
                           (stream-unfold (-> stream (values Natural stream)))
                           (stream-get (-> stream Natural Natural))
                           (stream-take (-> stream Natural (Listof Natural)))))))))))
    (for/and/test ([expected-mutation-type (list
                                            ;; Refer to test `mutate-benchmark` in `mutate-type-interface.rkt`
                                            ;; the struct
                                            type:struct-field-swap
                                            type:base-type-substitution
                                            type:complex-type->Any

                                            ;; make-stream
                                            type:complex-type->Any
                                            type:function-arg-swap
                                            type:base-type-substitution
                                            type:complex-type->Any

                                            ;; stream-unfold
                                            type:complex-type->Any
                                            type:function-result-swap
                                            type:base-type-substitution

                                            ;; stream-get
                                            type:complex-type->Any
                                            type:function-arg-swap
                                            type:base-type-substitution
                                            type:base-type-substitution

                                            ;; stream-take
                                            type:complex-type->Any
                                            type:function-arg-swap
                                            type:base-type-substitution
                                            type:complex-type->Any
                                            type:base-type-substitution)]
                   [i (in-naturals)])
      (define-values {instrumented-program type}
        (instrument-program/get-mutation-type
         the-program
         (λ (a-mod)
           (local-require "../../runner/mutation-runner.rkt")
           (match a-mod
             [(mod (== type-interface-file-name) _)
              (define-values {mutated-stx mutated-id}
                (mutate-module a-mod i #:in the-program))
              (make-test-resolved-mod type-interface-file-name
                                      mutated-stx)]
             [(mod path stx)
              (make-test-resolved-mod path stx)]))))
      (test-equal? type expected-mutation-type)))

  (test-begin
    #:name instrument-program
    (let* ([main.rkt-path "main.rkt"]
           [main.rkt-stx  #'(module main racket
                              (#%module-begin
                               (+ 2 2)))]
           [p (program->datum
               (instrument-program
                (program (mod main.rkt-path main.rkt-stx)
                         (list (mod type-interface-file-name
                                    #'(module interface racket
                                        (#%module-begin
                                         (require/typed/check/provide
                                          "something"
                                          [f (-> Number Real String)]))))))
                (make-test-mod-instrumentor type:function-arg-swap
                                            #'(module interface racket
                                                (#%module-begin
                                                 (require/typed/check/provide
                                                  "something"
                                                  [f (-> Real Number String)]))))))]
           [others (program-others p)]
           [rm->name+code (λ (rm) (cons (resolved-module-path-string rm)
                                        (resolved-module-stx rm)))])
      (and/test
       ;; Main is instrumented but otherwise untouched
       (test-equal? (program-main p)
                    (resolved-module->datum (make-test-resolved-mod main.rkt-path main.rkt-stx)))

       ;; Interface module is moved and adapter injected
       (test-match (make-immutable-hash (map rm->name+code others))
                   (hash-table [(== (path->string (simple-form-path type-interface-file-name)))
                                `(module mutation-adapter typed/racket
                                   (#%module-begin
                                    (module contracted racket
                                      (require (file ,(== (path->string
                                                           (simple-form-path
                                                            type-api-mutators.rkt)))))
                                      (require ,type-interface-file-rename)
                                      (provide (except-out (all-from-out ,type-interface-file-rename)
                                                           f))
                                      (begin
                                        (define ,f-gensym (contract (swap-> #t 0 1)
                                                                    f
                                                                    #f
                                                                    #f))
                                        (provide (rename-out [,f-gensym f]))))
                                    (require "../../../utilities/require-typed-check-provide.rkt")
                                    (require/typed/check/provide 'contracted
                                                                 [f (-> Number Real String)])))]

                               [(== (path->string (simple-form-path type-interface-file-rename)))
                                '(module interface racket
                                   (#%module-begin
                                    (require/typed/check/provide
                                     "something"
                                     [f (-> Real Number String)])))]))))

    (test-exn exn:fail?
              (instrument-program
               (program (mod "main.rkt" #'(module main racket
                                            (#%module-begin
                                             (+ 2 2))))
                        empty)
               (make-test-mod-instrumentor type:function-arg-swap
                                           #'(module interface racket
                                               (#%module-begin
                                                (require/typed/check/provide
                                                 "something"
                                                 [f (-> Real Number String)]))))))))
