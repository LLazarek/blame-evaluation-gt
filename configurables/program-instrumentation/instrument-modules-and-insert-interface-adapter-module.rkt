#lang at-exp racket

(provide (contract-out [instrument-program instrument-program/c])
         type-interface-file-name)

(require "../../util/program.rkt"
         "../../util/logging.rkt"
         "../../util/path-utils.rkt"
         "../../mutate/logger.rkt"
         "../../mutation-adapter/generate-adapters.rkt"
         "../../runner/instrumented-runner.rkt"
         "instrument-program.rkt"
         (rename-in "just-instrument-modules.rkt"
                    [instrument-program instrument-all-the-modules]))

(define type-interface-file-name "type-interface.rkt")
(define type-interface-file-rename "original-type-interface.rkt")
(define type-interface-adapter-temporary-name "type-interface-adapter.rkt")

(define (instrument-program a-program make-instrumented-module)
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

(struct type (name) #:transparent)
(struct mutating () #:transparent)

(define (instrument-program/get-mutation-type a-program make-instrumented-module)
  (define-values {mutation-log-messages instrumented-program}
    (with-collected-log-messages mutate-logger 'info 'mutate
      (match-lambda [(vector level
                             (regexp @~a{^mutate: @log-prefix:mutation-type (.+)$}
                                     (list _ type-name))
                             msg
                             _)
                     (type type-name)]
                    [(vector level
                             (regexp @~a{^mutate: @log-prefix:mutating .*})
                             msg
                             _)
                     (mutating)]
                    [other #f])
      (thunk (instrument-all-the-modules a-program make-instrumented-module))))
  (define mutation-type (find-mutation-type (filter-not false? mutation-log-messages)))
  (values instrumented-program mutation-type))

;; (listof (or/c type? mutating?)) -> string?
;; Assumption: there's only one `mutating?` in `mutation-log-messages`, and
;; there's at least one `type?` before it.
(define (find-mutation-type mutation-log-messages)
  (match mutation-log-messages
    [(list _ ... (type type-name) (mutating) _ ...)
     type-name]
    [else
     (error 'find-mutation-type
            @~a{
                expected a sequence of mutation type messages followed by mutating message @;
                in mutation log messages, but didn't find it
                })]))

;; program/c . -> . (or/c mod/c #f)
(define (program->interface-mod a-program)
  (match (program->mods a-program)
    [(list _ ... (and im (mod (? (path-ends-with type-interface-file-name)) _)) _ ...) im]
    [else #f]))

;; (listof resolved-module?) -> (values resolved-module? (listof resolved-module?))
(define split-interface-mod/rest
  (match-lambda [(list-no-order (and interface-mod
                                     (resolved-module (== type-interface-file-name)
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
           "../../mutate/type-api-mutators.rkt")

  (define-runtime-path type-api-mutators.rkt "../../mutation-adapter/type-api-mutators.rkt")

  (define (make-test-resolved-mod path stx)
    (resolved-module path #f #f #f stx))
  (define ((make-test-mod-instrumentor mutation-type new-interface-stx) a-mod)
    (match a-mod
      [(mod (== type-interface-file-name) _)
       (log-mutation-type mutation-type)
       (log-mutation-type mutation-type)
       (log-mutation 1 2)
       (log-mutation-type mutation-type)
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
      (test-equal? type type:function-arg-swap)))

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
       (test-equal? (make-immutable-hash (map rm->name+code others))
                    (hash (path->string (simple-form-path type-interface-file-name))
                          `(module mutation-adapter racket
                             (#%module-begin
                              (require (file ,(path->string
                                               (simple-form-path
                                                type-api-mutators.rkt))))
                              (require ,type-interface-file-rename)
                              (provide (except-out (all-from-out ,type-interface-file-rename)
                                                   f))
                              (provide (contract-out
                                        [f (swap-> #t 0 1)]))))

                          (path->string (simple-form-path type-interface-file-rename))
                          '(module interface racket
                             (#%module-begin
                              (require/typed/check/provide
                               "something"
                               [f (-> Real Number String)])))))))

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
