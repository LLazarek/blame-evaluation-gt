#lang at-exp racket

(provide (contract-out [instrument-program instrument-program/c]))

(require "../../util/program.rkt"
         "../../util/logging.rkt"
         "../../mutate/logger.rkt"
         "../../mutation-adapter/generate-adapters.rkt"
         "instrument-program.rkt"
         (rename-in "just-instrument-modules.rkt"
                    [instrument-program instrument-all-the-modules]))

(define type-interface-file-name "type-interface.rkt")
(define type-interface-file-rename "original-type-interface.rkt")
(define type-interface-adapter-temporary-name "type-interface-adapter.rkt")

;; wip
(define (instrument-program a-program make-instrumented-module)
  (unless (has-interface-module? a-program)
    (error 'instrument-modules-and-insert-interface-adapter-module:instrument-program
           @~a{
               expected to be called with a program that has a type-interface module,
               but given one that does not.
               }))

  (define-values {instrumented-program mutation-type}
    (instrument-program/get-mutation-type a-program make-instrumented-module))
  (define original-interface-mod-stx (program->interface-mod-stx a-program))
  (define mutated-interface-mod-stx (instrumented-program->interface-mod-stx instrumented-program))
  (define adapter-mod-stx
    (generate-adapter-module-for-mutation original-interface-mod-stx
                                          mutated-interface-mod-stx
                                          mutation-type
                                          type-interface-file-rename))
  (define instrumented-adapter-mod
    (instrument-module (mod (path-replace-filename (mod-path (program-main a-program))
                                                   type-interface-adapter-temporary-name)
                            adapter-mod-stx)))
  (insert-adapter-mod instrumented-program
                      instrumented-adapter-mod))

(struct type (name) #:transparent)
(struct mutating () #:transparent)

(define (instrument-program/get-mutation-type a-program make-instrumented-module)
  (define-values {mutation-log-messages instrumented-program}
    (with-collected-log-messages mutate-logger 'info 'mutate
      (match-lambda [(vector level
                             (regexp @~a{^@log-prefix:mutation-type (.+)$} (list _ type-name))
                             msg
                             _
                             _)
                     (type type-name)]
                    [(vector level
                             (regexp @~a{^@log-prefix:mutating .*})
                             msg
                             _
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
            "expected a sequence of mutation type message followed by mutating message in mutation log messages, but didn't find it")]))
