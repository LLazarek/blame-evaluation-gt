#lang at-exp racket

(require racket/runtime-path
         syntax/parse
         (prefix-in db: "../../db/db.rkt")
         "../../db/util.rkt"
         "../../util/program.rkt"
         "../../util/optional-contracts.rkt"
         "../../util/read-module.rkt"
         "../../util/path-utils.rkt"
         "../../util/experiment-exns.rkt"
         "instrument-module.rkt")

(provide (contract-out
          [instrument-module module-instrumenter/c]
          [transient-special-cases-db (parameter/c (db-path-relative-to? configurables))]))

(define-runtime-path configurables "..")
(define default-transient-special-cases-db
  "module-instrumentation/transient-special-cases/default.rktdb")
(define transient-special-cases-db
  (make-parameter default-transient-special-cases-db))

(define (instrument-module a-mod)
  (define stx (mod-stx/replace-special-cases a-mod))
  (define instrumented-stx
    (syntax-parse stx
      [(module name {~and {~or* {~datum typed/racket}
                                {~datum typed/racket/base}}
                          lang}
         (mod-begin . body))
       #:with t/r/s (datum->syntax #'lang 'typed/racket/shallow)
       #:with instrumented-body (redirect-require/typed/checks #'body)
       (syntax/loc this-syntax
         (module name t/r/s (mod-begin . instrumented-body)))]
      [other this-syntax]))
  (struct-copy mod a-mod
               [stx instrumented-stx]))

(define-syntax-class tlf
  #:description "top level form"
  (pattern ({~datum require} {~alt "../../../utilities/require-typed-check-provide.rkt"
                                   {~datum require-typed-check}
                                   other-reqs}
                             ...)
           #:with redirected (datum->syntax this-syntax
                                            `(require
                                              "../../../utilities/require-typed-check-provide-transient.rkt"
                                              . ,(attribute other-reqs))))
  (pattern something-else
           #:with redirected this-syntax))

(define (redirect-require/typed/checks stx)
  (syntax-parse stx
    [[top-level-form:tlf ...]
     (syntax/loc this-syntax
       (top-level-form.redirected ...))]))

(define (mod-stx/replace-special-cases a-mod)
  (define mod-relative-path
    (benchmark-mod-relative-path (mod-path a-mod)))
  (define resolved-db-path (build-path configurables
                                       (transient-special-cases-db)))
  (define replacement-mod
    (cond [(db:path-to-db? resolved-db-path)
           (define db (db:get resolved-db-path))
           (db:read db
                    mod-relative-path
                    #f)]
          [else
           (log-warning @~a{
                            type-with-transient: @;
                            transient-special-cases-db is misconfigured or not configured: @;
                            @(transient-special-cases-db)
                            })
           #f]))
  (match replacement-mod
    [#f (mod-stx a-mod)]
    [(? string? mod-str)
     (with-input-from-string mod-str
       (thunk (read-module/port (current-input-port)
                                #:source (mod-path a-mod))))]
    [else
     (raise-experiment-user-error
      'instrument-module:type-with-transient
      @~a{
          Invalid special case found in special cases db @;
          at @(transient-special-cases-db) with key @mod-relative-path
          Expected a literal string containing the source code of
          a module. Found something that is not a string.
          })]))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name instrument-module
    (test-equal?
     (syntax->datum (mod-stx
                     (instrument-module
                      (mod "foo/typed/main.rkt"
                           #'(module foo typed/racket (#%module-begin 42))))))
     '(module foo typed/racket/shallow (#%module-begin 42)))
    (test-equal?
     (syntax->datum (mod-stx
                     (instrument-module
                      (mod "foo/untyped/main.rkt"
                           #'(module foo racket (#%module-begin 42))))))
     '(module foo racket (#%module-begin 42)))

    (test-match
     (syntax->datum (mod-stx
                     (instrument-module
                      (mod "lnm/typed/lnm-plot.rkt"
                           #'(module lnm-plot typed/racket (#%module-begin 42))))))
     `(module ,_ typed/racket/shallow
        (#%module-begin . ,_)))
    (test-match
     (syntax->datum (mod-stx
                     (instrument-module
                      (mod "foo/typed/main.rkt"
                           #'(module foo typed/racket (#%module-begin
                                                       (require require-typed-check
                                                                other-thing
                                                                "../../../utilities/require-typed-check-provide.rkt"
                                                                foobar)
                                                       (define (f x) x)
                                                       42))))))
     `(module ,_ typed/racket/shallow
        (#%module-begin
         (require "../../../utilities/require-typed-check-provide-transient.rkt"
                  other-thing
                  foobar)
         (define (f x) x)
         42)))))
