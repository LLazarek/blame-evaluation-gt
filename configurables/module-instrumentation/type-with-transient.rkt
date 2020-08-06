#lang at-exp racket/base

(require racket/format
         racket/function
         racket/match
         racket/port
         racket/runtime-path
         syntax/parse
         (prefix-in db: "../../db/db.rkt")
         "../../db/util.rkt"
         "../../runner/program.rkt"
         "../../util/optional-contracts.rkt"
         "../../util/read-module.rkt"
         "../../util/path-utils.rkt"
         "instrument-module.rkt")

(provide (contract-out
          [instrument-module module-instrumenter/c]
          [transient-special-cases-db (db-path-relative-to? configurables)]))

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
                          tr}
         (#%module-begin . body))
       (syntax/loc this-syntax
         (module name tr (#%module-begin #:transient . body)))]
      [other this-syntax]))
  (struct-copy mod a-mod
               [stx instrumented-stx]))

(define (mod-stx/replace-special-cases a-mod)
  (define mod-relative-path
    (benchmark-mod-relative-path (mod-path a-mod)))
  (define resolved-db-path (build-path configurables
                                       (transient-special-cases-db)))
  (define db (db:get resolved-db-path))
  (match (db:read db
                  mod-relative-path
                  #f)
    [#f (mod-stx a-mod)]
    [(? string? mod-str)
     (with-input-from-string mod-str
       (thunk (read-module/port (current-input-port)
                                #:source (mod-path a-mod))))]
    [else
     (raise-user-error
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
  (when (db:path-to-db? default-transient-special-cases-db)
    (test-begin
      #:name instrument-module
      (ignore (define db (db:get default-transient-special-cases-db)))
      (string? (db:read db "lnm/typed/lnm-plot.rkt"))

      (test-equal?
       (syntax->datum (mod-stx
                       (instrument-module
                        (mod "foo/typed/main.rkt"
                             #'(module foo typed/racket (#%module-begin 42))))))
       '(module foo typed/racket (#%module-begin #:transient 42)))
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
       `(module ,_ typed/racket/base
          (#%module-begin #:transient . ,_))))))
