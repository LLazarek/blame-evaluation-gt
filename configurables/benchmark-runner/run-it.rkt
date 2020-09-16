#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt"
         "../../util/path-utils.rkt"
         "../../util/program.rkt"
         syntax/parse)

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c]))

(require "../../runner/instrumented-runner.rkt")
(define (make-benchmark-runner program mod-name index)
  (run-with:require (should-run-program-with-main-submod? program)))

(define (should-run-program-with-main-submod? program)
  (define main-mod (findf (compose1 (path-ends-with "main.rkt")
                                    mod-path)
                          (program->mods program)))
  (mod-has-submod? main-mod 'main))

(define (mod-has-submod? mod name)
  ;; Syntactic method; tried doing this the right way using
  ;; `module-compiled-submodules`, but couldn't get the relative requires to
  ;; work. This works for now since none of the benchmarks use unexpected #langs
  ;; or fancy main-submod-generating macros.
  (syntax-parse (mod-stx mod)
    [(module _ _
       (#%module-begin
        {~or ({~or {~datum module}
                   {~datum module*}
                   {~datum module+}}
              mod-name:id . _)
             _} ...))
     (member name (map syntax->datum (attribute mod-name)))]))

(module+ test
  (require ruinit)
  (test-begin
    (mod-has-submod? (mod "A.rkt" #'(module A racket
                                      (#%module-begin
                                       (define x 42)
                                       (module+ main
                                         (* x x)))))
                     'main)
    (not (mod-has-submod? (mod "A.rkt" #'(module A racket
                                           (#%module-begin
                                            (define x 42)
                                            (module+ main
                                              (* x x)))))
                          'test))))
