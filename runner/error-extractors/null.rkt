#lang at-exp racket

;; Null hypothesis stack inspection strategy:
;; Just pick a random next component to type, regardless of the error info.

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-runtime-error-location
           (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

(require "../../util/program.rkt"
         racket/random)

(define (make-extract-runtime-error-location the-program
                                             program-config
                                             format-mutant-info-for-error)
  (define untyped-mods (for/list ([{mod-name level} (in-hash program-config)]
                                  #:when (equal? level 'none))
                         mod-name))
  (const
   (if (empty? untyped-mods)
       empty
       (random-sample untyped-mods 1))))

(module+ test
  (require ruinit
           "../../configurations/configure-benchmark.rkt"
           racket/runtime-path)
  (define-runtime-path sieve-path "../../../gtp-benchmarks/benchmarks/sieve")
  (define sieve-prog
    (make-program (build-path sieve-path "typed" "main.rkt")
                  (list
                   (build-path sieve-path "typed" "streams.rkt"))))
  (define sieve-config (hash "main.rkt" 'types
                             "streams.rkt" 'none))
  (test-begin
    #:name make-extract-runtime-error-location
    (test-equal? ((make-extract-runtime-error-location sieve-prog
                                                       sieve-config
                                                       void)
                  42)
                 '("streams.rkt"))

    (ignore
     (define sieve-config-all-ut
       (hash "main.rkt" 'none
             "streams.rkt" 'none)))
    (test-match ((make-extract-runtime-error-location sieve-prog
                                                      sieve-config-all-ut
                                                      void)
                 42)
                (list (or "main.rkt" "streams.rkt")))

    (ignore
     (define sieve-config-all-t
       (hash "main.rkt" 'types
             "streams.rkt" 'types)))
    (test-equal? ((make-extract-runtime-error-location sieve-prog
                                                       sieve-config-all-t
                                                       void)
                  42)
                 empty)

    (ignore
     (define typed-sieve-prog-with-base
       (make-program (build-path sieve-path "typed" "main.rkt")
                     (list
                      (build-path sieve-path "typed" "streams.rkt")
                      ;; This is like a base module
                      "null.rkt"))))
    (test-equal? ((make-extract-runtime-error-location typed-sieve-prog-with-base
                                                       sieve-config-all-t
                                                       void)
                  42)
                 empty)))
