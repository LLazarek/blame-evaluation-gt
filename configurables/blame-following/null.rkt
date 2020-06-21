#lang at-exp racket

;; Null hypothesis blame following strategy:
;; Just pick a random next component to type.

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-blamed
           (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

(require "../../util/path-utils.rkt"
         "../../runner/error-extractors/blamed-location-extractor.rkt"
         "../../runner/error-extractors/extract-runtime-error-location.rkt"
         "../../runner/program.rkt"
         racket/random)

(define (make-extract-blamed the-program
                             format-mutant-info-for-error)
  (define config (infer-config-from-program the-program))
  (define untyped-mods (for/list ([{mod-name level} (in-hash config)]
                                  #:when (equal? level 'none))
                         mod-name))
  (const
   (if (empty? untyped-mods)
       empty
       (random-sample untyped-mods 1))))

(define (infer-config-from-program the-program)
  (define bench-path
    (match (explode-path/string (mod-path (program-main the-program)))
      [(list path-parts ... (or "typed" "untyped") _)
       (apply build-path path-parts)]))
  (define untyped
    (path->string (simple-form-path (build-path bench-path "untyped"))))
  (define typed
    (path->string (simple-form-path (build-path bench-path "typed"))))
  (define all-mods (map (compose1 path->string
                                  simple-form-path
                                  mod-path)
                        (cons (program-main the-program)
                              (program-others the-program))))
  (for/hash ([mod (in-list all-mods)]
             #:when #t
             [untyped-mod? (in-value (string-prefix? mod untyped))]
             [typed-mod?   (in-value (string-prefix? mod typed))]
             #:when (or untyped-mod? typed-mod?))
    (values (file-name-string-from-path mod)
            (if untyped-mod? 'none 'types))))

(module+ test
  (require ruinit
           "../../configurations/configure-benchmark.rkt"
           racket/runtime-path)
  (define-runtime-path sieve-path "../../../gtp-benchmarks/benchmarks/sieve")
  (define sieve-prog
    (make-program (build-path sieve-path "typed" "main.rkt")
                  (list
                   (build-path sieve-path "untyped" "streams.rkt"))))
  (test-begin
    #:name infer-config-from-program
    (test-equal? (infer-config-from-program sieve-prog)
                 (hash "main.rkt" 'types
                       "streams.rkt" 'none)))
  (test-begin
    #:name make-extract-blamed
    (test-equal? ((make-extract-blamed sieve-prog void) 42)
                 '("streams.rkt"))

    (ignore
     (define untyped-sieve-prog
       (make-program (build-path sieve-path "untyped" "main.rkt")
                     (list
                      (build-path sieve-path "untyped" "streams.rkt")))))
    (test-match ((make-extract-blamed untyped-sieve-prog void) 42)
                (list (or "main.rkt" "streams.rkt")))

    (ignore
     (define typed-sieve-prog
       (make-program (build-path sieve-path "typed" "main.rkt")
                     (list
                      (build-path sieve-path "typed" "streams.rkt")))))
    (test-equal? ((make-extract-blamed typed-sieve-prog void) 42)
                 empty)

    (ignore
     (define typed-sieve-prog-with-base
       (make-program (build-path sieve-path "typed" "main.rkt")
                     (list
                      (build-path sieve-path "typed" "streams.rkt")
                      ;; This is like a base module
                      "null.rkt"))))
    (test-equal? ((make-extract-blamed typed-sieve-prog-with-base void) 42)
                 empty)))
