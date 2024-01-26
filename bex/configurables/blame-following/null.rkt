#lang at-exp racket/base

;; Null hypothesis blame strategy: just like null stack inspection.
;; Just pick a random next component to type, regardless of the error info.

(require "../../util/optional-contracts.rkt"
         "blame-follower.rkt")
(provide (contract-out
          [follow-blame blame-follower/c]))


(require "../../util/program.rkt"
         "../../runner/mutation-runner-data.rkt"
         "../../configurations/config.rkt"
         racket/list
         racket/random)

(define (follow-blame a-run-status program-config)
  (define non-max-mods (for/list ([{mod-name level} (in-hash program-config)]
                                  #:when (not (config-at-max-precision-for? mod-name program-config)))
                         mod-name))
  (if (empty? non-max-mods)
      empty
      (random-sample non-max-mods 1)))

(module+ test
  (require ruinit
           "../../configurations/configure-benchmark.rkt"
           "../../configurables/configurables.rkt"
           racket/runtime-path)
  (define-runtime-path sieve-path "../../../../gtp-benchmarks/benchmarks/sieve")
  (define sieve-prog
    (make-program (build-path sieve-path "typed" "main.rkt")
                  (list
                   (build-path sieve-path "typed" "streams.rkt"))))
  (define sieve-config (hash "main.rkt" 'types
                             "streams.rkt" 'none))
  (parameterize ([configured:config-at-max-precision-for?
                  (λ (name config)
                    (equal? (hash-ref config name 'none)
                            'types))])
    (test-begin
      #:name null-follow-blame
      (test-equal? (follow-blame #f sieve-config)
                   '("streams.rkt"))

      (ignore
       (define sieve-config-all-ut
         (hash "main.rkt" 'none
               "streams.rkt" 'none)))
      (test-match (follow-blame #f sieve-config-all-ut)
                  (list (or "main.rkt" "streams.rkt")))

      (ignore
       (define sieve-config-all-t
         (hash "main.rkt" 'types
               "streams.rkt" 'types)))
      (test-equal? (follow-blame #f sieve-config-all-t)
                   empty)

      (ignore
       (define typed-sieve-prog-with-base
         (make-program (build-path sieve-path "typed" "main.rkt")
                       (list
                        (build-path sieve-path "typed" "streams.rkt")
                        ;; This is like a base module
                        "null.rkt"))))
      (test-equal? (follow-blame #f sieve-config-all-t)
                   empty))))

