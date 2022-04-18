#lang at-exp racket

(provide config-has-both-sides-of-interface-untyped?)

(require "../../experiment/mutant-factory-data.rkt"
         "../../configurations/configure-benchmark.rkt")

(define ((benchmark-named name) a-benchmark)
  (equal? (benchmark->name (bench-info-benchmark a-benchmark)) name))
(define (untyped? config key) (equal? (hash-ref config key) 'none))

(define config-has-both-sides-of-interface-untyped?
  (match-lambda** [{(? (benchmark-named "sieve")) config}
                   (and (untyped? config "main.rkt")
                        (untyped? config "streams.rkt"))]
                  [{(app (compose1 benchmark->name bench-info-benchmark) name) config}
                   (error 'config-has-both-sides-of-interface-untyped?
                          @~a{not implemented for benchmark @name})]))
