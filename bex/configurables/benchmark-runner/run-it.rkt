#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [rename make-require-benchmark-runner
                               make-benchmark-runner
                               make-benchmark-runner/c]))

(require "../../runner/instrumented-runner.rkt")
(define (make-require-benchmark-runner program mod-name index)
  run-with:require)

