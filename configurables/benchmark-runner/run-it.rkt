#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c]))

(require "../../runner/instrumented-runner.rkt")
(define (make-benchmark-runner . _)
  run-with:require)
