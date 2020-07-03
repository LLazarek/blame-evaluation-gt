#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c]))

(require "../../runner/instrumented-runner.rkt")

;; Don't run the program at all. Useful if all you want to do is typecheck it.
(define (make-benchmark-runner . _)
  void)
