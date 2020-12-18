#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [rename make-no-op-benchark-runner
                               make-benchmark-runner
                               make-benchmark-runner/c]))

;; Don't run the program at all. Useful if all you want to do is typecheck it.
(define (make-no-op-benchark-runner . _)
  void)
