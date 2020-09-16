#lang at-exp racket

(require "benchmark-runner.rkt"
         "../../util/optional-contracts.rkt"
         "../../util/path-utils.rkt"
         "../../util/program.rkt"
         syntax/parse)

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c]))

(require "../../runner/instrumented-runner.rkt")
(define (make-benchmark-runner program mod-name index)
  run-with:require)

