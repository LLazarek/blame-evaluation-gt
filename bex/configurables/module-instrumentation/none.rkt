#lang racket/base

(require "../../util/optional-contracts.rkt"
         "instrument-module.rkt")
(provide (contract-out
          [instrument-module module-instrumenter/c]))

(define (instrument-module a-mod)
  a-mod)
