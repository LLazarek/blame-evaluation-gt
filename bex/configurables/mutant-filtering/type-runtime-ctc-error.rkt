#lang at-exp racket/base

(require "mutant-filter.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [should-sample-mutant-blame-trails? mutant-filter/c]))

(define (should-sample-mutant-blame-trails? outcome)
  (and (member outcome '(blamed runtime-error type-error)) #t))
