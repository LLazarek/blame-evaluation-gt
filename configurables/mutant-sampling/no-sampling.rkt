#lang at-exp racket/base

(require "../../util/optional-contracts.rkt"
         "mutant-selector.rkt")
(provide (contract-out [select-mutants mutant-selector/c]
                       [all-mutants-should-have-trails? boolean?]))

(require "../../util/mutant-util.rkt")

(define (select-mutants module-to-mutate-name bench)
  (in-mutation-indices module-to-mutate-name
                       bench))

(define all-mutants-should-have-trails? #f)
