#lang racket/base

(provide mutant-filter/c)

(require "../../runner/mutation-runner-data.rkt"
         racket/contract/base)

(define mutant-filter/c (run-outcome/c . -> . boolean?))
