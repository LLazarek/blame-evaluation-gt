#lang at-exp racket

(provide mutant-selector/c)

(require "../../util/path-utils.rkt"
         "../../configurations/configure-benchmark.rkt")

(define mutant-selector/c
  (module-name? benchmark/c . -> . (sequence/c natural?)))
