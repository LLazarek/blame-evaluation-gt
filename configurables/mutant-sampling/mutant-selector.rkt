#lang at-exp racket

(provide mutant-selector/c)

(require "../../util/path-utils.rkt"
         "../../configurations/configure-benchmark.rkt")

(define mutant-selector/c
  (case-> (module-name? benchmark/c . -> . (sequence/c natural?))
          (module-name? benchmark/c any/c . -> . (sequence/c natural?))))
