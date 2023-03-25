#lang racket

(provide module-selector/c)

(require "../../configurations/configure-benchmark.rkt")

(define module-selector/c (benchmark/c . -> . (listof string?)))
