#lang racket/base

(require racket/contract/base
         "module-selector.rkt"
         "../../configurations/configure-benchmark.rkt")

(provide (contract-out [select-modules-to-mutate module-selector/c]))

(define (select-modules-to-mutate bench)
  (benchmark->mutatable-modules bench))
