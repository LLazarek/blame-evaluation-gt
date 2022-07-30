#lang racket/base

(provide (rename-out [experiment-modes modes]
                     [experiment-benchmarks benchmarks])
         scenario-samples-per-mutant
         configured:active-mutator-names
         benchmark-name->benchmark)

(require "../configurables/configurables.rkt"
         "../configurations/configure-benchmark.rkt"
         "../orchestration/experiment-info.rkt")

(define (benchmark-name->benchmark name)
  (read-benchmark (build-path benchmarks-dir name)))
