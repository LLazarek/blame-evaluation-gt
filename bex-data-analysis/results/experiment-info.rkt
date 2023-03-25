#lang racket/base

(provide (rename-out [experiment-modes modes]
                     [experiment-benchmarks benchmarks])
         scenario-samples-per-mutant
         configured:active-mutator-names
         benchmark-name->benchmark)

(require bex/configurables/configurables
         bex/configurations/configure-benchmark
         bex/orchestration/experiment-info)

(define (benchmark-name->benchmark name)
  (read-benchmark (build-path benchmarks-dir name)))
