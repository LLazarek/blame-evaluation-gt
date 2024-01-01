#lang at-exp racket/base

(provide (rename-out [experiment-modes modes]
                     [experiment-benchmarks benchmarks])
         scenario-samples-per-mutant
         configured:active-mutator-names
         benchmark-name->benchmark)

(require bex/configurables/configurables
         bex/configurations/configure-benchmark
         bex/orchestration/experiment-info
         racket/format)

(define (benchmark-name->benchmark name)
  (define b (read-benchmark (build-path benchmarks-dir name)))
  (unless b
    (error 'benchmark-name->benchmark
           @~a{
               Unable to find/read benchmark @name in currently-defined benchmarks directory @;
               @benchmarks-dir
               }))
  b)
