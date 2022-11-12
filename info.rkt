#lang info

(define test-omit-paths '("util/setup.rkt"
                          "mutation-analysis/plot-mutation-analyses.rkt"
                          "mutation-analysis/plot-new-mutation-analyses.rkt"
                          "mutation-analysis/categorize-mutants.rkt"
                          "data-analysis"
                          "configurables/code-mutation-configs"
                          "configurables/errortrace-configs"))
(define compile-omit-paths '("mutation-analysis/plot-mutation-analyses.rkt"
                             "mutation-analysis/plot-new-mutation-analyses.rkt"))
