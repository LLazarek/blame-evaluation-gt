#lang info

(define collection "blame-evaluation")
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "at-exp-lib"))
(define deps '("base"
               "custom-load"
               "errortrace-lib"
               "rackunit-lib"
               "require-typed-check"
               "sandbox-lib"
               "typed-racket-lib"
               "https://github.com/LLazarek/rscript.git"
               "https://github.com/LLazarek/ruinit.git"
               "https://github.com/LLazarek/process-queue.git"
               "https://github.com/LLazarek/mutate.git"

               ;; data analysis
               "complot"
               "data-frame"
               "db-lib"
               "pict-lib"
               "pict-util"
               "plot-gui-lib"
               "plot-lib"
               "plot-util"
               "text-table"))

(define scribblings '())

(define test-omit-paths '("util/setup.rkt"
                          "mutation-analysis/plot-mutation-analyses.rkt"
                          "mutation-analysis/plot-new-mutation-analyses.rkt"
                          "mutation-analysis/categorize-mutants.rkt"
                          "data-analysis"
                          "configurables/code-mutation-configs"
                          "configurables/errortrace-configs"))
(define compile-omit-paths '("mutation-analysis/plot-mutation-analyses.rkt"
                             "mutation-analysis/plot-new-mutation-analyses.rkt"
                             "data-analysis/spot-checking.rkt"))
