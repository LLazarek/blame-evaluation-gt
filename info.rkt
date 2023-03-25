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
               "https://github.com/LLazarek/configurable.git"

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

(define all-setup-ignore-paths
  '("util/places-suck-demonstration.rkt"
    "util/mutation-index-cache-performance-check.rkt"
    "mutation-analysis/plot-mutation-analyses.rkt"
    "mutation-analysis/plot-new-mutation-analyses.rkt"
    #rx"configurables/.*-configs"
    "data-analysis"))

(define test-omit-paths (append '("util/setup.rkt")
                                all-setup-ignore-paths))
(define compile-omit-paths (append '("data-analysis/spot-checking.rkt")
                                   all-setup-ignore-paths))
