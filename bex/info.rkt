#lang info

(define collection "bex") ; Blame Evaluation eXperiment
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
               "https://github.com/LLazarek/process-queue.git?path=process-queue"
               "https://github.com/LLazarek/mutate.git?path=mutate"
               "https://github.com/LLazarek/configurable.git?path=configurable"))

(define scribblings '())

(define all-setup-ignore-paths
  '("util/places-suck-demonstration.rkt"
    "util/mutation-index-cache-performance-check.rkt"
    "mutation-analysis/plot-mutation-analyses.rkt"
    "mutation-analysis/plot-new-mutation-analyses.rkt"
    #rx"configurables/.*-configs"))

(define test-omit-paths (append '("util/setup.rkt")
                                all-setup-ignore-paths))
(define compile-omit-paths all-setup-ignore-paths)
