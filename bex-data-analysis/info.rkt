#lang info

(define collection "bex-data-analysis")
(define build-deps '("racket-doc"
                     "scribble-lib"
                     "at-exp-lib"))
(define deps '("base"
               "bex"
               ;; "https://github.com/LLazarek/configurable.git?path=configurable"

               "https://github.com/LLazarek/rscript.git"
               "https://github.com/LLazarek/process-queue.git?path=process-queue"
               "https://github.com/LLazarek/ruinit.git"
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

(define test-omit-paths 'all)
(define compile-omit-paths '("spot-checking.rkt"))
