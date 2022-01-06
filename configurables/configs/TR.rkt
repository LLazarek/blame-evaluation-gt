#lang racket

(require "../configurables.rkt"
         "blame-following-common.rkt"
         "bt-root-sampling-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! mutant-sampling          none)
  (configure! module-instrumentation   none)
  (configure! benchmark-runner         run-it)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-blamed
              ; runtime-error
              select-top-of-context/filter-typed
              ; blame
              select-all-blamed)
  (configure! bt-root-sampling         subset-random-without-replacement
              config-has-both-sides-of-interface-untyped?)
  (configure! trail-completion         any-type-error/blamed-at-max)

  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module))
