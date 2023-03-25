#lang racket

(require "../configurables.rkt"
         "blame-following-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-mistakes-in-code)
  (configure! mutant-sampling          none)
  (configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
  (configure! module-selection-for-mutation all-regular-modules)
  (configure! benchmark-runner         run-it)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-top-of-context/filter-max
              ; runtime-error
              select-top-of-context/filter-max
              ; blame
              select-top-of-context/filter-max)
  (configure! bt-root-sampling         random-with-replacement)
  (configure! trail-completion         any-type-error/blamed-at-max)

  (configure! module-instrumentation   none)
  (configure! program-instrumentation  just-instrument-modules)

  (configure! configurations           module-export-ctcs))
