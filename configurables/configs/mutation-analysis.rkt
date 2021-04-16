#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-mistakes-in-code)
  (configure! mutant-sampling          none)
  (configure! module-instrumentation   none)
  (configure! benchmark-runner         none)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-blamed
              ; runtime-error
              select-top-of-context/filter-typed
              ; blame
              select-all-blamed)
  (configure! bt-root-sampling         random-with-replacement)
  (configure! trail-completion         any-type-error/blamed-at-max))
