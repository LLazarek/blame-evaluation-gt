#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-mistakes-in-code)
  (configure! mutant-sampling          use-pre-selected-samples "../dbs/code-mutations/mutant-samples.rktdb")
  (configure! module-instrumentation   none)
  (configure! benchmark-runner         run-it)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-errortrace/context-fallback
              ; runtime-error
              select-all-errortrace/context-fallback
              ; blame
              select-all-errortrace/context-fallback)
  (configure! bt-root-sampling         random-with-replacement)
  (configure! trail-completion         mutated-type-error/blamed-at-max))
