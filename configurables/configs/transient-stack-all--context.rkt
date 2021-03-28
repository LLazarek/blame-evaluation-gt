#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-mistakes-in-code)
  (configure! mutant-sampling          use-pre-selected-samples "../dbs/code-mutations/mutant-samples.rktdb")
  (configure! module-instrumentation   transient-types "../dbs/code-mutations/transient-special-cases.rktdb")
  (configure! benchmark-runner         run-it)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-context
              ; runtime-error
              select-all-context
              ; blame
              select-all-context)
  (configure! bt-root-sampling         pre-selected "../dbs/code-mutations/pre-selected-bt-roots.rktdb")
  (configure! trail-completion         mutated-type-error/blamed-at-max))
