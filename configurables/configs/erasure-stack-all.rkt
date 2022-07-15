#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! mutant-sampling          pre-selected
              "../dbs/type-api-mutations/mutant-samples.rktdb")
  (configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
  (configure! module-selection-for-mutation interface-module-only)
  (configure! benchmark-runner         load-pre-computed-result "../dbs/type-api-mutations/pre-computed-mutant-results.rktdb")
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-context
              ; runtime-error
              select-all-context
              ; blame
              select-all-context)
  (configure! bt-root-sampling         pre-selected
              "../dbs/type-api-mutations/pre-selected-bt-roots.rktdb")
  (configure! trail-completion         any-type-error/blamed-at-max)
  (configure! configurations           module-types)

  (configure! module-instrumentation   none)
  ;; We need to insert the TR adapter module to check for a type error, but we
  ;; won't use that TR adapter to run since we're loading pre-computed results anyway.
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module))
