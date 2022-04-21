#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt"
         "bt-root-sampling-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! mutant-sampling          none)
  (configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
  (configure! module-selection-for-mutation interface-module-only)
  (configure! benchmark-runner         run-it)
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-all-blamed
              ; runtime-error
              select-top-of-context/filter-typed
              ; blame
              select-first-blamed-pair)
  ;; lltodo
  (configure! bt-root-sampling         pre-selected "../dbs/code-mutations/pre-selected-bt-roots.rktdb")
  (configure! trail-completion         any-type-error/blamed-at-max)

  (configure! module-instrumentation   transient-types "../dbs/code-mutations/transient-special-cases.rktdb")
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module))
