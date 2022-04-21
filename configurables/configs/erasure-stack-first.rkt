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
  ;; lltodo: make this db
  (configure! benchmark-runner         load-pre-computed-result "../dbs/code-mutations/pre-computed-mutant-results.rktdb")
  (configure! blame-following          pick-some
              ; runtime-error-with-blame
              select-top-of-context/filter-typed
              ; runtime-error
              select-top-of-context/filter-typed
              ; blame
              select-top-of-context/filter-typed)
  ;; lltodo: this is wrong, need to make the db to pre-select these guys too
  (configure! bt-root-sampling         subset-random-with-replacement
              config-has-both-sides-of-interface-untyped?)
  (configure! trail-completion         any-type-error/blamed-at-max)

  (configure! module-instrumentation   none)
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module))
