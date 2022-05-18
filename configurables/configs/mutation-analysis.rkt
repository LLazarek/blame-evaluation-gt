#lang racket/base

(require "../configurables.rkt"
         "blame-following-common.rkt"
         "bt-root-sampling-common.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! module-selection-for-mutation interface-module-only)
  (configure! benchmark-runner         run-it)

  (configure! module-instrumentation   none)
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module)

  ;;;; These don't matter for mutation analysis
  ;; (configure! mutant-sampling          none)
  ;; (configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
  ;; (configure! bt-root-sampling         random-with-replacement)
  ;; (configure! blame-following          pick-some
  ;;             ; runtime-error-with-blame
  ;;             select-all-blamed
  ;;             ; runtime-error
  ;;             select-top-of-context/filter-typed
  ;;             ; blame
  ;;             select-all-blamed)
  ;; (configure! trail-completion         any-type-error/blamed-at-max)
  )
