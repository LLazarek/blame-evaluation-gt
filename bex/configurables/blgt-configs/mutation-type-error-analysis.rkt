#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt"
         "bt-root-sampling-common.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)

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
