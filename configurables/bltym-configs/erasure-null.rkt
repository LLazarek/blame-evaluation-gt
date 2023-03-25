#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 type-interface-mistakes)
(configure! mutant-sampling          pre-selected
            "../dbs/type-api-mutations/mutant-samples.rktdb")
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation interface-module-only)
(configure! benchmark-runner         load-pre-computed-result "../dbs/type-api-mutations/pre-computed-mutant-results.rktdb")
(configure! blame-following          null)
(configure! bt-root-sampling         pre-selected
            "../dbs/type-api-mutations/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
;; We need to insert the TR adapter module to check for a type error, but we
;; won't use that TR adapter to run since we're loading pre-computed results anyway.
(configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module)
