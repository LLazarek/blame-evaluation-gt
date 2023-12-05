#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! mutant-sampling          pre-selected
            "../dbs/code-mutations/mutant-samples.rktdb")
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! blame-translation
            TR-interface-to-value-source)
(configure! blame-following          null)
(configure! bt-root-sampling         pre-selected
            "../dbs/code-mutations/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)
