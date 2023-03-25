#lang configurable/config "../configurables.rkt"

(configure! mutation                 type-interface-mistakes)
(configure! mutant-sampling          pre-selected
            "../dbs/type-api-mutations/mutant-samples.rktdb")
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation interface-module-only)
(configure! benchmark-runner         run-it)
(configure! interface-blame-translation
            to-value-source)
(configure! blame-following          null)
(configure! bt-root-sampling         pre-selected
            "../dbs/type-api-mutations/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
(configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module)
