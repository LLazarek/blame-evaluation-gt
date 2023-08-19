#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 code-mistakes)
(configure! mutant-sampling          pre-selected
            "../dbs/blutil/mutant-samples.rktdb")
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! interface-blame-translation
            to-value-source)
(configure! blame-following          pick-some
            ; runtime-error-with-blame
            select-top-of-context/filter-max
            ; runtime-error
            select-top-of-context/filter-max
            ; blame
            select-top-of-context/filter-max)
(configure! bt-root-sampling         pre-selected
            "../dbs/blutil/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-export-ctcs)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)
