#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 code-mistakes)
(configure! mutant-sampling          none)
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! interface-blame-translation
            to-value-source)
(configure! blame-following          pick-some
            ; runtime-error-with-blame
            select-all-blamed
            ; runtime-error
            select-top-of-context/filter-max
            ; blame
            select-all-blamed)
(configure! bt-root-sampling         random-with-replacement)
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-export-ctcs)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)
