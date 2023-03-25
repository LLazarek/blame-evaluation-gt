#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 type-interface-mistakes)
(configure! mutant-sampling          pre-selected
            "../dbs/type-api-mutations/mutant-samples.rktdb")
(configure! mutant-filtering         select-type/runtime/ctc-erroring-max-config-mutants)
(configure! module-selection-for-mutation interface-module-only)
(configure! benchmark-runner         run-it)
(configure! interface-blame-translation
            to-module-that-declared-the-type)
(configure! blame-following          pick-some
            ; runtime-error-with-blame
            select-all-blamed/filter-library
            ; runtime-error
            select-top-of-context/filter-typed+library
            ; blame
            select-all-blamed/filter-library)
(configure! bt-root-sampling         pre-selected
            "../dbs/type-api-mutations/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
(configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module)
