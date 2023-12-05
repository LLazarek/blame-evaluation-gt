#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt")

(configure! mutation                 code-mistakes)
(configure! mutant-sampling          pre-selected
            "../dbs/blutil/mutant-samples.rktdb")
(configure! mutant-filtering         none)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! blame-translation
            configurable-ctc-middleman-mod-to-source)
(configure! blame-following          null)
(configure! bt-root-sampling         pre-selected
            "../dbs/blutil/pre-selected-bt-roots.rktdb")
(configure! trail-completion         any-type-error/blamed-at-max)
(configure! configurations           module-export-ctcs)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)
