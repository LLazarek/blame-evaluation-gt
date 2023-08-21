#lang configurable/config "../configurables.rkt"

(configure! mutation                 type-mistakes-in-code)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner         run-it)
(configure! configurations           module-types)

(configure! module-instrumentation   none)
(configure! program-instrumentation  just-instrument-modules)

(configure! bt-root-sampling         random-with-replacement)
