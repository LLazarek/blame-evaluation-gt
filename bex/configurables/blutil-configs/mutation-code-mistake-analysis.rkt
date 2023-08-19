#lang configurable/config "../configurables.rkt"

(require "blame-following-common.rkt"
	 "bt-root-sampling-common.rkt")

(configure! mutation code-mistakes)
(configure! module-selection-for-mutation all-regular-modules)
(configure! benchmark-runner run-it)
(configure! configurations module-export-ctcs)
(configure! module-instrumentation add-ctc-level)
(configure! program-instrumentation just-instrument-modules)