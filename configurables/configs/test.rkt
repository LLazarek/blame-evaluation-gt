#lang racket/base

(require "../configurables.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! mutant-sampling          none)
(configure! blame-following          natural-blame)
(configure! stack-location-selection top)
(configure! module-instrumentation   none)
(configure! benchmark-runner         run-it)
(configure! bt-root-sampling         random-with-replacement)
(configure! trail-completion         mutated-type-error/blamed-at-max)
