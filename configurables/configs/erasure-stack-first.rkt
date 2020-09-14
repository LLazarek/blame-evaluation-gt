#lang racket/base

(require "../configurables.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! mutant-sampling          use-pre-selected-samples "../dbs/code-mutations/mutant-samples.rktdb")
(configure! blame-following          stack)
(configure! stack-location-selection top)
(configure! module-instrumentation   none)
(configure! benchmark-runner         load-pre-computed-result "../dbs/code-mutations/pre-computed-mutant-results.rktdb")
(configure! configuration-sampling   uniform-with-replacement)
(configure! trail-completion         mutated-type-error/blamed-at-max)
