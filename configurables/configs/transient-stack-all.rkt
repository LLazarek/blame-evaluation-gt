#lang racket/base

(require "../configurables.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! mutant-sampling          use-pre-selected-samples "../dbs/code-mutations/mutant-samples.rktdb")
(configure! blame-following          stack)
(configure! stack-location-selection all)
(configure! module-instrumentation   transient-types "../dbs/code-mutations/transient-special-cases.rktdb")
(configure! benchmark-runner         run-it)
(configure! configuration-sampling   uniform-with-replacement)
(configure! trail-completion         mutated-type-error/blamed-at-max)
