#lang racket/base

(require "../configurables.rkt")

(configure! mutation                 type-mistakes-in-code)
(configure! mutant-sampling          use-pre-selected-samples "../dbs/code-mutations/mutant-samples.rktdb")
(configure! blame-following          stack)
(configure! stack-location-selection top)
(configure! module-instrumentation   none)
(configure! benchmark-runner         run-it)
(configure! bt-root-sampling         pre-selected "../dbs/code-mutations/pre-selected-bt-roots.rktdb")
(configure! trail-completion         mutated-type-error/blamed-at-max)
