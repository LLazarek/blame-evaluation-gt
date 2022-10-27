#lang racket/base

(require "../configurables.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! module-selection-for-mutation interface-module-only)
  (configure! benchmark-runner         run-it)
  (configure! configurations           module-types)

  (configure! module-instrumentation   erasure-types)
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module)

  (configure! bt-root-sampling         random-with-replacement))
