#lang racket/base

(require "../configurables.rkt")

(provide install!)

(define (install!)
  (configure! mutation                 type-interface-mistakes)
  (configure! benchmark-runner         run-it)
  (configure! bt-root-sampling         random-with-replacement)
  (configure! configurations           module-types)
  (configure! module-instrumentation   erasure-types)
  (configure! program-instrumentation  instrument-modules-and-insert-interface-adapter-module))
