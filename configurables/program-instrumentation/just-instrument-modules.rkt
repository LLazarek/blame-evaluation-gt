#lang at-exp racket/base

(require racket/contract/base
         racket/match
         "../../util/program.rkt"
         "instrument-program.rkt")

(provide (contract-out [instrument-program instrument-program/c]))

(define (instrument-program a-program make-instrumented-module)
  (match-define (program main-module other-modules-to-instrument)
    a-program)
  (define main/instrumented (make-instrumented-module main-module))
  (define others/instrumented
    (map make-instrumented-module other-modules-to-instrument))
  (program main/instrumented others/instrumented))
