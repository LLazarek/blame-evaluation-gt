#lang racket/base

(provide instrument-program/c)

(require racket/contract/base
         "../../runner/instrumented-runner.rkt"
         "../../util/program.rkt")

(define instrumented-program/c
  (struct/c program instrumented-module? (listof instrumented-module?)))

(define instrument-program/c
  (program/c (mod/c . -> . instrumented-module?) . -> . instrumented-program/c))

