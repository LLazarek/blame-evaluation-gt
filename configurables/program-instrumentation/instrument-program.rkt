#lang racket/base

(provide instrument-program/c
         instrumented-program/c)

(require racket/contract/base
         "../../runner/instrumented-runner.rkt"
         "../../util/program.rkt")

(define instrumented-program/c
  (struct/c program resolved-module? (listof resolved-module?)))

(define instrument-program/c
  (program/c (mod/c . -> . resolved-module?) . -> . instrumented-program/c))

