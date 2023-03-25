#lang at-exp racket/base

(provide make-benchmark-runner/c)

(require racket/contract/base
         racket/math
         "../../util/program.rkt"
         "../../util/path-utils.rkt")

(define benchmark-runner/c
  ((list/c 'file path-string?) . -> . any/c))

(define make-benchmark-runner/c
  (program/c module-name? natural? . -> . benchmark-runner/c))
