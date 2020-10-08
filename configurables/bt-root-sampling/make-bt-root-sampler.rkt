#lang at-exp racket/base

(provide make-bt-root-sampler/c)

(require racket/contract/base
         racket/math
         "../../configurations/config.rkt"
         "../../experiment/mutant-factory-data.rkt")

(define make-bt-root-sampler/c
  (bench-info/c mutant/c . -> . (natural? . -> . (listof config/c))))
