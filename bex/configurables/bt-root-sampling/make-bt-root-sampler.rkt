#lang at-exp racket/base

(provide make-bt-root-sampler/c
         root-missing-blame-response/c)

(require racket/contract/base
         racket/math
         "../../configurations/config.rkt"
         "../../experiment/mutant-factory-data.rkt"
         "../../runner/mutation-runner-data.rkt")

(define make-bt-root-sampler/c
  (bench-info/c mutant/c . -> . (natural? . -> . (listof config/c))))

(define root-missing-blame-response/c
  (run-outcome/c . -> . (or/c 'bt-failed
                              'resample
                              'error)))
