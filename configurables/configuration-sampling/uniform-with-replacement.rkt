#lang at-exp racket/base

(require "../../util/optional-contracts.rkt"
         "config-sampling-util.rkt")

(provide (contract-out [config-samples config-samples/c]))

;; Produce a list of `n` random samples (with replacement!) of the config whose
;; top element is `max-config`
;; Ignores which module is to be mutated.
(define (config-samples max-config n module-to-mutate)
  (for/list ([i (in-range n)])
    (random-config-variant max-config)))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name config-samples
    (for/and/test ([i (in-range 50)])
                  (test-= (length (config-samples
                                   (hash "a.rkt" 'types
                                         "b.rkt" 'types
                                         "c.rkt" 'types
                                         "main.rkt" 'types)
                                   i
                                   "b.rkt"))
                          i))))
