#lang at-exp racket/base

(require "../../experiment/mutant-factory-data.rkt"
         "../../util/optional-contracts.rkt"
         "make-bt-root-sampler.rkt"
         "random-config.rkt"
         racket/random)

(provide (contract-out [make-bt-root-sampler make-bt-root-sampler/c]))


(define ((make-bt-root-sampler bench-info mutant) n)
  (define max-config (bench-info-max-config bench-info))
  (for/list ([i (in-range n)])
    (random-config-variant max-config)))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name make-bt-root-sampler
    (for/and/test ([i (in-range 50)])
                  (test-= (length ((make-bt-root-sampler (bench-info #f
                                                                (hash "a.rkt" 'types
                                                                      "b.rkt" 'types
                                                                      "c.rkt" 'types
                                                                      "main.rkt" 'types))
                                                    #f)
                                   i))
                          i))))
