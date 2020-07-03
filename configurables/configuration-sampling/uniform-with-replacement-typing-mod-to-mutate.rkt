#lang at-exp racket/base

(require "../../util/optional-contracts.rkt"
         "config-sampling-util.rkt")

(provide (contract-out [config-samples config-samples/c]))

;; Produce a list of `n` random samples (with replacement!) of the config whose
;; top element is `max-config`
;; Also only sample configs that type `module-to-mutate`.
(define (config-samples max-config n module-to-mutate)
  (for/list ([i (in-range n)])
    (hash-set (random-config-variant max-config)
              module-to-mutate
              'types)))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name config-samples
    (for/and/test
     ([i (in-range 50)])
     (define samples (config-samples
                      (hash "a.rkt" 'types
                            "b.rkt" 'types
                            "c.rkt" 'types
                            "main.rkt" 'types)
                      i
                      "b.rkt"))
     (and/test/message
      [(test-= (length samples) i)
       @~a{Wrong number of samples}]
      [(andmap (λ (config) (equal? (hash-ref config "b.rkt")
                                   'types))
               samples)
       @~a{Not all configs type "b.rkt"}]))))
