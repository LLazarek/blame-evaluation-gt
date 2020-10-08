#lang at-exp racket/base

(provide random-config-variant)

(require racket/random)

(define (random-config-variant a-config)
  (for/hash ([(mod mod-config) (in-hash a-config)])
    (values mod
            (random-ref '(none types)))))
