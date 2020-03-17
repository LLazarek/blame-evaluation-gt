#lang at-exp racket

(provide (all-defined-out))

(require racket/random)

(define config/c (hash/c string? any/c))

(define config-set hash-set)
(define config-ref hash-ref)

(define (config-at-max-precision-for? name config)
  (equal? (hash-ref config name) 'types))

(define (increment-config-precision-for name config)
  (match (hash-ref config name)
    ['none (hash-set config name 'types)]
    [else
     (error 'increment-config-precision-for
            @~a{Given config with value @name already at types: @config})]))

;; Produce a set of `n` random samples of the config whose top element is
;; `max-config`
(define (sample-config max-config n)
  (for/set ([i (in-range n)])
    (random-config-variant max-config)))

(define (random-config-variant a-config)
  (for/hash ([(mod mod-config) (in-hash a-config)])
    (values mod
            (for/hash ([(id _) (in-hash mod-config)])
              (values id
                      (random-ref '(none types)))))))
