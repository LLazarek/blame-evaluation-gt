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

;; Produce a list of `n` random samples (with replacement!) of the config whose
;; top element is `max-config`
(define (sample-config max-config n)
  (for/list ([i (in-range n)])
    (random-config-variant max-config)))

(define (random-config-variant a-config)
  (for/hash ([(mod mod-config) (in-hash a-config)])
    (values mod
            (random-ref '(none types)))))

(module+ test
  (require ruinit)

  (test-begin
    #:name test:increment-config-precision-for
    (test-equal? (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'none
                        "bazzle.rkt" 'types))
                 (hash "baz.rkt" 'types
                       "bazzle.rkt" 'types))
    (test-exn exn:fail?
              (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'types
                        "bazzle.rkt" 'types))))

  (test-begin
    #:name config-at-max-precision-for?
    (not/test (config-at-max-precision-for?
               "baz.rkt"
               (hash "baz.rkt" 'none
                     "bazzle.rkt" 'types)))
    (config-at-max-precision-for?
     "baz.rkt"
     (hash "baz.rkt" 'types
           "bazzle.rkt" 'types)))

  (test-begin
    #:name sample-config
    (for/and/test ([i (in-range 50)])
                  (test-= (length (sample-config
                                   (hash "a.rkt" 'types
                                         "b.rkt" 'types
                                         "c.rkt" 'types
                                         "main.rkt" 'types)
                                   i))
                          i))))
