#lang at-exp racket/base

(require racket/contract/base
         racket/format
         racket/match)

(provide (all-defined-out))

(define config/c (hash/c string? any/c))

(define config-set hash-set)
(define config-ref hash-ref)

(define (config-at-max-precision-for? name config)
  (equal? (hash-ref config name) 'types))

(define (increment-config-precision-for name config
                                        #:increment-types-error?
                                        [error-if-already-types? #t])
  (match (hash-ref config name)
    ['none (hash-set config name 'types)]
    [else
     #:when error-if-already-types?
     (error 'increment-config-precision-for
            @~a{Given config with value @name already at types: @config})]
    [else config]))
(define (increment-config-precision-for-all names config
                                            #:increment-types-error?
                                            [error-if-already-types? #t])
  (for/fold ([config config])
            ([name (in-list names)])
    (increment-config-precision-for name config
                                    #:increment-types-error? error-if-already-types?)))

(module+ test
  (require ruinit
           racket)

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
           "bazzle.rkt" 'types))))
