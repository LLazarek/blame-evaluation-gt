#lang at-exp racket/base

(require "../configurables/configurables.rkt")

(provide (all-defined-out))

(define config/c hash?)

(define (config-at-max-precision-for? name config)
  ((configured:config-at-max-precision-for?) name config))

(define (increment-config-precision-for-all names config
                                            #:increment-max-error?
                                            [error-if-already-max? #t])
  (for/fold ([config config])
            ([name (in-list names)])
    ((configured:increment-config-precision-for) name config
                                                 #:increment-max-error? error-if-already-max?)))

(define (serialize-config config)
  ((configured:serialize-config) config))
(define (deserialize-config config-number
                            #:reference [reference-config #f]
                            #:modules [mods (hash-keys reference-config)])
  ((configured:deserialize-config) config-number
                                   #:reference reference-config
                                   #:modules mods))

