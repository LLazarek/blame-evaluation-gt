#lang at-exp racket/base

(provide log-mutate-info
         log-mutate-debug
         log-mutation-type
         mutate-logger)

(define-logger mutate)

(define (log-mutation-type type)
  (log-mutate-info "type: ~a" type))
