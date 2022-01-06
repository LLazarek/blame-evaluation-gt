#lang at-exp racket/base

(provide log-mutate-info
         log-mutate-debug
         log-mutation-type
         log-mutation
         mutate-logger

         log-prefix:mutating
         log-prefix:mutation-type)

(require racket/format)

(define-logger mutate)

(define log-prefix:mutation-type "type:")
(define log-prefix:mutating "Mutating")

(define (log-mutation-type type)
  (log-mutate-info @~a{@log-prefix:mutation-type ~a} type))

(define (log-mutation before after)
  (log-message mutate-logger
               'info
               @~a{@log-prefix:mutating @before -> @after}
               (list before after)))
