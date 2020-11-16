#lang racket/base

(provide absent
         absent?
         option/c
         in-option
         for*/option
         option-let*)

(require racket/contract/base
         racket/list
         racket/match
         syntax/parse/define
         (for-syntax racket/base))

(define-values {absent absent?}
  (let ()
    (struct absent ())
    (values (absent) absent?)))
(define (option/c inner/c)
  (or/c absent? inner/c))

(define (in-option o)
  (in-list (match o
             [(? absent?) empty]
             [else (list o)])))
(define-simple-macro (for*/option clauses body ...)
  (let/ec return
    (for* clauses
      (return (let () body ...)))
    absent))
(define-simple-macro (option-let* ([name maybe-option] more ...) body ...)
  #:with result-expr (if (null? (attribute more))
                         #'(let () body ...)
                         #'(option-let* (more ...) body ...))
  (let ([name maybe-option])
    (if (absent? name)
        absent
        result-expr)))
