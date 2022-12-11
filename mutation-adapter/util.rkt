#lang at-exp racket

(provide assert
         binding
         somewhere
         position?
         position-match?
         flip)

(require syntax/parse/define
         "../util/experiment-exns.rkt")

(define-simple-macro (assert c
                             {~alt {~optional message #:defaults ([message #'""])}
                                   {~optional {~seq #:name name} #:defaults ([name #'#f])}}
                             ...)
  (unless c
    (raise-internal-experiment-error (or name 'unknown-function) message)))

(define-match-expander binding
  (syntax-parser
    [(_ pat {~seq #:with [name:id val:expr]} ...)
     #'(and pat
            (app (const val) name)
            ...)]))
(define-match-expander somewhere
  (syntax-parser
    [(_ pats ...) #'(list _ ___ pats ... _ ___)]))

(define position? (or/c 'pos 'neg 'any))
(define (position-match? a b)
  (or (equal? b 'any) ;; common case, I hypothesize
      (equal? a b)
      (equal? a 'any)))
(define flip (match-lambda ['neg 'pos]
                           ['pos 'neg]))

