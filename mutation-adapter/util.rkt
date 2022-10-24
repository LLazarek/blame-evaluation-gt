#lang at-exp racket

(provide assert
         binding
         position?
         position-match?
         flip)

(require syntax/parse/define)

(define-simple-macro (assert c
                             {~alt {~optional message #:defaults ([message #'""])}
                                   {~optional {~seq #:name name} #:defaults ([name #'#f])}}
                             ...)
  (unless c
    (error (or name 'unknown-function) message)))

(define-match-expander binding
  (syntax-parser
    [(_ pat {~seq #:with [name:id val:expr]} ...)
     #'(and pat
            (app (const val) name)
            ...)]))

(define position? (or/c 'pos 'neg 'any))
(define (position-match? a b)
  (or (equal? b 'any) ;; common case, I hypothesize
      (equal? a b)
      (equal? a 'any)))
(define flip (match-lambda ['neg 'pos]
                           ['pos 'neg]))

