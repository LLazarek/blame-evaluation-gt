#lang at-exp racket

(provide assert
         binding)

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
