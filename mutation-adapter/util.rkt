#lang at-exp racket

(provide assert)

(require syntax/parse/define)

(define-simple-macro (assert c
                             {~alt {~optional message #:defaults ([message #'""])}
                                   {~optional {~seq #:name name} #:defaults ([name #'#f])}}
                             ...)
  (unless c
    (error (or name 'unknown-function) message)))
