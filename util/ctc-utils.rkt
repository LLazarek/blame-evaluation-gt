#lang at-exp racket

(provide (all-defined-out))

(define (->bool x)
  (and x #t))

(define (simple-flat-contract-with-explanation pred
                                               expected-msg)
  (flat-contract-with-explanation
   (λ (val)
     (or (->bool (pred val))
         (λ (blame)
           (raise-blame-error blame val
                              (list 'expected:
                                    expected-msg
                                    'given:
                                    "~e")
                              val))))))
