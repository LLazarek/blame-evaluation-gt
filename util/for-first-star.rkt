#lang at-exp racket

(provide for/first*
         for*/first*)

(require syntax/parse/define)

#;(define-simple-macro (for/first* (clause ...) body ...)
  (for/first (clause
              ...
              #:when #t
              [value (in-value (let () body ...))]
              #:when value)
    value))

(define-simple-macro (define-for/first*s normal-name nested-name)
  #:with [{for*?/first*? for*?/first} ...] (syntax/loc this-syntax
                                             [{normal-name for/first}
                                              {nested-name for*/first}])
  (begin
    (define-simple-macro (for*?/first*? (clause (... ...)) body (... ...))
      (for*?/first (clause
                    (... ...)
                    #:when #t
                    [value (in-value (let () body (... ...)))]
                    #:when value)
                   value))
    ...))

(define-for/first*s for/first* for*/first*)

(module+ test
  (require ruinit)
  (test-begin
    #:name for/first*
    (test-equal? (for/first* ([i (in-range 5)])
                             (if (< i 3) #f (* i 2)))
                 6)))
