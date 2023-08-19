#lang at-exp racket

(require syntax/parse
         syntax/parse/define
         (for-syntax syntax/parse)
         "../../util/program.rkt"
         "instrument-module.rkt")

(provide (contract-out
          [instrument-module module-instrumenter/c]))


(define (instrument-module a-mod)
  (define instrumented-stx
    (add-ctc-level
     (mod-stx a-mod)))
  (struct-copy mod a-mod
               [stx instrumented-stx]))

(define (add-ctc-level stx)
  (syntax-parse stx
    [(module name lang
                      (#%module-begin
                       body ...))
     #'(module name lang
         (#%module-begin
          (define-syntax ctc-level 'none)
          body ...))]))

(module+ test
  (require racket
           rackunit)
  (check-equal? (syntax->datum (add-ctc-level #'(module add-two-numbers racket
                                                  (#%module-begin
                                                   (define (add-em x y)
                                                     (+ x y))
                                                   (add-em 5 4)))))
                '(module add-two-numbers racket
                   (#%module-begin
                    (define-syntax ctc-level 'none)
                    (define (add-em x y)
                      (+ x y))
                    (add-em 5 4))))
  (check-equal? (syntax->datum (add-ctc-level #'(module mod racket
                                                  (#%plain-module-begin
                                                   5))))
                '(module mod racket
                   (#%plain-module-begin
                    (define-syntax ctc-level 'none)
                    5))))
