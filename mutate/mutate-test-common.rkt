#lang at-exp racket/base

(provide programs-equal?
         test-programs-equal?
         test-mutator
         test-mutator*)

(require racket/format
         racket/pretty
         ruinit
         ruinit/diff/diff
         "mutated.rkt")

(define (programs-equal? a b)
  (equal? (syntax->datum a)
          (syntax->datum b)))

(define (diff-programs/string actual expected)
  (dumb-diff-lines/string (pretty-format (syntax->datum actual))
                          (pretty-format (syntax->datum expected))))

(define-simple-test (test-programs-equal? actual expected)
  #:fail-message @~a{
                     Programs are not equal. Diff (expected <):
                     @(diff-programs/string expected actual)
                     }
  (programs-equal? actual expected))


(define-test (test-mutator mutator orig-stx expected-stx
                           [counter-index-offset 0])
  (test-programs-equal? (mutated-stx (mutator orig-stx counter-index-offset 0))
                        expected-stx))

(define-test (test-mutator* mutator
                            orig-stx
                            expected-stx-seq)
  (for/and/test ([expected-stx (in-list expected-stx-seq)]
                 [counter (in-naturals)])
                (test-mutator mutator
                              orig-stx
                              expected-stx
                              counter)))
