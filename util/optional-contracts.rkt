#lang racket/base

(provide define/contract)

(require syntax/parse/define
         (prefix-in r/c: racket/contract)
         (for-syntax racket/base))

(define-for-syntax ENABLE-CONTRACTS #f)

(define-simple-macro (define/contract id/sig ctc . body)
  #:with [definer {~optional maybe-ctc}]
  (if ENABLE-CONTRACTS
      #'(r/c:define/contract ctc)
      #'(define))
  (definer id/sig {~? maybe-ctc} . body))
