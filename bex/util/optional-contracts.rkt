#lang racket/base

(provide (rename-out
          [optional:define/contract define/contract]
          [optional:contract-out contract-out]))

(require (for-syntax racket/base
                     racket/provide-transform)
         racket/contract/base
         racket/contract/region
         syntax/parse/define)

(define-for-syntax ENABLE-CONTRACTS #f)

(define-simple-macro (optional:define/contract id/sig ctc . body)
  #:with [definer {~optional maybe-ctc}] (if ENABLE-CONTRACTS
                                             #'(define/contract ctc)
                                             #'(define))
  (definer id/sig {~? maybe-ctc} . body))


(define-syntax optional:contract-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ {~and clause {~or* [id _] [rename old-id new-id _]}} ...)
        (pre-expand-export
         (if ENABLE-CONTRACTS
             (syntax/loc this-syntax
               (contract-out clause ...))
             (syntax/loc this-syntax
               (contract-out {~? [id any/c]} ... {~? [rename old-id new-id any/c]} ...)))
         modes)]))))
