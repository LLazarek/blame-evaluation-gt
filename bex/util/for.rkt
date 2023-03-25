#lang at-exp racket

(provide (rename-out [for/or for/first*]
                     [for*/or for*/first*])
         for/hash/fold)

(require syntax/parse/define)

(define-simple-macro (for/hash/fold for-clauses
                       {~optional {~seq #:init initial-hash}
                                  #:defaults ([initial-hash #'(hash)])}
                       #:combine combine
                       #:default default
                       body ...)
  (for/fold ([result-hash initial-hash])
            for-clauses
    (define-values {key value} (let () body ...))
    (hash-update result-hash
                 key
                 (λ (accumulator) (combine value accumulator))
                 default)))
