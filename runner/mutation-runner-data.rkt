#lang at-exp racket/base

(provide (struct-out run-status)
         outcomes
         index-exceeded-outcome
         index-exceeded?
         run-status/c
         run-outcome/c)

(require racket/contract
         racket/match
         racket/math
         "../util/path-utils.rkt")

(define index-exceeded-outcome 'index-exceeded)
(define outcomes `(,index-exceeded-outcome
                   blamed
                   runtime-error
                   type-error
                   oom
                   timeout
                   syntax-error
                   completed))

(struct run-status (mutated-module
                    index
                    mutated-id
                    outcome
                    blamed
                    result-value)
  #:prefab)

(define module-name-or-library-path?
  (or/c module-name?
        ;; This case allows the benchmarks to
        ;; blame library code, which happens for
        ;; instance in the quadU mutant
        ;; "quad-main.rkt" @ 79
        library-path?))

(define run-outcome/c (apply or/c outcomes))

(define run-status/c
  (struct/dc run-status
             [mutated-module  module-name?]
             [index           natural?]
             [mutated-id      {outcome}
                              (if (equal? outcome index-exceeded-outcome)
                                  #f
                                  symbol?)]
             [outcome         run-outcome/c]
             [blamed          {outcome}
                              (match outcome
                                ['blamed
                                 ;; This should really be `non-empty-listof`,
                                 ;; but one limitation of transient is that
                                 ;; sometimes it raises a blame error blaming
                                 ;; nothing
                                 (listof module-name-or-library-path?)]
                                ['type-error
                                 (list/c module-name-or-library-path?)]
                                ['runtime-error
                                 (listof module-name-or-library-path?)]
                                [else
                                 any/c])]
             [result-value    any/c]))

;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) index-exceeded-outcome))
