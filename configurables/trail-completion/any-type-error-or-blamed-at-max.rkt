#lang at-exp racket

(require "../../configurations/config.rkt"
         "../../experiment/mutant-factory-data.rkt"
         "../../util/mutant-util.rkt"
         "../../runner/mutation-runner.rkt"
         "../../util/optional-contracts.rkt"
         (only-in "../../runner/mutation-runner-data.rkt" run-outcome/c))

(provide (contract-out [blame-trail-ended-normally?
                        (dead-mutant-process/c
                         blame-labels?
                         (symbol? string? . -> . any)
                         . -> .
                         boolean?)]))

(define/contract normal-trail-end-outcomes
  (listof run-outcome/c)
  '(type-error
    runtime-error
    blamed))

(define (blame-trail-ended-normally? dead-proc
                                     locations-selected-as-blamed
                                     log-factory-message)
  (match-define (dead-mutant-process (mutant _ mod index)
                                     config
                                     result
                                     id
                                     the-blame-trail
                                     _)
    dead-proc)

  (define normal-trail-outcome?
    (member (run-status-outcome result) normal-trail-end-outcomes))
  (define all-blamed-at-max-precision?
    ;; Note that this also says #t if there are no blamed locations at all
    (andmap (λ (blamed) (config-at-max-precision-for? blamed config))
            (filter (λ (mod) (hash-has-key? config mod))
                    locations-selected-as-blamed)))

  (define type-error?
    (equal? (run-status-outcome result)
            'type-error))

  (and normal-trail-outcome?
       (or type-error?
           all-blamed-at-max-precision?)))
