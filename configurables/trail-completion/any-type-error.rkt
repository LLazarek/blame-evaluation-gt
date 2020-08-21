#lang at-exp racket/base

(require racket/match
         "../../configurations/config.rkt"
         "../../experiment/mutant-factory-data.rkt"
         "../../util/mutant-util.rkt"
         "../../runner/mutation-runner.rkt"
         "../../util/optional-contracts.rkt")

(provide (contract-out [blame-trail-ended?
                        (dead-mutant-process/c
                         blame-labels?
                         (symbol? string? . -> . any)
                         . -> .
                         boolean?)]))

(define (blame-trail-ended? dead-proc
                            blamed/type-error-locations
                            log-factory-message)
  (match-define (dead-mutant-process (mutant _ mod index)
                                     config
                                     result
                                     id
                                     the-blame-trail
                                     _)
    dead-proc)

  (define all-blamed-at-max-precision?
    (andmap (λ (blamed) (config-at-max-precision-for? blamed config))
            blamed/type-error-locations))

  (define type-error?
    (equal? (run-status-outcome (dead-mutant-process-result dead-proc))
            'type-error))

  (or type-error?
      all-blamed-at-max-precision?))
