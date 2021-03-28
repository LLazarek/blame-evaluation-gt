#lang at-exp racket

(require "../../configurations/config.rkt"
         "../../experiment/mutant-factory-data.rkt"
         "../../util/mutant-util.rkt"
         "../../runner/mutation-runner.rkt"
         "../../util/optional-contracts.rkt"
         "../../util/path-utils.rkt")

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
  (define buggy-mod-type-error?
    (type-error-on-buggy-mod? blamed/type-error-locations result))

  (define trail-ended? (or buggy-mod-type-error?
                           all-blamed-at-max-precision?))

  (cond [(and buggy-mod-type-error?
              (not all-blamed-at-max-precision?))
         ;; This should never happen: something has gone wrong, because
         ;; type errors only blame one location and that location must be
         ;; typed
         (log-factory-message
          'warning
          @~a{
              Found a mutant with type error in the buggy module, @;
              but the module is not typed?
              Type error location: @blamed/type-error-locations

              Mutant: @;
              @mod @"@" @index [@id] {@(blame-trail-id the-blame-trail)}
              With config:
              @~v[config]
              })]
        [(and (not buggy-mod-type-error?)
              all-blamed-at-max-precision?)
         (log-factory-message
          'info
          @~a{
              BT VIOLATION: @;
              Found mutant with blamed/type-error location at types @;
              that is not the buggy module.
              Blamed: @~v[blamed/type-error-locations]

              Mutant: @;
              @mod @"@" @index [@id] {@(blame-trail-id the-blame-trail)}
              With config:
              @~v[config]
              })]
        [else (void)])

  trail-ended?)

(define/contract (type-error-on-buggy-mod? blamed-mod-names result)
  ((listof module-name?) run-status/c . -> . boolean?)

  (match result
    [(struct* run-status ([outcome 'type-error]
                          [blamed (== blamed-mod-names)]))
     #t]
    [_ #f]))
