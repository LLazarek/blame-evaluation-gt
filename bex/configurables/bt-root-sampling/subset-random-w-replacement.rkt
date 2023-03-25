#lang at-exp racket

(require "../../experiment/mutant-factory-data.rkt"
         "../../util/optional-contracts.rkt"
         "../../configurations/config.rkt"
         "make-bt-root-sampler.rkt"
         "random-config.rkt"
         racket/function
         racket/random)

(provide (contract-out [make-bt-root-sampler make-bt-root-sampler/c]
                       [root-missing-blame-response root-missing-blame-response/c]
                       [root-config-filter (parameter/c (bench-info? config/c . -> . boolean?))]))

(define root-missing-blame-response (const 'resample))

(define root-config-filter (make-parameter
                            (λ _ (error 'make-bt-root-sampler:subset-random
                                        "No root config filter installed."))))
(define max-root-config-filter-failures (make-parameter 1000))

(define ((make-bt-root-sampler bench-info mutant) n)
  (define max-config (bench-info-max-config bench-info))
  (define (take-root? root)
    ((root-config-filter) bench-info root))
  (cond [(<= (hash-count max-config) 10) ; not "too many"
         (define cache (make-hash))
         (define all-config-variants
           (let loop ([keys (hash-keys max-config)])
             (match keys
               [(cons a-key more-keys)
                (append*
                 (map (λ (h) (list (hash-set h a-key 'none)
                                   (hash-set h a-key 'types)))
                      (loop more-keys)))]
               ['() (list (hash))])))
         (define valid-configs (filter take-root? all-config-variants))
         (if (empty? valid-configs)
             (error
              'make-bt-root-sampler:subset-random
              @~a{
                  The current root config filter rejects all configurations!
                  Giving up.
                  })
             (random-sample valid-configs n))]
        [else
         (let loop ([remaining-todo n]
                    [samples empty]
                    [failure-count 0])
           (cond [(zero? remaining-todo)
                  samples]
                 [(= failure-count (max-root-config-filter-failures))
                  (error
                   'make-bt-root-sampler:subset-random
                   @~a{
                       The current root config filter rejected @failure-count configs in a row.
                       Giving up.
                       })]
                 [else
                  (define root (random-config-variant max-config))
                  (if (take-root? root)
                      (loop (sub1 remaining-todo)
                            (cons root samples)
                            0)
                      (loop remaining-todo
                            samples
                            (add1 failure-count)))]))]))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name make-bt-root-sampler
    (parameterize ([root-config-filter (λ _ #t)])
      (for/and/test ([i (in-range 50)])
        (test-= (length ((make-bt-root-sampler (bench-info #f
                                                           (hash "a.rkt" 'types
                                                                 "b.rkt" 'types
                                                                 "c.rkt" 'types
                                                                 "main.rkt" 'types))
                                               #f)
                         i))
                i)))

    (parameterize ([root-config-filter (λ (bench-info config)
                                         (equal? (hash-ref config "a.rkt") 'types))])
      (define configs ((make-bt-root-sampler (bench-info #f
                                                         (hash "a.rkt" 'types
                                                               "b.rkt" 'types
                                                               "c.rkt" 'types
                                                               "main.rkt" 'types))
                                             #f)
                       20))
      (for/and/test ([c (in-list configs)])
        (equal? (hash-ref c "a.rkt") 'types)))))
