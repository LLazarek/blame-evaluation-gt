#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "mutant-selector.rkt")
(provide (contract-out [select-mutants mutant-selector/c]))

(require "../../util/mutant-util.rkt"
         "../mutation/mutate-benchmark.rkt"
         "../../configurations/configure-benchmark.rkt"
         racket/runtime-path
         racket/random)

(define-runtime-path summaries-dir "../../mutation-analysis/summaries")

(struct summary (valid-indices ; (hash/c string? (listof natural?))
                 max-index ; natural?
                 triggered-mutators ; (listof string?)
                 )
  #:prefab)
;; benchmark/c -> summary?
(define (summary-of bench)
  (file->value (build-path summaries-dir (benchmark->name bench))))
;; string? summary? -> (listof natural?)
(define (summarized-indices-for-mutator mutator summary)
  (hash-ref (summary-valid-indices summary) mutator))

(define (select-mutants module-to-mutate-name
                        bench
                        sample-size)
  (define summary (summary-of bench))
  (sanity-check-summary! summary
                         module-to-mutate-name
                         bench)
  (define valid-indices-by-mutator
    (for/hash ([mutator (in-list active-mutator-names)])
      (values mutator
              (summarized-indices-for-mutator mutator summary))))
  (define max-sample-size-by-mutator
    (for/hash ([{mutator indices} (in-hash valid-indices-by-mutator)])
      (values mutator (length indices))))
  (define samples-by-mutator
    (distribute-sample-size-to-mutators sample-size
                                        max-sample-size-by-mutator))
  (define sampled-indices*
    (for/list ([{mutator samples} (in-hash samples-by-mutator)])
      (define indices-for-mutator
        (hash-ref valid-indices-by-mutator mutator))
      (random-sample indices-for-mutator samples)))
  (in-list (flatten sampled-indices*)))

(define (sanity-check-summary! summary
                               module-to-mutate-name
                               bench)
  (unless (= (max-mutation-index module-to-mutate-name bench)
             (summary-max-index summary))
    (raise-user-error
     'sample-within-mutators:sanity-check
     @~a{
         The summary is out of date: update it before running again.
         In particular, @;
         Max mutation index calculated for @module-to-mutate-name @;
         differs from that recorded in summary.
         }))
  (unless (subset? (summary-triggered-mutators summary)
                   active-mutator-names)
    (raise-user-error
     'sample-within-mutators:sanity-check
     @~a{
         The summary is out of date: update it before running again.
         In particular, @;
         Mutators recorded in summary are not a subset of active mutator names.
         From summary: @(summary-triggered-mutators summary)
         Active:       @active-mutator-names
         })))

(define (distribute-sample-size-to-mutators sample-size
                                            max-sample-size-by-mutator)
  (define mutators (hash-keys max-sample-size-by-mutator))
  (let redistribute
      ([distributed (for/hash ([mutator (in-list mutators)])
                      (values mutator 0))]
       [space-left max-sample-size-by-mutator]
       [remaining-to-distribute sample-size])
    (define remaining-mutators
      (for/list ([{mutator left} (in-hash space-left)]
                 #:when (> left 0))
        mutator))
    (cond [(or (<= remaining-to-distribute 0)
               (empty? remaining-mutators))
           distributed]
          [(<= remaining-to-distribute (length remaining-mutators))
           (for/fold ([distributed distributed])
                     ([i (in-range remaining-to-distribute)]
                      [mutator (in-list remaining-mutators)])
             (hash-update distributed mutator add1))]
          [else
           (define distribution-per-mutator
             (quotient remaining-to-distribute (length remaining-mutators)))
           (define distributed-this-time
             (for/hash ([mutator (in-list remaining-mutators)])
               (define distributed-to-mutator
                 (if (<= distribution-per-mutator (hash-ref space-left mutator))
                     distribution-per-mutator
                     (hash-ref space-left mutator)))
               (values mutator
                       distributed-to-mutator)))
           (define new-distributed
             (for/hash ([{mutator previously-distributed} (in-hash distributed)])
               (values mutator
                       (+ previously-distributed
                          (hash-ref distributed-this-time mutator 0)))))
           (define new-space-left
             (for/hash ([{mutator previously-left} (in-hash space-left)])
               (values mutator
                       (- previously-left
                          (hash-ref distributed-this-time mutator 0)))))
           (define total-distributed-this-time
             (apply + (hash-values distributed-this-time)))
           (define new-remaining-to-distribute
             (- remaining-to-distribute total-distributed-this-time))
           (redistribute new-distributed
                         new-space-left
                         new-remaining-to-distribute)])))

(module+ test
  (require ruinit)
  (test-begin
    #:name distribute-sample-size-to-mutators
    (test-match (distribute-sample-size-to-mutators 100
                                                    (make-immutable-hash
                                                     (map (λ (name) (cons name 15))
                                                          (build-list 10 values))))
                (hash-table [(? (between/c 0 9)) 10] ___))
    (test-match (distribute-sample-size-to-mutators 100
                                                    (make-immutable-hash
                                                     (map (λ (name) (cons name 5))
                                                          (build-list 10 values))))
                (hash-table [(? (between/c 0 9)) 5] ___))
    (test-equal? (distribute-sample-size-to-mutators 100
                                                    (hash 0 15
                                                          1 50
                                                          2 30
                                                          3 20))
                 (hash 0 15
                       1 35
                       2 30
                       3 20))
    (test-equal? (distribute-sample-size-to-mutators 2
                                                    (hash 0 15
                                                          1 50
                                                          2 30
                                                          3 20))
                 (hash 0 1
                       1 1
                       2 0
                       3 0))))

