#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "mutant-selector.rkt")
(provide (contract-out [select-mutants mutant-selector/c])
         (struct-out summary))

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
(define (summary-of bench module-to-mutate-name)
  (define summary
    (file->value (build-path summaries-dir (benchmark->name bench))))
  (sanity-check-summary! summary
                         module-to-mutate-name
                         bench)
  summary)
;; string? summary? -> (listof natural?)
(define (summarized-indices-for-mutator mutator summary)
  (hash-ref (summary-valid-indices summary) mutator empty))

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

(define (select-mutants module-to-mutate-name
                        bench
                        sample-size
                        [get-summary-of summary-of])
  (define summary (get-summary-of bench module-to-mutate-name))
  (define sampled-indices-by-mutator (sample-indices-by-mutator summary sample-size))
  (in-list (flatten (hash-values sampled-indices-by-mutator))))

(define (sample-indices-by-mutator summary sample-size)
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
  (define sampled-indices-by-mutator
    (for/hash ([{mutator samples} (in-hash samples-by-mutator)])
      (define indices-for-mutator
        (hash-ref valid-indices-by-mutator mutator))
      (define sampled-indices (random-sample indices-for-mutator samples
                                             #:replacement? #f))
      (values mutator sampled-indices)))
  sampled-indices-by-mutator)

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
                       3 0)))

  (define m1-text
    @~a{
        #lang racket

        (define (foo x)
          (baz x 42)) ; 1-2, 3, 4-8
        (define (baz x y)
          (if (even? x) ; 9
              y
              (* (sub1 y) (add1 x) 42))) ; 10-18
        (define (booz x y)
          (if (even? x) ; 19
              y
              (* (sub1 y) (add1 x) 42))) ; 20-28

        (foo (baz 0 1))
        })
  ; negate-cond 2
  ; top-level-id-swap 2
  ; position-swap 3
  ; constant-swap 15
  ; arith-op-swap 6
  (define-test-env {setup-env! cleanup-env!}
    #:directories ([test-bench "./test-bench"]
                   [ut "./test-bench/untyped"]
                   [t  "./test-bench/typed"])
    #:files ([test-module-1/ut (build-path ut "test-mod-1.rkt")
                               m1-text]
             [test-module-1/t (build-path t "test-mod-1.rkt")
                              m1-text]))
  (define call-with-deterministic-random
    (let ([rando (make-pseudo-random-generator)])
      (λ (thunk)
        (parameterize ([current-pseudo-random-generator rando])
          (random-seed 42)
          (thunk)))))
  (test-begin
    #:name sample-indices-by-mutator
    #:before (setup-env!)
    #:after (cleanup-env!)
    (ignore (define the-test-bench (read-benchmark test-bench))
            (define operator-indices
              (hash "constant-swap" (append (range 3 8)
                                            (range 13 18)
                                            (range 23 28))
                    "negate-conditional" '(8 18)
                    "arithmetic-op-swap" (append (range 9 12)
                                                 (range 19 22))
                    "top-level-id-swap" '(0 1)
                    "position-swap" '(2 10 20)))
            (define the-summary
              (summary
               operator-indices
               28
               (hash-keys operator-indices)))
            (define test:summary-of (const the-summary))
            (define sample-size 26)

            (define sampled-indices-by-operator
              (call-with-deterministic-random
               (thunk (sample-indices-by-mutator the-summary sample-size)))))
    (test-match sampled-indices-by-operator
                (hash-table ["arithmetic-op-swap" (app length 6)]
                            ["constant-swap" (app length 13)]
                            ["negate-conditional" (app length 2)]
                            ["position-swap" (app length 3)]
                            ["top-level-id-swap" (app length 2)]))
    (extend-test-message
     (for/and/test ([operator (in-list (hash-keys operator-indices))])
                   (define samples (hash-ref sampled-indices-by-operator operator))
                   (equal? (remove-duplicates samples)
                           samples))
     "Sampled indices contain duplicates")

    (ignore (define sampled-indices/list
              (stream->list
               (call-with-deterministic-random
                (thunk (select-mutants
                        "test-mod-1.rkt"
                        the-test-bench
                        sample-size
                        test:summary-of))))))
    (test-= (length sampled-indices/list)
            sample-size)

    (test-equal?
     (sort sampled-indices/list <)
     (sort (flatten (hash-values sampled-indices-by-operator)) <))))

