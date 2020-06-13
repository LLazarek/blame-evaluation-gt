#lang at-exp rscript

(require "../../util/mutant-util.rkt"
         "../../configurations/configure-benchmark.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../mutation-analysis/mutation-analysis-summaries.rkt"
         "../mutation/mutate-benchmark.rkt"
         "mutant-selector.rkt"
         "sample-within-mutators.rkt"
         racket/random)

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
         Error: The summary is out of date: update it before running again.
         In particular, @;
         max mutation index calculated for @(benchmark->name bench) @;
         module @module-to-mutate-name @;
         differs from that recorded in summary.
         Calculated: @(max-mutation-index module-to-mutate-name bench)
         Summary: @(summary-max-index summary)
         }))
  (unless (subset? (summary-triggered-mutators summary)
                   active-mutator-names)
    (raise-user-error
     'sample-within-mutators:sanity-check
     @~a{
         Error: The summary is out of date: update it before running again.
         In particular, @;
         mutators recorded in summary are not a subset of active mutator names.
         From summary: @(summary-triggered-mutators summary)
         Active:       @active-mutator-names
         })))

(define (sample-mutants module-to-mutate-name
                        sample-size
                        summary
                        #:exclude [excluded-indices empty]
                        #:replacement? [replacement? #f])
  (define sampled-indices-by-mutator
    (sample-indices-by-mutator summary
                               sample-size
                               #:exclude excluded-indices
                               #:replacement? replacement?))
  (flatten (hash-values sampled-indices-by-mutator)))

(define (sample-indices-by-mutator summary
                                   sample-size
                                   #:exclude [excluded-indices empty]
                                   #:replacement? [replacement? #f])
  (define valid-indices-by-mutator
    (for/hash ([mutator (in-list active-mutator-names)])
      (define all-indices-for-mutator
        (summarized-indices-for-mutator mutator summary))
      (values mutator
              (set-subtract all-indices-for-mutator
                            excluded-indices))))
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
                                             #:replacement? replacement?))
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
     "Sampled indices contain duplicates")))


(define default-sample-size (* 20 (length active-mutator-names)))
(main
 #:arguments {[(hash-table ['summaries-db summaries-db-path]
                           ['samples-db samples-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['sample-size (app string->number sample-size)]
                           ['exclude samples-to-exclude]
                           ['sample-with-replacement? sample-with-replacement?])
               args]
              #:once-each
              [("-s" "--sumaries-db")
               'summaries-db
               ("Database in which to find benchmark mutant summaries."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect ["path"
                          take-latest
                          (path->string (mutation-analysis-summaries-db))]]
              [("-o" "--samples-db")
               'samples-db
               ("Database in which to place benchmark mutant summaries."
                @~a{Default: @(mutation-analysis-samples-db)})
               #:collect ["path"
                          take-latest
                          (path->string (mutation-analysis-samples-db))]]

              [("-n" "--sample-size")
               'sample-size
               ("Size of mutant samples to generate."
                "This is the total number of mutants to sample."
                @~a{Default: @default-sample-size})
               #:collect ["N" take-latest (~a default-sample-size)]]
              [("-r" "--with-replacement")
               'sample-with-replacement?
               ("Sample mutants with replacement instead of without."
                "Default: sample without replacement.")
               #:record]

              [("-b" "--benchmarks-dir")
               'benchmarks-dir
               ("Directory in which to find benchmarks."
                "Used to sanity-check summaries while generating samples,"
                "if provided."
                "*It is strongly recommended to perform this checking.*")
               #:collect ["path" take-latest #f]]

              #:multi
              [("-e" "--exclude")
               'exclude
               ("Exclude the samples in the given database from sampling."
                "This is intended to allow incremental sample additions,"
                "when sampling without replacement (see -r).")
               #:collect ["path" cons empty]]}
 #:check [(db:path-to-db? summaries-db-path)
          @~a{Can't find db at @summaries-db-path}]

 (unless (db:path-to-db? samples-db-path)
   (displayln @~a{Creating new db at @samples-db-path})
   (db:new! samples-db-path))

 (displayln
  @~a{Sampling @(if sample-with-replacement? "with" "without") replacement.})

 (define excluded-sample-dbs (map db:get samples-to-exclude))

 (define samples-db (db:get samples-db-path))
 (define summaries-db (db:get summaries-db-path))

 (define data
   (for/hash ([bench-name (in-list (db:keys summaries-db))])
     (displayln @~a{Sampling for @bench-name ...})

     (define summaries (db:read summaries-db bench-name))

     (when benchmarks-dir
       (define bench (read-benchmark (build-path benchmarks-dir
                                                 bench-name)))
       (for ([{module-name summary} (in-hash summaries)])
         (sanity-check-summary! summary
                                module-name
                                bench)))

     (define excluded-sample-maps-for-this-bench
       (for/list ([excluded-db (in-list excluded-sample-dbs)])
         (db:read excluded-db bench-name)))

     (define samples-by-module
       (for/hash ([{module-name summary} (in-hash summaries)])
         (define excluded-indices-for-this-module
           (flatten
            (for/list ([excluded-sample-map
                        (in-list excluded-sample-maps-for-this-bench)])
              (hash-ref excluded-sample-map
                        module-name))))
         (values module-name
                 (sample-mutants module-name
                                 sample-size
                                 summary
                                 #:exclude excluded-indices-for-this-module
                                 #:replacement? sample-with-replacement?))))
     (values bench-name
             samples-by-module)))
 (void (db:write! samples-db data)))
