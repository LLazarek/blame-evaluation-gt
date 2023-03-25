#lang at-exp rscript

(require "../../util/mutant-util.rkt"
         "../../configurations/configure-benchmark.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../mutation-analysis/mutation-analysis-summaries.rkt"
         "../configurables.rkt"
         "mutant-selector.rkt"
         "use-pre-selected-samples.rkt"
         racket/random)

(define-runtime-paths
  [configurables-dir ".."])

(define current-active-mutator-names (make-parameter #f))

(define ((mutant-for module) index)
  (mutant #f module index))

(define (module-samples->benchmark-samples module-samples)
  (hash-map mutant-for module-samples))
(define (benchmark-samples->module-samples benchmark-samples)
  (define ((add-to-list x) l)
    (cons x l))
  (for/fold ([samples-by-module (hash)])
            ([sample (in-list benchmark-samples)])
    (match-define (mutant _ module-name index) sample)
    (hash-update samples-by-module
                 module-name
                 (add-to-list index)
                 empty)))


;; string? benchmark-summary? -> (listof mutant?)
(define (summarized-indices-for-mutator mutator benchmark-summary)
  (hash-ref (benchmark-summary-mutants-by-mutator benchmark-summary)
            mutator
            empty))

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
                   (current-active-mutator-names))
    (raise-user-error
     'sample-within-mutators:sanity-check
     @~a{
         Error: The summary is out of date: update it before running again.
         In particular, @;
         mutators recorded in summary are not a subset of active mutator names.
         From summary: @(summary-triggered-mutators summary)
         Active:       @(current-active-mutator-names)
         })))

(define (sample-mutants sample-size
                        benchmark-summary
                        #:exclude [excluded-indices empty]
                        #:replacement? [replacement? #f])
  (define sampled-indices-by-mutator
    (sample-indices-by-mutator benchmark-summary
                               sample-size
                               #:exclude excluded-indices
                               #:replacement? replacement?))
  (flatten (hash-values sampled-indices-by-mutator)))

(define (sample-indices-by-mutator benchmark-summary
                                   sample-size
                                   #:exclude [excluded-indices empty]
                                   #:replacement? [replacement? #f])
  (define valid-indices-by-mutator
    (for/hash ([mutator (in-list (current-active-mutator-names))])
      (define all-indices-for-mutator
        (summarized-indices-for-mutator mutator benchmark-summary))
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
  (require ruinit
           "../../util/path-utils.rkt")

  (install-configuration! "../configs/test.rkt")
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
  (define operator-indices
    (hash "constant-swap" (append (range 3 8)
                                  (range 13 18)
                                  (range 23 28))
          "negate-conditional" '(8 18)
          "arithmetic-op-swap" (append (range 9 12)
                                       (range 19 22))
          "top-level-id-swap" '(0 1)
          "position-swap" '(2 10 20)))

  (define test-mod-name (file-name-string-from-path test-module-1/ut))
  (define test-mod-summary (summary
                            operator-indices
                            28
                            (hash-keys operator-indices)))
  (test-begin
    #:name module-summaries->benchmark-summary
    (test-equal?
     (module-summaries->benchmark-summary (hash test-mod-name test-mod-summary))
     (benchmark-summary
      (for/hash ([{mutator indices} (in-hash operator-indices)])
        (values mutator
                (map (mutant-for test-mod-name) indices))))))

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
            (define sample-size 26)

            (define the-summary
              (module-summaries->benchmark-summary
               (hash test-mod-name test-mod-summary)))

            (define sampled-indices-by-operator
              (call-with-deterministic-random
               (thunk
                (parameterize ([current-active-mutator-names (configured:active-mutator-names)])
                  (sample-indices-by-mutator the-summary sample-size))))))
    (test-match sampled-indices-by-operator
                (hash-table ["arithmetic-op-swap" (app length 6)]
                            ["constant-swap" (app length 13)]
                            ["negate-conditional" (app length 2)]
                            ["position-swap" (app length 3)]
                            ["top-level-id-swap" (app length 2)]))

    (ignore (define the-summary-2
              (module-summaries->benchmark-summary
               (hash test-mod-name test-mod-summary
                     "another-mod.rkt" test-mod-summary)))
            (define sampled-indices-by-operator-2
              (call-with-deterministic-random
               (thunk
                (parameterize ([current-active-mutator-names
                                (configured:active-mutator-names)])
                  (sample-indices-by-mutator the-summary-2 sample-size))))))
    ;; Now there's double the number of mutants, so things can be more evenly
    ;; distributed: 26/5 = 5 to start with:
    ;; 5 -> 6
    ;; 5    6
    ;; 4    4
    ;; 5    6
    ;; 4    4
    (test-match sampled-indices-by-operator-2
                (hash-table ["arithmetic-op-swap" (app length 6)]
                            ["constant-swap" (app length 6)]
                            ["negate-conditional" (app length 4)]
                            ["position-swap" (app length 6)]
                            ["top-level-id-swap" (app length 4)]))
    (for/and/test
     ([samples (in-list (list sampled-indices-by-operator
                                           sampled-indices-by-operator-2))])
     (extend-test-message
      (for/and/test ([operator (in-list (hash-keys operator-indices))])
                    (define samples (hash-ref sampled-indices-by-operator operator))
                    (equal? (remove-duplicates samples)
                            samples))
      "Sampled indices contain duplicates"))


    ;; exclusion
    (ignore
     (define already-sampled (flatten (hash-values sampled-indices-by-operator)))
     (define sampled-indices-by-operator/exclusion
              (call-with-deterministic-random
               (thunk
                (parameterize ([current-active-mutator-names
                                (configured:active-mutator-names)])
                  (sample-indices-by-mutator the-summary
                                             sample-size
                                             #:exclude already-sampled))))))
    (test-match sampled-indices-by-operator/exclusion
                (hash-table ["constant-swap" (app length 2)]))))


(define default-sample-size-multiplier 10)
(main
 #:arguments {[(hash-table ['config-path config-path]
                           ['summaries-db summaries-db-path]
                           ['benchmark-summaries-db benchmark-summaries-db-path]
                           ['samples-db samples-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['sample-size maybe-sample-size]
                           ['exclude samples-to-exclude]
                           ['sample-with-replacement? sample-with-replacement?])
               args]
              #:once-each
              [("-c" "--config")
               'config-path
               ("Configuration to use for generating samples."
                "This argument is mandatory.")
               #:mandatory
               #:collect ["path" take-latest #f]]

              [("-s" "--summaries-db")
               'summaries-db
               ("Database in which to find benchmark mutant summaries (like type/dyn-err-sumaries.rktdb)."
                "Either this or -S must be supplied.")
               #:collect ["path" take-latest #f]
               #:mandatory-unless (λ (flags) (member 'benchmark-summaries-db flags))]
              [("-S" "--benchmark-summaries-db")
               'benchmark-summaries-db
               ("Database in which to find benchmark summaries (like interesting-mutant-summaries.rktdb)."
                "Either this or -s must be supplied.")
               #:collect ["path" take-latest #f]
               #:mandatory-unless (λ (flags) (member 'summaries-db flags))]

              [("-o" "--samples-db")
               'samples-db
               ("Database in which to place benchmark mutant summaries."
                @~a{Default: @(pre-selected-mutant-samples-db)})
               #:collect ["path"
                          take-latest
                          (path->string (build-path configurables-dir
                                                    (pre-selected-mutant-samples-db)))]]

              [("-n" "--sample-size")
               'sample-size
               ("Size of mutant samples to generate."
                "This is the total number of mutants to sample per benchmark."
                @~a{Default: @default-sample-size-multiplier * <number of mutators>})
               #:collect ["N" take-latest #f]]
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
 #:check [(or (not summaries-db-path)
              (db:path-to-db? summaries-db-path))
          @~a{Can't find db at @summaries-db-path}]
 #:check [(or (not benchmark-summaries-db-path)
              (db:path-to-db? benchmark-summaries-db-path))
          @~a{Can't find db at @benchmark-summaries-db-path}]

 (install-configuration! config-path)
 (current-active-mutator-names (configured:active-mutator-names))

 (define sample-size
   (match maybe-sample-size
     [#f (* default-sample-size-multiplier
            (length (current-active-mutator-names)))]
     [(? string? (app string->number (and n (not #f)))) n]
     [else (raise-user-error 'generate-samples-within-mutators
                             "Error: sample size must be a number")]))

 (unless (db:path-to-db? samples-db-path)
   (displayln @~a{Creating new db at @samples-db-path})
   (db:new! samples-db-path))

 (displayln
  @~a{Sampling @(if sample-with-replacement? "with" "without") replacement.})

 (define excluded-sample-dbs (map db:get samples-to-exclude))

 (define-values {benchmark-names benchmark->benchmark-summary}
   (match* {summaries-db-path benchmark-summaries-db-path}
     [{(? path-string?) #f}
      (define summaries-db (db:get summaries-db-path))
      (values (db:keys summaries-db)
              (λ (bench-name)
                (define module-summaries (db:read summaries-db bench-name))
                (when benchmarks-dir
                  (define bench (read-benchmark (build-path benchmarks-dir
                                                            bench-name)))
                  (for ([{module-name summary} (in-hash module-summaries)])
                    (sanity-check-summary! summary
                                           module-name
                                           bench)))
                (module-summaries->benchmark-summary module-summaries)))]
     [{#f (? path-string?)}
      (define bench-summaries-db (db:get benchmark-summaries-db-path))
      (values (db:keys bench-summaries-db)
              (λ (bench) (db:read bench-summaries-db bench)))]))

 (define data
   (for/hash ([bench-name (in-list benchmark-names)])
     (displayln @~a{Sampling for @bench-name ...})

     (define benchmark-summary (benchmark->benchmark-summary bench-name))

     (define excluded-samples-for-this-bench
       (flatten
        (for/list ([excluded-db (in-list excluded-sample-dbs)])
          (module-samples->benchmark-samples (db:read excluded-db bench-name)))))

     (define benchmark-samples
       (sample-mutants sample-size
                       benchmark-summary
                       #:exclude excluded-samples-for-this-bench
                       #:replacement? sample-with-replacement?))

     (define samples-by-module
       (benchmark-samples->module-samples benchmark-samples))
     (values bench-name
             samples-by-module)))

 (define samples-db (db:get samples-db-path))
 (void (db:write! samples-db data)))
