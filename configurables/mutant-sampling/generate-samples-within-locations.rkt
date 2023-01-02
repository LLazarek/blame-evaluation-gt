#lang at-exp rscript

(require "../../mutation-analysis/categorize-mutants.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "use-pre-selected-samples.rkt"
         "sampling-common.rkt")

(define-runtime-paths
  [configurables-dir ".."])

(define default-sample-size-multiplier 10)
(main
 #:arguments {[(hash-table ['config-path config-path]
                           ['summaries-db mutant-summaries-db-path]
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
               ("Database in which to find benchmark mutant summaries."
                "Either this or -S must be supplied.")
               #:collect ["path" take-latest #f]
               #:mandatory-unless (λ (flags) (member 'benchmark-summaries-db flags))]
              [("-S" "--benchmark-summaries-db")
               'benchmark-summaries-db
               ("Database in which to find benchmark summaries."
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
                "This is the total number of mutants to sample."
                @~a{Default: @default-sample-size-multiplier * <number of location types>})
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
 #:check [(or (not mutant-summaries-db-path)
              (db:path-to-db? mutant-summaries-db-path))
          @~a{Can't find db at @mutant-summaries-db-path}]
 #:check [(or (not benchmark-summaries-db-path)
              (db:path-to-db? benchmark-summaries-db-path))
          @~a{Can't find db at @benchmark-summaries-db-path}]

 (install-configuration! config-path)

 (define-values {benchmark-names benchmark->benchmark-summary}
   (summaries-db->benchmark-info mutant-summaries-db-path
                                 benchmark-summaries-db-path))
 (define all-categories
   )

 (define sample-size
   (match maybe-sample-size
     [#f (* default-sample-size-multiplier
            (length all-categories))]
     [(? string? (app string->number (and n (not #f)))) n]
     [else (raise-user-error 'generate-samples-within-mutators
                             "Error: sample size must be a number")]))

 ;; copied from generate-samples-within-mutators.rkt, needs to be adapted:

 (unless (db:path-to-db? samples-db-path)
   (displayln @~a{Creating new db at @samples-db-path})
   (db:new! samples-db-path))

 (displayln
  @~a{Sampling @(if sample-with-replacement? "with" "without") replacement.})

 (define excluded-sample-dbs (map db:get samples-to-exclude))

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
