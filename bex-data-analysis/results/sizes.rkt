#lang at-exp racket

(provide possible-interesting-scenario-counts/by-benchmark)

(require racket/hash
         (only-in rscript define-runtime-paths)
         (prefix-in db: bex/db/db)
         bex/configurations/configure-benchmark
         bex/mutation-analysis/mutation-analysis-summaries
         bex/util/for
         bex/configurables/configurables
         "plot-common.rkt")

(define-runtime-paths
  [dyn-err-summaries-db-path "../../bex/dbs/type-api-mutations/dyn-err-summaries.rktdb"]
  [benchmarks-dir "../../../gtp-benchmarks/benchmarks"]
  [TR-config "../../bex/configurables/configs/TR.rkt"])

(define (benchmark-name->max-config name)
  (call-with-configuration
   TR-config
   (λ _ (make-max-bench-config (read-benchmark (build-path benchmarks-dir name))))))

(define dyn-err-summaries-db (db:get dyn-err-summaries-db-path))
(define all-benchmarks (db:keys dyn-err-summaries-db))

(define mutants-with-interesting-scenarios-count/by-benchmark
  (for/hash ([benchmark (in-list all-benchmarks)])
    (define mutant-count
      (for*/sum ([{mod summary} (in-hash (db:read dyn-err-summaries-db benchmark))]
                 [{mutator-name indices} (in-hash (summary-valid-indices summary))])
        (length indices)))
    (values benchmark mutant-count)))

(define mutants-with-interesting-scenarios-count/by-benchmark+mutator
  (for/fold ([h (hash)])
            ([benchmark (in-list all-benchmarks)])
    (define mutant-count-by-mutator
      (for/hash/fold ([{mod summary} (in-hash (db:read dyn-err-summaries-db benchmark))]
                      #:when #t
                      [{mutator-name indices} (in-hash (summary-valid-indices summary))])
                     #:combine +
                     #:default 0
                     (values mutator-name (length indices))))
    (define mutant-count-by-benchmark+mutator
      (for/hash ([{mutator count} (in-hash mutant-count-by-mutator)])
        (values (list benchmark mutator) count)))
    (hash-union h mutant-count-by-benchmark+mutator)))

(define possible-interesting-scenario-counts/by-benchmark
  (for/hash ([benchmark (in-list all-benchmarks)])
    (define max-config (benchmark-name->max-config benchmark))
    (define typeable-mod-count (hash-count max-config))
    (values benchmark
            (expt 2 typeable-mod-count))))

(define all-possible-interesting-scenarios
  (for/sum ([benchmark (in-list all-benchmarks)])
    (define mutants (hash-ref mutants-with-interesting-scenarios-count/by-benchmark
                              benchmark))
    (define possible-scenarios (hash-ref possible-interesting-scenario-counts/by-benchmark
                                         benchmark))
    (* mutants possible-scenarios)))
