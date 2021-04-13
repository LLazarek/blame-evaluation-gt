#lang at-exp rscript

(require racket/hash
         (prefix-in db: "../db/db.rkt")
         "../configurations/configure-benchmark.rkt"
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../util/for.rkt"
         "plot-common.rkt")

(define-runtime-paths
  [dyn-err-summaries-db-path "../dbs/code-mutations/dyn-err-summaries.rktdb"]
  [benchmarks-dir "../../gtp-benchmarks/benchmarks"])

(define (benchmark-name->max-config name)
  (make-max-bench-config (read-benchmark (build-path benchmarks-dir name))))

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
    ;; sub1 because the mutated mod must always be untyped
    (define typeable-mod-count (sub1 (hash-count max-config)))
    (values benchmark
            (expt 2 typeable-mod-count))))

(define all-possible-interesting-scenarios
  (for/sum ([benchmark (in-list all-benchmarks)])
    (define mutants (hash-ref mutants-with-interesting-scenarios-count/by-benchmark
                              benchmark))
    (define possible-scenarios (hash-ref possible-interesting-scenario-counts/by-benchmark
                                         benchmark))
    (* mutants possible-scenarios)))
