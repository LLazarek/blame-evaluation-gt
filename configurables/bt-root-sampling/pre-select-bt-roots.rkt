#lang at-exp rscript

(require (prefix-in db: "../../db/db.rkt")
         (only-in "../../experiment/mutant-factory-data.rkt" sample-size)
         "../../util/mutant-util.rkt"
         "../../configurations/configure-benchmark.rkt"
         "random-config.rkt")

(define-runtime-paths
  [default-benchmarks-dir "../../../gtp-benchmarks/benchmarks"])

;; benchmark/c
;; (hash/c mod-name? (listof mutation-index?))
;; ->
;; (hash/c mutant/c (listof config/c))
(define (select-roots-by-mutant bench mutant-samples-by-module root-sample-size)
  (define ((add-to-list v) l)
    (cons v l))
  (define bench-max-config (make-max-bench-config bench))
  (for*/fold ([roots-by-mutant (hash)])
             ([{mod-name indices} (in-hash mutant-samples-by-module)]
              [index (in-list indices)]
              [root-id (in-range root-sample-size)])
    (define root (hash-set (random-config-variant bench-max-config)
                           mod-name
                           'none))
    (hash-update roots-by-mutant
                 (mutant #f mod-name index)
                 (add-to-list root)
                 empty)))

(main
 #:arguments {[(hash-table ['benchmarks-dir benchmarks-dir]
                           ['samples-db mutant-samples-db-path]
                           ['outdb-path outdb-path]
                           ['root-sample-size (app string->number root-sample-size)])
               args]
              #:once-each
              [("-b" "--benchmarks")
               'benchmarks-dir
               ("Path to the directory containing benchmarks."
                @~a{Default: @default-benchmarks-dir})
               #:collect {"path" take-latest default-benchmarks-dir}]
              [("-d" "--mutant-samples-db")
               'samples-db
               ("Path to the db containing mutant samples, specifying all mutants"
                "for which to sample roots."
                "(See `generate-samples-within-mutators.rkt`.)")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--outdb")
               'outdb-path
               "DB to populate with pre-selected bt roots."
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-n" "--sample-size")
               'root-sample-size
               ("How many roots to select?"
                @~a{Default: @(sample-size)})
               #:collect {"N" take-latest (~a (sample-size))}]}
 #:check [(path-to-existant-directory? benchmarks-dir)
          @~a{@benchmarks-dir is not a directory.}]
 #:check [(db:path-to-db? mutant-samples-db-path)
          @~a{@mutant-samples-db-path does not look like a db.}]

 (define mutant-samples-db (db:get mutant-samples-db-path))

 (define roots-by-benchmark
   (for/hash ([bench-name (in-list (db:keys mutant-samples-db))])
     (define bench (read-benchmark (build-path benchmarks-dir bench-name)))
     (define mutant-summaries-by-module
       (db:read mutant-samples-db bench-name))
     (define selected-roots-by-mutant
       (select-roots-by-mutant bench mutant-summaries-by-module root-sample-size))
     (values bench-name selected-roots-by-mutant)))

 (db:new! outdb-path)
 (define outdb (db:get outdb-path))
 (void (db:write! outdb roots-by-benchmark #:writer (λ (v f)
                                                      (with-output-to-file f
                                                        (thunk (pretty-write v)))))))
