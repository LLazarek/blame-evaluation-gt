#lang at-exp rscript

(require racket/random
         (prefix-in db: "../../db/db.rkt")
         (only-in "../../experiment/mutant-factory-data.rkt" sample-size)
         "../../util/mutant-util.rkt"
         "../../util/for.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../configurations/config.rkt"
         "../../configurables/configurables.rkt"
         "../../mutation-analysis/debugging-scenarios.rkt"
         "random-config.rkt")

(define-runtime-paths
  [default-benchmarks-dir "../../../../gtp-benchmarks/benchmarks"])

;; benchmark/c
;; (hash/c mod-name? (listof mutation-index?))
;; natural?
;; [(or/c #f (hash/c mutant/c (listof config/c)))]
;; ->
;; (hash/c mutant/c (listof config/c))
(define (select-roots-by-mutant bench
                                mutant-samples-by-module
                                root-sample-size
                                [interesting-configs-by-mutant #f])
  (define ((add-to-list v) l)
    (cons v l))
  (define bench-max-config (make-max-bench-config bench))
  (cond [interesting-configs-by-mutant
         (for*/hash ([{mod-name indices} (in-hash mutant-samples-by-module)]
                     [index (in-list indices)])
           (define this-mutant (mutant #f mod-name index))
           (values this-mutant
                   (random-sample (hash-ref interesting-configs-by-mutant this-mutant)
                                  root-sample-size
                                  #:replacement? #t)))]
        [else ;; select random scenarios
         (for*/fold ([roots-by-mutant (hash)])
                    ([{mod-name indices} (in-hash mutant-samples-by-module)]
                     [index (in-list indices)]
                     [root-id (in-range root-sample-size)])
           (define random-config (random-config-variant bench-max-config))
           (define root
             (if (hash-has-key? random-config mod-name)
                 (hash-set random-config
                           mod-name
                           'none)
                 random-config))
           (hash-update roots-by-mutant
                        (mutant #f mod-name index)
                        (add-to-list root)
                        empty))]))

(main
 #:arguments {[(hash-table ['benchmarks-dir benchmarks-dir]
                           ['samples-db mutant-samples-db-path]
                           ['interesting-scenarios-db-path interesting-scenarios-db-path]
                           ['outdb-path outdb-path]
                           ['root-sample-size (app string->number root-sample-size)]
                           ['config-path config-path])
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
              [("-i" "--interesting-scenarios-db")
               'interesting-scenarios-db-path
               ("Path to the db containing interesting scenarios for the mutants in"
                "the mutant-samples-db."
                "If supplied, scenarios for each mutant will be sampled from those in this db."
                "Otherwise, random scenarios (which do not type the buggy module) from the lattice for each mutant will be selected.")
               #:collect {"path" take-latest #f}]
              [("-o" "--outdb")
               'outdb-path
               "DB to populate with pre-selected bt roots."
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-n" "--sample-size")
               'root-sample-size
               ("How many roots to select per mutant?"
                @~a{Default: @(sample-size)})
               #:collect {"N" take-latest (~a (sample-size))}]
              [("-c" "--config")
               'config-path
               "The config to use for generating mutants."
               #:mandatory
               #:collect ["path" take-latest #f]]}
 #:check [(path-to-existant-directory? benchmarks-dir)
          @~a{@benchmarks-dir is not a directory.}]
 #:check [(db:path-to-db? mutant-samples-db-path)
          @~a{@mutant-samples-db-path does not look like a db.}]
 #:check [(or (not interesting-scenarios-db-path)
              (db:path-to-db? interesting-scenarios-db-path))
          @~a{@interesting-scenarios-db-path does not look like a db.}]

 (install-configuration! config-path)

 (define mutant-samples-db (db:get mutant-samples-db-path))
 (define interesting-scenarios-db (and interesting-scenarios-db-path
                                       (db:get interesting-scenarios-db-path)))

 (define roots-by-benchmark
   (for/hash ([bench-name (in-list (db:keys mutant-samples-db))])
     (define bench (read-benchmark (build-path benchmarks-dir bench-name)))
     (define mutant-summaries-by-module
       (db:read mutant-samples-db bench-name))
     (define selected-roots-by-mutant
       (select-roots-by-mutant bench
                               mutant-summaries-by-module
                               root-sample-size
                               (and interesting-scenarios-db
                                    (db:read interesting-scenarios-db bench-name))))
     (values bench-name selected-roots-by-mutant)))

 (define (maybe-serialize-config c)
   (if (serialized-config? c)
       c
       (serialize-config c)))

 (define serialized-roots-by-benchmark
   (for/hash ([{bench-name roots-by-mutant} (in-hash roots-by-benchmark)])
     (values bench-name
             (for/hash ([{mutant roots} (in-hash roots-by-mutant)])
               (values mutant
                       ;; maybe bc the configs that come fomr the
                       ;; interesting-scenarios-db are already serialized
                       (map maybe-serialize-config roots))))))

 (db:new! outdb-path)
 (define outdb (db:get outdb-path))
 (void (db:write! outdb
                  serialized-roots-by-benchmark
                  #:writer (λ (v f)
                             (with-output-to-file f
                               (thunk (pretty-write v)))))))
