#lang at-exp rscript

(require (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../util/program.rkt"
         "../../runner/mutation-runner.rkt"
         "../../runner/unify-program.rkt"
         "../../util/mutant-util.rkt"
         "../../util/path-utils.rkt")

;; db layout is:
;; bench-name? -> (hash/c mutant/c run-status?)

(main
 #:arguments {[(hash-table ['out-db results-db-path]
                           ['config config-path]
                           ['mutants-db-path mutants-db-path])
               bench-paths]
              #:once-each
              [("-c" "--config")
               'config
               ("Config for running the mutants."
                "This program runs mutants in the minimum config, so TR is probably what you want.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-m" "--mutants-db")
               'mutants-db-path
               ("Path to the db of mutants for which to compute results.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--out-db")
               'out-db
               ("Path to the db to populate with pre-computed results.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              #:args bench-paths}

 #:check [(not (empty? bench-paths))
          @~a{
              This program takes as positional arguments the benchmark @;
              paths for which to compute mutant results.
              }]
 #:check [(db:path-to-db? mutants-db-path)
          @~a{Can't find db at @mutants-db-path}]

 (install-configuration! config-path)

 (define mutants-db (db:get mutants-db-path))

 (unless (db:path-to-db? results-db-path)
   (db:new! results-db-path))
 (define results-db (db:get results-db-path))

 (for ([bench-path (in-list bench-paths)])
   (define the-benchmark (read-benchmark bench-path))
   (define bench-name (benchmark->name the-benchmark))
   (define config
     (for/hash ([{mod _} (in-hash (make-max-bench-config the-benchmark))])
       (values mod 'none)))
   (match-define (struct* benchmark-configuration
                          ([main main-path]
                           [others others-paths]))
     (configure-benchmark the-benchmark config))
   (define mutants-by-mod (db:read mutants-db bench-name))

   (displayln @~a{Computing result of @bench-name ...})
   (define mutant-results-hash
     (for*/hash ([mod-name (in-list (benchmark->mutatable-modules the-benchmark))]
                 [index (in-list (hash-ref mutants-by-mod mod-name empty))])
       (display @~a{@mod-name @"@" @index                              @"\r"})
       (define module-to-mutate-path
         (pick-file-by-name (list* main-path others-paths)
                            mod-name))
       (define the-program (make-unified-program main-path
                                                 others-paths))
       (define the-program-mods (program->mods the-program))
       (define the-module-to-mutate
         (find-unified-module-to-mutate module-to-mutate-path
                                        the-program-mods))
       (values (mutant #f mod-name index)
               (run-with-mutated-module the-program
                                        the-module-to-mutate
                                        index
                                        config
                                        #:timeout/s (default-timeout/s)
                                        #:memory/gb (default-memory-limit/gb)
                                        #:suppress-output? #t))))
   (db:set! results-db bench-name mutant-results-hash)))
