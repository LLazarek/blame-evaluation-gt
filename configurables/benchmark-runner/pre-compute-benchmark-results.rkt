#lang at-exp rscript

(require "load-pre-computed-result.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../util/program.rkt"
         "../../runner/mutation-runner.rkt"
         "../../runner/unify-program.rkt"
         "../../util/mutant-util.rkt"
         "../../util/path-utils.rkt")

;; db layout is:
;; bench-name? -> (hash/c mutant/c run-status?)

(define-runtime-paths
  [configurables-dir ".."])

(main
 #:arguments {[(hash-table ['out-db results-db]
                           ['config config-path]
                           _ ...)
               bench-paths]
              #:once-each
              [("-c" "--config")
               'config
               "Config for selecting which mutants to compute results for."
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--out-db")
               'out-db
               ("Path to the db to populate with pre-computed results."
                @~a{Default: @(pre-computed-results-db)})
               #:collect ["path"
                          take-latest
                          (path->string (build-path configurables-dir
                                                    (pre-computed-results-db)))]]
              #:args bench-paths}

 #:check [(not (empty? bench-paths))
          @~a{
              This program takes as positional arguments the benchmark @;
              paths for which to compute mutant results.
              }]

 (install-configuration! config-path)

 (unless (db:path-to-db? results-db)
   (db:new! results-db))
 (define db (db:get results-db))

 (for ([bench-path (in-list bench-paths)])
   (define the-benchmark (read-benchmark bench-path))
   (define bench-name (benchmark->name the-benchmark))
   (define config
     (for/hash ([{mod _} (in-hash (make-max-bench-config the-benchmark))])
       (values mod 'none)))
   (match-define (and the-benchmark-configuration
                      (struct* benchmark-configuration
                               ([main main-path]
                                [others others-paths])))
     (configure-benchmark the-benchmark config))
   (define select-mutants (configured:select-mutants))

   (displayln @~a{Computing result of @bench-name ...})
   (define mutant-results-hash
     (for*/hash ([mod-name (in-list (benchmark->mutatable-modules the-benchmark))]
                 [index (select-mutants mod-name the-benchmark)])
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
                                        #:suppress-output? #f))))
   (db:set! db bench-name mutant-results-hash)))
