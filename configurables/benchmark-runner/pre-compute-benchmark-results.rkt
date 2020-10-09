#lang at-exp rscript

(require "load-pre-computed-result.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../util/program.rkt"
         "../../runner/mutation-runner.rkt"
         "../../util/mutant-util.rkt"
         "../../util/path-utils.rkt")

;; db layout is:
;; bench-name? -> (hash/c mutant/c run-status?)

(define-runtime-paths
  [configurables-dir ".."])

(define (make-untyped-program-for the-bench untyped-config)
  (define bench-configured-untyped
    (configure-benchmark the-bench
                         untyped-config))
  (make-program (benchmark-configuration-main bench-configured-untyped)
                (benchmark-configuration-others bench-configured-untyped)))

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

 (define results
   (for/hash ([bench-path (in-list bench-paths)])
     (define the-bench (read-benchmark bench-path))
     (define bench-name (benchmark->name the-bench))
     (define untyped-config
       (for/hash ([{mod _} (in-hash (make-max-bench-config the-bench))])
         (values mod 'none)))
     (define bench-untyped-program
       (make-untyped-program-for the-bench untyped-config))
     (define select-mutants (configured:select-mutants))

     (displayln @~a{Computing result of @bench-name ...})
     (define mutant-results-hash
       (for*/hash ([mod-name (in-list (benchmark->mutatable-modules the-bench))]
                   [index (select-mutants mod-name the-bench)])
         (display @~a{@mod-name @"@" @index                              @"\r"})
         (define mod
           (findf (compose1 (path-ends-with mod-name) mod-path)
                  (program->mods bench-untyped-program)))
         (values (mutant #f mod-name index)
                 (run-with-mutated-module bench-untyped-program
                                          mod
                                          index
                                          untyped-config
                                          #:timeout/s (default-timeout/s)
                                          #:memory/gb (default-memory-limit/gb)))))
     (values bench-name
             mutant-results-hash)))

 (unless (db:path-to-db? (pre-computed-results-db))
   (db:new! (pre-computed-results-db)))
 (define db (db:get (pre-computed-results-db)))
 (void (db:write! db results)))
