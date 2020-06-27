#lang at-exp rscript

(require "load-pre-computed-result.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../runner/program.rkt"
         "../../runner/mutation-runner.rkt"
         "../../util/mutant-util.rkt"
         "../../util/path-utils.rkt")

;; db layout is:
;; bench-name? -> (hash/c (list/c module-name? index?) run-status?)

(define (make-untyped-program-for the-bench untyped-config)
  (define bench-configured-untyped
    (configure-benchmark the-bench
                         untyped-config))
  (make-program (benchmark-configuration-main bench-configured-untyped)
                (benchmark-configuration-others bench-configured-untyped)))

(main
 #:arguments {[_ bench-paths]
              #:once-each
              [("-c" "--config")
               'config
               "Config for selecting which mutants to compute results for."
               #:collect ["path"
                          (set-parameter current-configuration-path)
                          #f]
               #:mandatory]
              [("-o" "--out-db")
               'out-db
               ("Path to the db to populate with pre-computed results."
                @~a{Default: @(pre-computed-results-db)})
               #:collect ["path"
                          (set-parameter pre-computed-results-db)
                          (pre-computed-results-db)]]
              #:args bench-paths}

 #:check [(not (empty? bench-paths))
          @~a{
              This program takes as positional arguments the benchmark @;
              paths for which to compute mutant results.
              }]

 (define results
   (for/hash ([bench-path (in-list bench-paths)])
     (define the-bench (read-benchmark bench-path))
     (define bench-name (benchmark->name the-bench))
     (define untyped-config
       (for/hash ([{mod _} (in-hash (make-max-bench-config the-bench))])
         (values mod 'none)))
     (define bench-untyped-program
       (make-untyped-program-for the-bench untyped-config))
     (define select-mutants
       (load-configured (current-configuration-path)
                        "mutant-sampling"
                        'select-mutants))

     (displayln @~a{Computing result of @bench-name ...})
     (define mutant-results-hash
       (for*/hash ([mod-name (in-list (benchmark->mutatable-modules the-bench))]
                   [index (select-mutants mod-name the-bench)])
         (display @~a{@mod-name @"@" @index                              @"\r"})
         (define mod
           (findf (compose1 (path-ends-with mod-name) mod-path)
                  (list* (program-main bench-untyped-program)
                         (program-others bench-untyped-program))))
         (values (list mod-name index)
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
