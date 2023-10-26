#lang at-exp rscript

(require (prefix-in db: "../../db/db.rkt")
         "../configurables.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../util/program.rkt"
         "../../runner/mutation-runner.rkt"
         "../../runner/unify-program.rkt"
         "../../util/mutant-util.rkt"
         "../../util/path-utils.rkt"
         "../../util/run-mutants-in-parallel.rkt"
         process-queue/priority)

;; db layout is:
;; bench-name? -> (hash/c mutant/c run-status?)

(define-logger pre-compute)

(main
 #:arguments {[(hash-table ['out-db results-db-path]
                           ['config config-path]
                           ['mutants-db-path mutants-db-path]
                           ['progress-log progress-log]
                           ['process-limit (app string->number process-limit)]
                           ['working-dir working-dir])
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
              [("-l" "--progress-log")
               'progress-log
               ("Record progress in the given log file."
                "If it exists and is not empty, resume from the point reached in the log.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-j" "--process-limit")
               'process-limit
               ("How many parallel processes to use."
                "Default: 1")
               #:collect {"N" take-latest "1"}]
              [("-d" "--working-dir")
               'working-dir
               ("Set the working directory for storing temporary data."
                @~a{Default: pre-compute-benchmark-results-scratch})
               #:collect {"path" take-latest "pre-compute-benchmark-results-scratch"}]
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

 (when (directory-exists? working-dir)
   (log-pre-compute-info @~a{Deleting old working dir at @working-dir})
   (delete-directory/files working-dir))
 (make-directory* working-dir)

 (define results-db (db:get results-db-path))

 (for ([bench-path (in-list bench-paths)])
   (define the-benchmark (read-benchmark bench-path))
   (define bench-name (benchmark->name the-benchmark))
   (define config
     (for/hash ([{mod _} (in-hash (make-max-bench-config the-benchmark))])
       (values mod 'none)))
   (define mutants-by-mod (db:read mutants-db bench-name))

   (log-pre-compute-info @~a{Computing results for @bench-name ...})
   (define mutant-results-hash
     (collect-mutant-results (list the-benchmark)
                             config-path
                             (λ (mod) (hash-ref mutants-by-mod mod empty))
                             (λ _ config)
                             process-limit
                             (λ (mutant-output-file the-mutant)
                               (match (with-handlers ([exn:fail? (λ _ (raise-bad-mutant-result))])
                                        (file->value mutant-output-file))
                                 [(? run-status? rs)
                                  (log-pre-compute-info @~a{@bench-name @the-mutant => @(run-status-outcome rs)})
                                  (when (equal? (run-status-outcome rs) 'syntax-error)
                                    (eprintf @~a{

                                                 Warning: @bench-name @the-mutant produces syntax-error

                                                 }))
                                  rs]
                                 [else (raise-bad-mutant-result)]))
                             #:progress-logging (list 'auto progress-log)
                             #:working-dir working-dir
                             #:failure-retries 3))
   (log-pre-compute-info @~a{@bench-name done})
   (db:set! results-db bench-name mutant-results-hash))
 (log-pre-compute-info @~a{Done.}))
