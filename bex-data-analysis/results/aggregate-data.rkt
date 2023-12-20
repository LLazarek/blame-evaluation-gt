#lang at-exp rscript

(require db
         bex/orchestration/experiment-info
         bex/configurables/configurables
         bex/configurations/config
         bex/configurations/configure-benchmark
         bex/experiment/blame-trail-data
         bex/util/path-utils
         (prefix-in dumb-db: bex/db/db)
         "read-data.rkt"
         "util.rkt"
         "check-for-missing-mutants-or-trails.rkt"
         "experiment-info.rkt")

(define-runtime-paths
  [repo-setup-configs-dir "../../bex/setup"]
  [db-setup-configs-dir "../../bex/orchestration/db-setup"]
  [this-repo "../.."]
  [TR-repo "../../../typed-racket"])

(define default-db-name "data.sqlite")

(define (read-blame-trails-into-db/all-modes/benchmarks! results-dir
                                                         conn
                                                         mutant-mutators)
  (query-exec conn
              "CREATE TABLE trails (
mode NOT NULL,
benchmark NOT NULL,
mutant NOT NULL,
id NOT NULL,
mutant_summaries NOT NULL,
mutator NOT NULL,
success NOT NULL,

PRIMARY KEY (mode, benchmark, mutant, id)
)")
  (for* ([mode (in-list experiment-modes)]
         [mode-dir (in-value (build-path results-dir mode))]
         #:when (directory-exists? mode-dir)

         [a-benchmark-data-files (in-list (find-data-files mode-dir))]
         [benchmark-name (in-value (benchmark-data-files-name a-benchmark-data-files))]

         [mutant-trails-file (in-directory (benchmark-data-files-data-dir a-benchmark-data-files))]
         #:when (regexp-match? #rx".rkt_[0-9]+.rktd$" mutant-trails-file)
         [bt (in-list (blame-trail-summaries->blame-trails (file->list mutant-trails-file)
                                                           a-benchmark-data-files))])
    (define mutator (hash-ref mutant-mutators (blame-trail-mutant-id bt)))
    (query-exec conn
                "INSERT INTO trails VALUES ($1, $2, $3, $4, $5, $6, $7)"
                mode
                benchmark-name
                (~s (blame-trail-mutant-id bt))
                (blame-trail-trail-id bt)
                (~s (map serialize-mutant-summary (blame-trail-mutant-summaries bt)))
                mutator
                (if (blame-trail-succeeded? bt) 1 0))))

(define serialize-mutant-summary
  (match-lambda [(mutant-summary id run-status config-hash)
                 (mutant-summary id run-status (serialize-config config-hash))]))

(define (record-benchmark-info-in-db! db-conn)
  (define (abstractify-benchmark-path p)
    (match (explode-path/string p)
      [(list _ ... "benchmarks" more ...)
       (~a (apply build-path "benchmarks" more))]))
  (define abstractify-benchmark
    (match-lambda
      [(benchmark typed-mods untyped-mods maybe-base maybe-both)
       (benchmark (map abstractify-benchmark-path typed-mods)
                  (map abstractify-benchmark-path untyped-mods)
                  (and maybe-base (abstractify-benchmark-path maybe-base))
                  (and maybe-both (abstractify-benchmark-path maybe-both)))]))

  (query-exec db-conn
              "CREATE TABLE benchmarks (
name NOT NULL PRIMARY KEY,
benchmark NOT NULL
)")
  (for ([benchmark-name (in-list experiment-benchmarks)])
    (define bench (benchmark-name->benchmark benchmark-name))
    (query-exec db-conn
                "INSERT INTO benchmarks VALUES ($1, $2)"
                benchmark-name
                (~s (abstractify-benchmark bench)))))

(define (sanity-check-for-missing-mutants-or-trails! db-conn
                                                     mutants-by-mutator
                                                     mutant-samples-db-path
                                                     bt-root-db-path
                                                     source-dir)
  (define mutant-samples-db (dumb-db:get mutant-samples-db-path))
  (define root-samples-db (dumb-db:get bt-root-db-path))
  (define modes (query-list db-conn "SELECT DISTINCT mode FROM trails"))
  (define benchmarks (query-list db-conn "SELECT DISTINCT benchmark FROM trails"))
  (unless (set=? modes experiment-modes)
    (raise-user-error
     @~a{
         Sanity check failed: @;
         bex/orchestration/experiment-info has a different set of modes than those in the results

         experiment-info has: @~s[experiment-modes]
         results has: @~s[modes]
         }))
  (unless (set=? benchmarks experiment-benchmarks)
    (raise-user-error
     @~a{
         Sanity check failed: @;
         bex/orchestration/experiment-info has a different set of benchmarks than those in the results

         experiment-info has: @~s[experiment-benchmarks]
         results has: @~s[benchmarks]
         }))
  (for ([mode-to-check (in-list modes)])
    (for ([bench-name (in-list benchmarks)])
      (define bench-mutant-samples (dumb-db:read mutant-samples-db bench-name))
      (define bench-bt-root-samples (dumb-db:read root-samples-db bench-name))
      (define mutants-in-db
        (map string->value
             (query-list db-conn
                         "SELECT mutant FROM trails WHERE mode = $1 AND benchmark = $2"
                         mode-to-check
                         bench-name)))
      (sanity-check-mutants bench-name
                            mutants-in-db
                            bench-mutant-samples
                            (build-path source-dir mode-to-check bench-name (~a bench-name ".log")))
      (sanity-check-blame-trails bench-name
                                 mutants-in-db
                                 bench-bt-root-samples))))

(define (record-mutant/mutator-info-in-db! db-conn)
  (query-exec db-conn
              "CREATE VIEW mutant_info as SELECT mutant, mutator FROM trails")
  (query-exec db-conn
              "CREATE TABLE mutators (name NOT NULL)")
  (for ([m (in-list (configured:active-mutator-names))])
    (query-exec db-conn
                "INSERT INTO mutators VALUES ($1)"
                m)))

(define (record-setup-info-in-db! db-conn)
  (define repo-setup-config
    (let loop ()
      (define name
        (read-user-input-line!
         @~a{
             Enter the name of the setup config to store in the DB metadata. @;
             It should be the one (of those in @repo-setup-configs-dir) that @;
             can be used to set up this repo for the same experiment that generated this data.

             Files in @repo-setup-configs-dir :
             @(pretty-format (map ~a (directory-list repo-setup-configs-dir)))
             }))
      (if (file-exists? (build-path repo-setup-configs-dir name))
          name
          (loop))))
  (define db-setup-config
    (let loop ()
      (define name
        (read-user-input-line!
         @~a{
             Enter the name of the experiment dbs setup config to store in the DB metadata. @;
             It should be the one (of those in @db-setup-configs-dir) that @;
             can be used to set up databases for the same experiment that generated this data.

             Files in @db-setup-configs-dir :
             @(pretty-format (map ~a (directory-list db-setup-configs-dir)))
             }))
      (if (file-exists? (build-path db-setup-configs-dir name))
          name
          (loop))))
  (query-exec db-conn
              "CREATE TABLE setup_configs (
type NOT NULL PRIMARY KEY,
name NOT NULL
)")
  (query-exec db-conn
              "INSERT INTO setup_configs VALUES ($1, $2)"
              "repo-setup"
              repo-setup-config)
  (query-exec db-conn
              "INSERT INTO setup_configs VALUES ($1, $2)"
              "db-setup"
              db-setup-config)

  (user-prompt! @~a{
                    Now going to record the current state of the following repos in the @;
                    database metadata. Make sure they're in the desired state before continuing.
                    : blame-evaluation-gt, gtp-benchamrks, typed-racket
                    })
  (query-exec db-conn
              "CREATE TABLE setup_state (
repo NOT NULL PRIMARY KEY,
remote NOT NULL,
sha NOT NULL
)")
  (for ([repo (list this-repo
                    (apply build-path (drop-right (explode-path/string (simple-form-path benchmarks-dir)) 1))
                    TR-repo)])
    (define name (basename repo))
    (define-values {remote commit}
      (parameterize ([current-directory repo])
        (values (system/string "git remote -v")
                (system/string "git rev-parse HEAD"))))
    (query-exec db-conn
                "INSERT INTO setup_state VALUES ($1, $2, $3)"
                name
                remote
                commit)))

(main
 #:arguments ([(hash-table ['maybe-db-path maybe-db-path]
                           ['type-err-summaries-db-path type-err-db-path]
                           ['config config-path]

                           ['mutant-samples-db-path mutant-samples-db-path]
                           ['bt-root-db-path bt-root-db-path])
               (list results-dir)]
              #:once-each
              [("-s" "--type-err-summaries-db")
               'type-err-summaries-db-path
               ("Path to the type error summaries DB from which to extract mutator info."
                "Mandatory.")
               #:mandatory
               #:collect {"path" take-latest #f}]
              [("-S" "--mutant-samples-db")
               'mutant-samples-db-path
               ("Mutant samples DB, for sanity-checking results."
                "Mandatory.")
               #:mandatory
               #:collect {"path" take-latest #f}]
              [("-r" "--bt-root-samples-db")
               'bt-root-db-path
               ("BT root sample db, for sanity-checking results.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names and deserializing configurations.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--outdb")
               'maybe-db-path
               ("The path to the sqlite db in which to aggregate the data."
                @~a{Default: '@default-db-name' under `results-dir`.})
               #:collect {"path" take-latest 'auto}]
              #:args [results-dir]
              ;; #:args-contract (list/c path-to-existant-directory?)
              )

 (install-configuration! config-path)
 (define db-path
   (match maybe-db-path
     ['auto (build-path results-dir default-db-name)]
     [other other]))
 (when (and (file-exists? db-path)
            (user-prompt! @~a{There's already a db at @db-path, trash it first?}))
   (rename-file-or-directory db-path (build-path "/tmp/trash" (basename db-path)) #t))
 (displayln "Creating db")
 (define conn (sqlite3-connect #:database db-path
                               #:mode 'create))
 (define mutants-by-mutator (read-mutants-by-mutator type-err-db-path))
 (displayln "Populating trails table...")
 (call-with-transaction conn
                        (thunk (read-blame-trails-into-db/all-modes/benchmarks!
                                results-dir
                                conn
                                mutants-by-mutator)))

 (displayln "Populating metadata tables...")
 (call-with-transaction conn
                        (thunk (record-benchmark-info-in-db! conn)))
 (call-with-transaction conn
                        (thunk (record-mutant/mutator-info-in-db! conn)))
 (call-with-transaction conn
                        (thunk (record-setup-info-in-db! conn)))

 (displayln "Sanity checking data...")
 (sanity-check-for-missing-mutants-or-trails! conn
                                              mutants-by-mutator
                                              mutant-samples-db-path
                                              bt-root-db-path
                                              results-dir)

 (displayln "Done.")
 (disconnect conn))
