#lang at-exp rscript

(provide read-blame-trail-db->df)

(require db
         data-frame
         bex/orchestration/experiment-info
         bex/configurables/configurables
         (prefix-in db: bex/db/db)
         "read-data.rkt"
         "plot-common.rkt")

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
                (~s (blame-trail-mutant-summaries bt))
                mutator
                (if (satisfies-BT-hypothesis? bt) 1 0))))

(define (read-blame-trail-db->df db-path [query "SELECT * FROM trails"])
  (define conn (sqlite3-connect #:database db-path
                                #:mode 'read-only))
  (begin0 (for/data-frame {mode benchmark mutant id mutant-summaries mutator success}
            ([{m b mt id m-s mu ok?} (in-query conn query)])
            (values m b (string->value mt) id (string->value m-s) mu (= ok? 1)))
    (disconnect conn)))

(define (string->value s)
  (call-with-input-string s read))

(main
 #:arguments ([(hash-table ['maybe-db-path maybe-db-path]
                           ['type-err-summaries-db-path type-err-db-path]
                           ['config config-path])
               (list results-dir)]
              #:once-each
              [("-s" "--type-err-summaries-db")
               'type-err-summaries-db-path
               ("Path to the type error summaries DB from which to extract mutator info."
                "Mandatory.")
               #:mandatory
               #:collect {"path" take-latest #f}]
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
 (define conn (sqlite3-connect #:database db-path
                               #:mode 'create))
 (call-with-transaction conn
                        (thunk (read-blame-trails-into-db/all-modes/benchmarks!
                                results-dir
                                conn
                                (read-mutants-by-mutator type-err-db-path))))
 (disconnect conn))
