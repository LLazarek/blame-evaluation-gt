#lang at-exp rscript

(require (prefix-in db: "../db/db.rkt")
         "../data-analysis/read-data.rkt"
         "../util/for.rkt"
         "../util/mutant-util.rkt"
         "mutation-analysis-summaries.rkt"
         "debugging-scenarios.rkt")

(define (interesting-scenarios-log->db! log-path db-path)
  (define logged-data (file->list log-path))
  (define interesting-scenarios-by-benchmark-then-mutant
    (for/hash/fold ([{scenario-parts interesting?} (in-dict logged-data)]
                    #:when interesting?)
      #:combine cons
      #:default empty
      (match-define (list benchmark-name (list mutant config)) scenario-parts)
      (values benchmark-name
              (scenario mutant config))))

  (unless (file-exists? db-path)
    (db:new! db-path))
  (define db (db:get db-path))
  (db:write! db interesting-scenarios-by-benchmark-then-mutant))

(define mutator-name? string?)
(define/contract (group-by-mutator interesting-scenarios
                                   benchmark-name
                                   mutant-mutators)
  ((listof scenario?)
   string?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name?
           (listof mutant?)))

  (define (scenario->mutator a-scenario)
    (match-define (scenario (mutant #f mod index) _) a-scenario)
    (hash-ref mutant-mutators
              (mutant benchmark-name mod index)))
  (define grouped-by-mutator
    (group-by scenario->mutator interesting-scenarios))
  (for/hash ([mutator-group (in-list grouped-by-mutator)])
    (values (scenario->mutator (first mutator-group))
            (remove-duplicates (map scenario-mutant mutator-group)))))

(main
 #:arguments ([(hash-table ['interesting-scenarios-log interesting-scenarios-log]
                           ['interesting-scenarios-db interesting-scenarios-db-path]
                           ['mutant-summaries-db mutant-summaries-db]
                           ['outdb outdb-path])
               args]
              #:once-each
              [("-l" "--log")
               'interesting-scenarios-log
               ("Path to the log of `find-interesting-scenarios.rkt` with which to populate"
                "the interesting-scenarios-db provided with -d.")
               #:collect {"path" take-latest #f}]
              [("-i" "--interesting-scenarios-db")
               'interesting-scenarios-db
               ("Path to the db of interesting scenarios by benchmark."
                "Mandatory.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-s" "--mutant-summaries-db")
               'mutant-summaries-db
               ("Path to the db of mutant summaries (typically: `type-err-summaries.rktdb`)."
                "Mandatory.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--out-db")
               'outdb
               ("Path to the db to write with interesting mutant summaries."
                "Mandatory.")
               #:collect {"path" take-latest #f}
               #:mandatory])
 #:check [(or (not interesting-scenarios-log)
              (path-to-existant-file? interesting-scenarios-log))
          @~a{Can't find @interesting-scenarios-log}]

 (when interesting-scenarios-log
   (interesting-scenarios-log->db! interesting-scenarios-log
                                   interesting-scenarios-db-path))

 (define interesting-scenarios-db (db:get interesting-scenarios-db-path))
 (define mutant-mutators (read-mutants-by-mutator mutant-summaries-db))
 (define data
   (for/hash ([bench (in-list (db:keys interesting-scenarios-db))])
     (define interesting-scenarios (db:read interesting-scenarios-db bench))
     (values bench
             (benchmark-summary
              (group-by-mutator interesting-scenarios
                                bench
                                mutant-mutators)))))
 (unless (file-exists? outdb-path)
   (db:new! outdb-path))
 (define outdb (db:get outdb-path))
 (db:write! outdb data))
