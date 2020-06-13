#lang at-exp rscript

(require "mutation-analysis-summaries.rkt"
         (prefix-in db: "../db/db.rkt"))

(define (summarize-mutations-in progress-log-path)
  (define ((add-to-list x) l)
    (cons x l))

  (define contents (file->list progress-log-path))
  (define mutation-info-by-module
    (for/fold ([info (hash)])
              ([mutation (in-list contents)])
      (match mutation
        [(list (list mod-name index) #t mutator)
         (hash-update info
                      mod-name
                      (add-to-list (list index mutator))
                      empty)]
        [else info])))
  (for/hash ([{mod-name mutations} (in-hash mutation-info-by-module)])
    (define mutator-indices
      (for/fold ([mutator-indices (hash)])
                ([mutation (in-list mutations)])
        (match-define (list index mutator) mutation)
        (hash-update mutator-indices
                     mutator
                     (add-to-list index)
                     empty)))
    (define max-index (apply max (map first mutations)))
    (define triggered-mutators (hash-keys mutator-indices))
    (values mod-name
            (summary mutator-indices
                     max-index
                     triggered-mutators))))

(define (progress-log-path->bench-name progress-log-path)
  (match (basename progress-log-path)
    [(regexp @regexp{^(.+)-progress\.(log|txt)}
             (list _ name _))
     name]
    [else
     (raise-user-error
      'summarize-mutation-analyses
      @~a{Unable to extract benchmark name from @progress-log-path})]))

(main
 #:arguments {[(hash-table ['outdb outdb])
               progress-logs]
              #:once-each
              [("-o" "--outdb")
               'outdb
               ("Database in which to place the summaries."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect ["path"
                          take-latest
                          (path->string (mutation-analysis-summaries-db))]]
              #:args progress-log-paths}

 (unless (db:path-to-db? outdb)
   (displayln @~a{Creating db at @outdb})
   (db:new! outdb))

 (define db (db:get outdb))
 (define data
   (for/hash ([progress-log (in-list progress-logs)])
     (define the-summary (summarize-mutations-in progress-log))
     (define bench-name (progress-log-path->bench-name progress-log))
     (values bench-name
             the-summary)))
 (void (db:write! db data)))
