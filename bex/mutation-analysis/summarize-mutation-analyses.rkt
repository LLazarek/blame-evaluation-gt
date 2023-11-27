#lang at-exp rscript

(require "mutation-analysis-summaries.rkt"
         (prefix-in db: "../db/db.rkt"))

(define (summarize-mutations-in progress-log-path)
  (define ((add-to-list x) l)
    (cons x l))

  (define contents (file->list progress-log-path))
  ;; Tracking max-indices just for sanity checking
  (define-values {mutation-info-by-module
                  max-indices-by-module}
    (for/fold ([info (hash)]
               [max-indices (hash)])
              ([mutation (in-list contents)])
      (define new-max-indices
        (match mutation
          [(list (list benchmark mod-name index) _ _)
           (hash-update max-indices
                        mod-name
                        (λ (x) (max x index))
                        -1)]))
      (match mutation
        [(list (list benchmark mod-name index) (not #f) mutator)
         (values (hash-update info
                              mod-name
                              (add-to-list (list index mutator))
                              empty)
                 new-max-indices)]
        [else (values info new-max-indices)])))
  (for/hash ([{mod-name mutations} (in-hash mutation-info-by-module)])
    (define mutator-indices
      (for/fold ([mutator-indices (hash)])
                ([mutation (in-list mutations)])
        (match-define (list index mutator) mutation)
        (hash-update mutator-indices
                     mutator
                     (add-to-list index)
                     empty)))
    (define max-index (hash-ref max-indices-by-module mod-name))
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

(module test racket/base)
