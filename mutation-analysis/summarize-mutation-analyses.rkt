#lang at-exp rscript

(require "../configurables/mutant-sampling/sample-within-mutators.rkt")

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
 #:arguments {[(hash-table ['outdir outdir])
               progress-logs]
              #:once-each
              [("-o" "--outdir")
               'outdir
               ("Directory in which to place the summaries."
                @~a{Default: @summaries-dir})
               #:collect ["path" take-latest (path->string summaries-dir)]]
              #:args progress-log-paths}

 (unless (directory-exists? outdir)
   (displayln @~a{Creating output directory @outdir})
   (make-directory* outdir))

 (for ([progress-log (in-list progress-logs)])
   (define the-summary (summarize-mutations-in progress-log))
   (define bench-name (progress-log-path->bench-name progress-log))
   (define outfile (build-path outdir
                               (benchmark-name->summary-file-name bench-name)))
   (with-output-to-file outfile
     #:exists 'replace
     (thunk (pretty-write the-summary)))))
