#lang at-exp rscript

(define-runtime-paths
  [summaries-dir "../mutation-analysis/summaries"]
  [mutations-dir "../configurables/mutant-sampling/samples"]
  [special-cases-dir
   "../configurables/module-instrumentation/transient-special-cases"]
  [pre-computed-results-dir
   "../configurables/benchmark-runner/pre-computed-results"])

(define db-dirs (list summaries-dir
                      mutations-dir
                      special-cases-dir
                      pre-computed-results-dir))

(main
 #:arguments {[(hash-table ['dbs dbs-path]
                           ['save save?]
                           ['load load?]
                           ['temp-dir temp-dir])
               args]
              #:once-each
              [("-d" "--dbs")
               'dbs
               "The file path in which so save or load the dbs."
               #:mandatory
               #:collect ["path" take-latest #f]]
              [("-s" "--save")
               'save
               "Action: Save the dbs."
               #:conflicts '(load)
               #:mandatory-unless (λ (flags) (member 'load flags))
               #:record]
              [("-l" "--load")
               'load
               "Action: Load the dbs."
               #:conflicts '(save)
               #:mandatory-unless (λ (flags) (member 'save flags))
               #:record]
              [("-t" "--temp-dir")
               'temp-dir
               ("Specify a directory for temporary files."
                "Useful if the default temp dir is a different filesystem.")
               #:collect ["path" take-latest (~a (find-system-path 'temp-dir))]]}
 #:check [(or save? (file-exists? dbs-path))
          @~a{Error: unable to find @dbs-path}]

 (define data-dir (build-path temp-dir "copy-dbs.rkt-data"))
 (when (directory-exists? data-dir)
   (delete-directory/files data-dir))
 (make-directory data-dir)
 (define temp-dirs
   (for/hash ([db-dir (in-list db-dirs)])
     (values db-dir
             (build-path data-dir (basename db-dir)))))
 (define temp-db-name "dbs.tar.gz")
 (cond
   [save?
    (when (file-exists? dbs-path)
      (cond [(user-prompt! @~a{
                               @dbs-path already exists, @;
                               overwrite it?
                               })
             (delete-file dbs-path)]
            [else (eprintf "Canceling~n")
                  (exit 1)]))
    (for ([db-dir (in-list db-dirs)])
      (copy-directory/files db-dir
                            (hash-ref temp-dirs db-dir)))
    (parameterize ([current-directory data-dir])
      (define temp-dirs-str
        (string-join
         (for/list ([db-dir (in-list db-dirs)])
           (~a (find-relative-path data-dir
                                   (hash-ref temp-dirs db-dir))))))
      (system @~a{tar -czf @temp-db-name @temp-dirs-str}))
    (rename-file-or-directory (build-path data-dir temp-db-name)
                              dbs-path)]
   [load?
    (for ([db-dir (in-list db-dirs)]
          #:when (directory-exists? db-dir))
      (cond [(user-prompt! @~a{
                               @db-dir already exists, @;
                               overwrite it?
                               })
             (delete-directory/files db-dir)]
            [else
             (eprintf "Canceling~n")
             (exit 1)]))

    (copy-file dbs-path
               (build-path data-dir temp-db-name))
    (parameterize ([current-directory data-dir])
      (system @~a{tar -xzmf @temp-db-name}))
    (for ([db-dir (in-list db-dirs)])
      (rename-file-or-directory (hash-ref temp-dirs db-dir)
                                db-dir
                                #t))])
 (delete-directory/files data-dir))
