#lang at-exp rscript

(define-runtime-paths
  [summaries-dir "../mutation-analysis/summaries"]
  [mutations-dir "../configurables/mutant-sampling/samples"])

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
 (define temp-summaries-dir (build-path data-dir (basename summaries-dir)))
 (define temp-mutations-dir (build-path data-dir (basename mutations-dir)))
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
    (copy-directory/files summaries-dir
                          temp-summaries-dir)
    (copy-directory/files mutations-dir
                          temp-mutations-dir)
    (parameterize ([current-directory data-dir])
      (system @~a{
                  tar -czf @temp-db-name @;
                  @(find-relative-path data-dir
                                       temp-summaries-dir) @;
                  @(find-relative-path data-dir
                                       temp-mutations-dir)
                  }))
    (rename-file-or-directory (build-path data-dir temp-db-name)
                              dbs-path)]
   [load?
    (when (directory-exists? summaries-dir)
      (cond [(user-prompt! @~a{
                                      @summaries-dir already exists, @;
                                      overwrite it?
                                      })
             (delete-directory/files summaries-dir)]
            [else
             (eprintf "Canceling~n")
             (exit 1)]))
    (when (directory-exists? mutations-dir)
      (cond [(user-prompt! @~a{
                                      @mutations-dir already exists, @;
                                      overwrite it?
                                      })
             (delete-directory/files mutations-dir)]
            [else
             (eprintf "Canceling~n")
             (exit 1)]))

    (copy-file dbs-path
               (build-path data-dir temp-db-name))
    (parameterize ([current-directory data-dir])
      (system @~a{tar -xzf @temp-db-name}))
    (rename-file-or-directory temp-summaries-dir
                              summaries-dir
                              #t)
    (rename-file-or-directory temp-mutations-dir
                              mutations-dir
                              #t)])
 (delete-directory/files data-dir))
