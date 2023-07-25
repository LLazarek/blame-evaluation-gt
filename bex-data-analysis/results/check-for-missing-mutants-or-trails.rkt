#lang at-exp rscript

(require bex/configurables/mutant-sampling/use-pre-selected-samples
         bex/configurables/bt-root-sampling/pre-selected
         bex/configurables/configurables
         (prefix-in db: bex/db/db)
         "read-data.rkt"
         bex/mutation-analysis/mutation-analysis-summaries
         bex/util/debug-mutant
         bex/runner/mutation-runner
         bex/experiment/blame-trail-data
         "experiment-info.rkt")

(define (check-mutants bench-name bench-data expected-samples log-path)
  (define expected-mutants
    (for*/list ([{mod-name indices} (in-hash expected-samples)]
                [index (in-list indices)])
      (mutant bench-name mod-name index)))
  (define benchmark-mutants
    (remove-duplicates
     (filter-map (match-lambda [(blame-trail (and id (mutant (== bench-name) _ _))
                                             _
                                             _
                                             _)
                                id]
                               [else #f])
                 (flatten (hash-values bench-data)))))
  (cond [(set=? expected-mutants benchmark-mutants)
         #f]
        [else
         (define missing-mutants
           (set-subtract expected-mutants benchmark-mutants))
         (define extra-mutant-count
           (length (set-subtract benchmark-mutants expected-mutants)))
         (displayln
          @~a{
              @bench-name mutant sets differ: @;
              @(if (zero? extra-mutant-count) "" (~a extra-mutant-count " extra and")) @;
              @(length missing-mutants) missing:
              })
         (missing-mutant-reasons missing-mutants
                                 log-path)
         (newline)
         (newline)]))

(define (check-blame-trails bench-name bench-data expected-bt-samples)
  (define expected-mutants (hash-keys expected-bt-samples))
  (define expected-bt-count
    (for/sum ([{mutant root-configs} (in-hash expected-bt-samples)])
      (length root-configs)))
  (define (expected-mutant? m)
    (and (string=? (mutant-benchmark m) bench-name)
         (member (struct-copy mutant m [benchmark #f])
                 expected-mutants)))
  (define actual-bt-count
    (count
     (compose1 expected-mutant? blame-trail-mutant-id)
     (flatten (hash-values bench-data))))
  (unless (= actual-bt-count expected-bt-count)
    (displayln
     @~a{
         @bench-name Has missing blame trails.
         There are @expected-bt-count bt roots in the db, but only @actual-bt-count bts in the data
         (of which @;
             @(count
               (compose1 expected-mutant? blame-trail-mutant-id)
               (remove-duplicates (map (λ (bt1)
                                         (define (normalize-mutant-summary summary1)
                                           (struct-copy mutant-summary summary1
                                                        [id 0]))
                                         (struct-copy blame-trail bt1
                                                      [mutant-summaries
                                                       (map normalize-mutant-summary
                                                            (blame-trail-mutant-summaries bt1))]))
                                       (flatten (hash-values bench-data))))) @;
             are unique)

         })))

(define (missing-mutant-reasons mutants log-path)
  (define pattern "Mutant.+has no type error. discarding")
  (define discarded-mutant-lines
    (cond [(find-executable-path "ag")
           => (λ (ag) (system/string @~a{@ag '@pattern' @log-path}))]
          [(find-executable-path "grep")
           => (λ (grep) (system/string @~a{@grep -E '@pattern' @log-path}))]
          [else
           (raise-user-error 'check-for-missing-mutants
                             "Can't find ag or grep")]))

  (for ([missing-mutant (in-list mutants)])
    (match-define (mutant benchmark mod index) missing-mutant)
    (define reason
      (cond [(string-contains? discarded-mutant-lines
                               @~a{@mod @"@" @index})
             @~a{
                 discarded because top has no type error: @;
                 TR produces @(mutant-outcome missing-mutant "TR"), @;
                 transient produces @(mutant-outcome missing-mutant "transient-oldest")
                 }]
            [else "?"]))
    (displayln (list missing-mutant reason))))

(define (mutant-outcome a-mutant mode/config)
  (match-define (mutant benchmark mod index) a-mutant)
  (run-status-outcome (debug-mutant benchmark
                                    mod
                                    index
                                    'types
                                    #:run? #t
                                    #:config mode/config
                                    #:interactive? #f)))

(define-runtime-paths
  [me "check-for-missing-mutants-or-trails.rkt"])
(main
 #:arguments {[(hash-table ['interactive? interactive?]
                           ['parallel?    parallel?]
                           ['config       config-path]
                           ['mutant-samples-db mutant-samples-db-path]
                           ['summaries-db summaries-db-path]
                           ['bt-root-db bt-root-db-path]
                           _ ...)
               dirs-to-check]
              #:once-each
              [("-s" "--samples-db")
               'mutant-samples-db
               ("Mutant samples db to check against.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-S" "--summaries-db")
               'summaries-db
               ("Summaries db to check against.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-r" "--bt-root-samples-db")
               'bt-root-db
               ("BT root sample db to check against.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-i" "--interactive")
               'interactive?
               ("Display which benchmarks are being checked while running."
                "This doesn't work when -p is specified and you provide more than one directory.")
               #:record]
              [("-p" "--parallel")
               'parallel?
               "Check each directory in parallel?"
               #:record]
              [("-c" "--config")
               'config
               ("Config for deserializing configurations.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              #:args data-dirs-to-check}

 (cond [(and parallel?
             (> (length dirs-to-check) 1))
        (define racket (find-system-path 'exec-file))
        (define output-ports+ctls-for-each-dir
          (for/list ([dir (in-list dirs-to-check)])
            (match-define (list stdout stdin _ #f ctl)
              (process*/ports #f #f 'stdout
                              racket
                              me
                              "-s"
                              mutant-samples-db-path
                              "-S"
                              summaries-db-path
                              "-r"
                              bt-root-db-path
                              "-c"
                              config-path
                              dir))
            (close-output-port stdin)
            (list stdout ctl)))
        (let loop ([remaining-processes output-ports+ctls-for-each-dir])
          (if (empty? remaining-processes)
              (void)
              (loop
               (for/fold ([still-alive empty])
                         ([process-output+ctl (in-list remaining-processes)])
                 (cond [(equal? ((second process-output+ctl) 'status) 'running)
                        (cons process-output+ctl still-alive)]
                       [else
                        (define output-string
                          (port->string (first process-output+ctl)))
                        (close-input-port (first process-output+ctl))
                        (displayln output-string)
                        still-alive])))))]
       [else
        (install-configuration! config-path)
        (file-stream-buffer-mode (current-output-port) 'line)

        (define mutants-by-mutator (read-mutants-by-mutator summaries-db-path))
        (define mutant-samples-db (db:get mutant-samples-db-path))
        (define root-samples-db (db:get bt-root-db-path))
        (for ([dir-to-check (in-list dirs-to-check)])
          (displayln (~a "⟶ " dir-to-check))
          (for ([bench-name (in-list benchmarks)])
            (when interactive?
              (display @~a{Checking @bench-name                     @"\r"}))
            (define bench-mutant-samples (db:read mutant-samples-db bench-name))
            (define bench-bt-root-samples (db:read root-samples-db bench-name))
            (define bench-data (read-blame-trails-by-mutator/across-all-benchmarks dir-to-check mutants-by-mutator))
            (check-mutants bench-name
                           bench-data
                           bench-mutant-samples
                           (build-path dir-to-check bench-name (~a bench-name ".log")))
            (check-blame-trails bench-name
                                bench-data
                                bench-bt-root-samples)))]))
