#lang at-exp rscript

(require "../configurables/mutant-sampling/sample-within-mutators.rkt"
         (prefix-in db: "../db/db.rkt")
         "read-data.rkt"
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../util/debug-mutant.rkt"
         "../runner/mutation-runner.rkt")

;; returns a string if difference, else #f
(define (check-mutants bench-name bench-data expected-samples log-path)
  (define expected-mutants
    (for*/list ([{mod-name indices} (in-hash expected-samples)]
                [index (in-list indices)])
      (mutant bench-name mod-name index)))
  (define benchmark-mutants
    (remove-duplicates
     (filter-map (match-lambda [(blame-trail (and id (mutant (== bench-name) _ _))
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
              @(if (zero? extra-mutant-count) "" (~a extra-mutant-count "extra and")) @;
              @(length missing-mutants) missing:
              })
         (missing-mutant-reasons missing-mutants
                                 log-path)
         (newline)
         (newline)]))

(define (missing-mutant-reasons mutants log-path)
  (define ag/grep (or (find-executable-path "ag")
                      (find-executable-path "grep")))
  (define discarded-mutant-lines
    (system/string @~a{@ag/grep 'Mutant.+has no type error. discarding' @log-path}))

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

(define benchmarks-to-check
  '("suffixtree"
    "kcfa"
    "snake"
    "take5"
    "acquire"
    "tetris"
    "synth"
    "gregor"
    "quadT"
    "quadU"))

(define-runtime-paths
  [me "check-for-missing-mutants.rkt"])
(main
 #:arguments {[(hash-table ['interactive? interactive?]
                           ['parallel?    parallel?]
                           _ ...)
               dirs-to-check]
              #:once-each
              [("-s" "--samples-db")
               'samples-db
               ("Samples db to check against."
                @~a{Default: @(mutation-analysis-samples-db)})
               #:collect ["path"
                          (set-parameter mutation-analysis-samples-db)
                          (mutation-analysis-samples-db)]]
              [("-S" "--summaries-db")
               'summaries-db
               ("Summaries db to check against."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect ["path"
                          (set-parameter mutation-analysis-summaries-db)
                          (mutation-analysis-summaries-db)]]
              [("-i" "--interactive")
               'interactive?
               ("Display which benchmarks are being checked while running."
                "This doesn't work when -p is specified and you provide more than one directory.")
               #:record]
              [("-p" "--parallel")
               'parallel?
               "Check each directory in parallel?"
               #:record]
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
                              (mutation-analysis-samples-db)
                              "-S"
                              (mutation-analysis-summaries-db)
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
        (file-stream-buffer-mode (current-output-port) 'line)

        (define mutants-by-mutator (read-mutants-by-mutator (mutation-analysis-summaries-db)))
        (define samples-db (db:get (mutation-analysis-samples-db)))
        (for ([dir-to-check (in-list dirs-to-check)])
          (displayln (~a "⟶ " dir-to-check))
          (for ([bench-name (in-list benchmarks-to-check)])
            (when interactive?
              (display @~a{Checking @bench-name                     @"\r"}))
            (define bench-samples (db:read samples-db bench-name))
            (define bench-data (read-data dir-to-check mutants-by-mutator))
            (check-mutants bench-name
                           bench-data
                           bench-samples
                           (build-path dir-to-check bench-name (~a bench-name ".log")))))]))
