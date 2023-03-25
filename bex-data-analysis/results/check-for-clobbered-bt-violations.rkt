#lang at-exp rscript

(require bex/configurables/mutant-sampling/use-pre-selected-samples
         (prefix-in db: bex/db/db)
         "read-data.rkt"
         bex/mutation-analysis/mutation-analysis-summaries)

(define (check-for-possible-clobbered-bt-violations bench-data-files)
  (define log-path (benchmark-data-files-log bench-data-files))
  (define experiment-start-lines
    (string-split (system/string @~a{grep -E 'Running on benchmark' @log-path})
                  "\n"))
  (unless (> (length experiment-start-lines) 0)
    (displayln @~a{@log-path doesn't have experiment start log line?}))
  (define resumed?
    (not (string=? (system/string @~a{grep -E 'Resuming sampling of mutant' @log-path})
                   "")))
  (define log-clobbered?
    (and resumed? (> (length experiment-start-lines) 1)))

  (define no-bt-violations-logged?
    (string=? (system/string @~a{
                                 grep -E '(@;
                                           (Unable to continue following blame trail)|@;
                                           (BT VIOLATION))' @;
                                 @log-path
                                 })
              ""))

  (when (and log-clobbered? no-bt-violations-logged?)
    (displayln
     @~a{
         @(benchmark-data-files-name bench-data-files) was restarted without preserving the log @;
         so there may be missing BT violations
         })))

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

(main
 #:arguments {[(hash-table ['interactive? interactive?]
                           _ ...)
               dirs-to-check]
              #:once-each
              [("-s" "--samples-db")
               'samples-db
               ("Samples db to check against."
                @~a{Default: @(pre-selected-mutant-samples-db)})
               #:collect ["path"
                          (set-parameter pre-selected-mutant-samples-db)
                          (pre-selected-mutant-samples-db)]]
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
              #:args data-dirs-to-check}

 (file-stream-buffer-mode (current-output-port) 'line)

 (for ([dir-to-check (in-list dirs-to-check)])
   (displayln (~a "⟶ " dir-to-check))
   (define bench-data-files (find-data-files dir-to-check))
   (for ([a-benchmarks-data-files (in-list bench-data-files)])
     (define bench-name (benchmark-data-files-name a-benchmarks-data-files))
     (when interactive?
       (display @~a{Checking @bench-name                     @"\r"}))
     (check-for-possible-clobbered-bt-violations a-benchmarks-data-files))))
