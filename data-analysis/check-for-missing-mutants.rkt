#lang at-exp rscript

(require "../configurables/mutant-sampling/sample-within-mutators.rkt"
         (prefix-in db: "../db/db.rkt")
         "read-data.rkt"
         "../mutation-analysis/mutation-analysis-summaries.rkt")

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
         (define missing-mutants+reasons
           (missing-mutant-reasons missing-mutants
                                   log-path))
         (define extra-mutant-count
           (length (set-subtract benchmark-mutants expected-mutants)))
         @~a{
             Mutant sets differ: @;
             @(if (zero? extra-mutant-count) "" (~a extra-mutant-count "extra and")) @;
             missing:
             @(pretty-format missing-mutants+reasons 100)
             }]))

(define (missing-mutant-reasons mutants log-path)
  (define ag/grep (or (find-executable-path "ag")
                      (find-executable-path "grep")))
  (define discarded-mutant-lines
    (system/string @~a{@ag/grep 'Mutant.+has no type error. discarding' @log-path}))

  (for/list ([missing-mutant (in-list mutants)])
    (match-define (mutant _ mod index) missing-mutant)
    (define reason
      (cond [(string-contains? discarded-mutant-lines
                               @~a{@mod @"@" @index})
             "discarded because top has no type error"]
            [else "?"]))
    (list missing-mutant reason)))

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
 #:arguments {[_ dirs-to-check]
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
              #:args data-dirs-to-check}

 (file-stream-buffer-mode (current-output-port) 'line)

 (define mutants-by-mutator (read-mutants-by-mutator (mutation-analysis-summaries-db)))
 (define samples-db (db:get (mutation-analysis-samples-db)))
 (for ([dir-to-check (in-list dirs-to-check)])
   (displayln (~a "⟶ " dir-to-check))
   (for ([bench-name (in-list benchmarks-to-check)])
     (display @~a{Checking @bench-name                     @"\r"})
     (define bench-samples (db:read samples-db bench-name))
     (define bench-data (read-data dir-to-check mutants-by-mutator))
     (define difference
       (check-mutants bench-name
                      bench-data
                      bench-samples
                      (build-path dir-to-check bench-name (~a bench-name ".log"))))
     (when difference
       (displayln
        @~a{@bench-name doesn't match. Reason: @difference})))))
