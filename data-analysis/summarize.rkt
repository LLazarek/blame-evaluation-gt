#lang at-exp rscript

(require "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../configurables/configurables.rkt"
         "../experiment/blame-trail-data.rkt"
         "../runner/mutation-runner-data.rkt"

         "plot-common.rkt"
         "read-data.rkt")

(define summary/c
  (hash/c symbol? any/c))

;; path-to-existant-directory? . -> . summary/c
(define (summarize data-dir)
  (define blame-trails-by-mutator/across-all-benchmarks
    (directory->blame-trails-by-mutator/across-all-benchmarks
     data-dir
     #:summaries-db (mutation-analysis-summaries-db)))

  (define all-bts (append* (hash-values blame-trails-by-mutator/across-all-benchmarks)))

  (define stat:total-bt-count
    (length all-bts))
  (define stat:mutant-count
    (length (group-by blame-trail-mutant-id all-bts)))
  (define stat:outcome-counts (count-outcomes all-bts))
  (define stat:blame-disappearances (blame-disappearances all-bts))
  (define stat:runtime-err-inference-failures
    (blame-disappearances stat:blame-disappearances
                          #:outcome 'runtime-error))
  (define stat:blame-but-none-in-program (blame-disappearances stat:blame-disappearances
                                                               #:outcome 'blamed))
  (define stat:resource-limits (summarize-resource-limit-occurrences stat:blame-disappearances))
  (define stat:0-length-bts (summarize-0-length-trails all-bts))
  (define stat:bt-failure-outcomes (summarize-bt-failure-outcomes all-bts))

  (define stat:bt-failures-blaming-typed-code (bt-failures-blaming-typed-code all-bts))

  (define stat:blamed-sizes (summarize-blamed-sizes all-bts))

  (hash 'total-bt-count stat:total-bt-count
        'total-mutant-count stat:mutant-count
        'outcomes stat:outcome-counts
        'blame-disappearances stat:blame-disappearances
        'stack-inference-failures stat:runtime-err-inference-failures
        'blame-but-none-in-program stat:blame-but-none-in-program
        'resource-limits stat:resource-limits
        '0-length-bts stat:0-length-bts
        'bt-failure-outcomes stat:bt-failure-outcomes
        'bt-failures-blaming-typed-code stat:bt-failures-blaming-typed-code
        'blamed-sizes stat:blamed-sizes))

(define (count-outcomes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when #t
                  [mutant (in-list (blame-trail-mutant-summaries bt))])
                 #:combine +
                 #:default 0
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _) mutant)
    (values outcome 1)))

(define (bt-failures-blaming-typed-code bts)
  (filter-not false?
              (for/list ([bt (in-list bts)])
                (match (first (blame-trail-mutant-summaries bt))
                  [(mutant-summary _
                                   (struct* run-status ([outcome 'blamed]
                                                        [blamed (? list? blamed)]))
                                   config)
                   (define blamed-in-program
                     (filter (λ (mod) (hash-has-key? config mod)) blamed))
                   (and (not (empty? blamed-in-program))
                        (for/and ([blamed-mod (in-list blamed-in-program)])
                          (equal? (hash-ref config blamed-mod) 'types))
                        bt)]
                  [else #f]))))

(define (blame-disappearances bts #:outcome [outcome #f])
  (define (outcome-ok? o) (or (false? outcome) (equal? o outcome)))
  (define blame-disappeared?
    (match-lambda
      [(struct* blame-trail
                ([mutant-summaries
                  (list* (mutant-summary _
                                         (struct* run-status ([blamed (or #f '())]
                                                              [outcome (? outcome-ok?)]))
                                         _)
                         _)]))
       #t]
      [else #f]))
  (filter blame-disappeared? bts))

(define (summarize-0-length-trails bts)
  (define (0-length-trail-reason bt)
    (define last-mutant (first (blame-trail-mutant-summaries bt)))
    (match last-mutant
      [(mutant-summary _
                       (struct* run-status ([outcome 'blamed]
                                            [blamed '()]))
                       _)
       "blame disappeared"]
      [(mutant-summary _
                       (struct* run-status ([outcome outcome]
                                            [blamed (and (not '()) (? list? mods))]))
                       config)
       #:when (for/and ([blamed-mod (in-list mods)])
                (equal? 'types (hash-ref config blamed-mod 'types)))
       @~a{@outcome blame on wrong typed mod(s)}]
      [(mutant-summary _
                       (struct* run-status ([outcome (and limit-type (or 'timeout 'oom))]))
                       _)
       (~a limit-type)]
      [else "other?"]))

  (define (0-length? bt) (= (length (blame-trail-mutant-summaries bt)) 1))
  (for/hash/fold ([bt (in-list bts)]
                  #:when (0-length? bt))
                 #:combine cons
                 #:default empty
                 (define reason (0-length-trail-reason bt))
                 (values reason bt)))

(define (summarize-resource-limit-occurrences bts)
  (hash 'timeout (blame-disappearances bts #:outcome 'timeout)
        'oom (blame-disappearances bts #:outcome 'oom)))

(define (summarize-bt-failure-outcomes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when (not (satisfies-BT-hypothesis? bt)))
                 #:combine cons
                 #:default empty
                 (match-define (struct* blame-trail
                                        ([mutant-summaries
                                          (list* (struct* mutant-summary
                                                          ([run-status
                                                            (struct* run-status
                                                                     ([outcome outcome]))]))
                                                 _)]))
                   bt)
                 (values outcome bt)))

(define (summarize-blamed-sizes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when #t
                  [mutant (in-list (blame-trail-mutant-summaries bt))]
                  #:when (equal? (run-status-outcome (mutant-summary-run-status mutant))
                                 'blamed))
                 #:combine cons
                 #:default empty
                 (match-define (struct* mutant-summary
                                        ([run-status
                                          (struct* run-status
                                                   ([outcome 'blamed]
                                                    [blamed blamed-mods]))]
                                         [config config]))
                   mutant)
                 (define unique-blamed-mods (remove-duplicates blamed-mods))
                 (define untyped-blamed-mods (set-intersect unique-blamed-mods
                                                            (for/list ([{mod level} (in-hash config)]
                                                                       #:when (equal? level 'none))
                                                              mod)))
                 (values (length untyped-blamed-mods) mutant)))

(define (format-summary summary)
  (match-define (hash-table ['total-bt-count stat:total-bt-count]
                            ['total-mutant-count stat:mutants]
                            ['outcomes stat:outcome-counts]
                            ['blame-disappearances stat:blame-disappearances]
                            ['stack-inference-failures stat:runtime-err-inference-failures]
                            ['blame-but-none-in-program stat:blame-but-none-in-program]
                            ['0-length-bts stat:0-length-bts]
                            ['resource-limits stat:resource-limits]
                            ['bt-failure-outcomes stat:bt-failure-outcomes]
                            ['bt-failures-blaming-typed-code stat:bt-failures-blaming-typed-code]
                            ['blamed-sizes stat:blamed-sizes])
    summary)
  (define blame-disappearances-count (length stat:blame-disappearances))
  (define runtime-err-disappearances-count (length stat:runtime-err-inference-failures))
  (define resource-limit-disappearances-count (apply + (map length (hash-values stat:resource-limits))))
  (define blamed-but-empty-count (length stat:blame-but-none-in-program))
  (define (summary->counts hash)
    (for/hash ([{k bts} (in-hash hash)])
      (values k (length bts))))
  (define (pretty-format/indent v indent)
    (define str (pretty-format v))
    (string-replace str "\n" (~a "\n" (make-string indent #\space))))
  @~a{
      Total mutants:                               @stat:mutants
      Total blame trails:                          @stat:total-bt-count

      With outcome counts:                         @(pretty-format/indent stat:outcome-counts 45)

      Blamed mod list size counts:                 @(pretty-format/indent
                                                     (summary->counts stat:blamed-sizes)
                                                     45)

      # NOTE: in stack modes, the outcomes `blamed` and `runtime-error` indicate which kind of
      # error happened, but the blame always comes from the stack. So "blaming" in below stats
      # is not really the right word...
      Blame trail failures by outcome:             @(pretty-format/indent
                                                     (summary->counts stat:bt-failure-outcomes)
                                                     45)

      ... blaming typed code:                      @(length stat:bt-failures-blaming-typed-code)

      Blame disappearances:                        @blame-disappearances-count
        of which, due to stack inference failure:  @runtime-err-disappearances-count
        of which, with blamed but none in program: @blamed-but-empty-count
        of-which, due to resource limits:          @resource-limit-disappearances-count
          breakdown:                               @(pretty-format/indent
                                                     (summary->counts stat:resource-limits)
                                                     45)
      ... remaining:                               @(- blame-disappearances-count
                                                       runtime-err-disappearances-count
                                                       resource-limit-disappearances-count
                                                       blamed-but-empty-count)

      0-length blame trails:                       @(pretty-format/indent
                                                     (summary->counts stat:0-length-bts)
                                                     45)
      })

(main
 #:arguments ([(hash-table ['config   config-path]
                           ['summaries-db _]
                           ['run-checks? run-checks?]
                           ['dump-values values-to-dump])
               data-dirs]
              #:once-each
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-s" "--mutant-summaries")
               'summaries-db
               ("Path to the db containing summaries of the mutants in the data."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect {"path"
                          (set-parameter mutation-analysis-summaries-db)
                          (mutation-analysis-summaries-db)}
               #:mandatory]
              [("-C" "--check")
               'run-checks?
               ("Run integrity checks in addition to summarizing the data.")
               #:record]
              #:multi
              [("-D" "--dump")
               'dump-values
               ("Dump a given summary value to stdout after the summary.")
               #:collect {"summary value key" cons empty}]
              #:args data-dirs)
 #:check [(andmap path-to-existant-directory? data-dirs)
          @~a{Can't find @(filter-not path-to-existant-directory? data-dirs)}]
 (install-configuration! config-path)

 (for ([data-dir (in-list data-dirs)])
   (displayln @~a{-------------------- @(basename data-dir) --------------------})
   (define summary (summarize data-dir))
   (displayln (format-summary summary))
   (newline)
   (when run-checks?
     (define missing-mutants-output (open-output-string))
     (define errors-output (open-output-string))
     (parameterize ([current-output-port missing-mutants-output]
                    [current-error-port missing-mutants-output])
       (system* "/usr/bin/fish"
                "-c"
                @~a{
                    rt check-for-missing-mutants-or-trails.rkt -s ../dbs/code-mutations/mutant-samples.rktdb -S @(mutation-analysis-summaries-db) -r ../dbs/code-mutations/pre-selected-bt-roots.rktdb -p '@data-dir'
                    }))
     (parameterize ([current-output-port errors-output]
                    [current-error-port errors-output])
       (system* "/usr/bin/fish"
                "-c"
                @~a{
                    rt check-for-errors.rkt '@data-dir'
                    }))
     (displayln @~a{
                    check-for-missing-mutants:
                    @(get-output-string missing-mutants-output)

                    check-for-errors:
                    @(get-output-string errors-output)


                    }))
   (newline)
   (unless (empty? values-to-dump)
     (displayln "-------------------- data dump --------------------")
     (pretty-write (for/hash ([key (in-list (map string->symbol values-to-dump))])
                     (values key (hash-ref summary key)))))))
