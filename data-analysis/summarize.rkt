#lang at-exp rscript

(require "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../configurables/configurables.rkt"
         "../experiment/blame-trail-data.rkt"
         "../runner/mutation-runner-data.rkt"
         "../util/for.rkt"

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
  (define stat:start-outcome-counts (start-outcome-counts all-bts))
  (define stat:end-outcome-counts (end-outcome-counts all-bts))
  (define stat:trails-ending-with-no-blame
    (trails-ending-with (match-lambda** [{(struct* run-status ([blamed (or #f '())]
                                                               [outcome 'blamed]))
                                          _}
                                         #t]
                                        [{_ _} #f])
                        all-bts))
  (define stat:runtime-err-search-failures
    (trails-ending-with (match-lambda** [{(struct* run-status ([context-stack ctx]
                                                               [outcome 'runtime-error]))
                                          config}
                                         (for/and ([{mod level} (in-hash config)]
                                                   #:when (member mod ctx))
                                           (equal? level 'types))]
                                        [{_ _} #f])
                        all-bts))
  (define stat:blame-but-none-in-program
    (trails-ending-with (match-lambda** [{(struct* run-status ([blamed blamed]
                                                               [outcome 'blamed]))
                                          config}
                                         (set-empty? (set-intersect blamed (hash-keys config)))]
                                        [{_ _} #f])
                        all-bts))
  (define stat:resource-limits (summarize-resource-limit-occurrences all-bts))
  (define stat:0-length-bts (summarize-0-length-trails all-bts))
  (define stat:bt-failure-outcomes (summarize-bt-failure-outcomes all-bts))

  (define stat:bt-failures-blaming-typed-code (bt-failures-blaming-typed-code all-bts))

  (define stat:blamed-sizes (summarize-blamed-sizes all-bts))
  (define stat:blamed-sizes-untyped (summarize-blamed-sizes all-bts #:filter-by-mod-level 'none))

  (hash 'total-bt-count stat:total-bt-count
        'total-mutant-count stat:mutant-count
        'outcomes stat:outcome-counts
        'start-outcomes stat:start-outcome-counts
        'end-outcomes stat:end-outcome-counts
        'trails-ending-with-empty-blamed stat:trails-ending-with-no-blame
        'stack-search-failures stat:runtime-err-search-failures
        'blame-but-none-in-program stat:blame-but-none-in-program
        'resource-limits stat:resource-limits
        '0-length-bts stat:0-length-bts
        'bt-failure-outcomes stat:bt-failure-outcomes
        'bt-failures-blaming-typed-code stat:bt-failures-blaming-typed-code
        'blamed-sizes stat:blamed-sizes
        'blamed-sizes-untyped stat:blamed-sizes-untyped))

(define (count-outcomes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when #t
                  [mutant (in-list (blame-trail-mutant-summaries bt))])
    #:combine +
    #:default 0
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _) mutant)
    (values outcome 1)))

(define (start-outcome-counts bts)
  (for/hash/fold ([bt (in-list bts)])
    #:combine +
    #:default 0
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _)
      (last (blame-trail-mutant-summaries bt)))
    (values outcome 1)))

(define (end-outcome-counts bts)
  (for/hash/fold ([bt (in-list bts)])
    #:combine +
    #:default 0
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _)
      (first (blame-trail-mutant-summaries bt)))
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

(define (trails-ending-with predicate bts)
  (filter (match-lambda
            [(struct* blame-trail
                      ([mutant-summaries
                        (list* (mutant-summary _
                                               rs
                                               config)
                               _)]))
             (predicate rs config)]
            [else #f])
          bts))

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
      [(mutant-summary _
                       (struct* run-status ([outcome 'runtime-error]
                                            [context-stack (? list? mods)]))
                       config)
       #:when (for/and ([blamed-mod (in-list mods)])
                (equal? 'types (hash-ref config blamed-mod 'types)))
       @~a{runtime-error with only typed mod(s) on stack}]
      [else "other?"]))

  (define (0-length? bt) (= (length (blame-trail-mutant-summaries bt)) 1))
  (for/hash/fold ([bt (in-list bts)]
                  #:when (0-length? bt))
                 #:combine cons
                 #:default empty
                 (define reason (0-length-trail-reason bt))
                 (values reason bt)))

(define (summarize-resource-limit-occurrences bts)
  (hash 'timeout (trails-ending-with
                  (match-lambda** [{(struct* run-status ([outcome 'timeout])) _} #t]
                                  [{_ _} #f])
                  bts)
        'oom (trails-ending-with
              (match-lambda** [{(struct* run-status ([outcome 'oom])) _} #t]
                              [{_ _} #f])
              bts)))

(define (summarize-bt-failure-outcomes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when (not (satisfies-BT-hypothesis? bt)))
    #:combine cons
    #:default empty
    (match-define (struct* blame-trail
                           ([mutant-summaries
                             (list* (struct* mutant-summary
                                             ([run-status (struct* run-status
                                                                   ([outcome outcome]))]))
                                    _)]))
      bt)
    (values outcome bt)))

(define (summarize-blamed-sizes bts #:filter-by-mod-level [mod-level #f])
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
                 (define untyped-blamed-mods
                   (if mod-level
                       (set-intersect unique-blamed-mods
                                      (for/list ([{mod level} (in-hash config)]
                                                 #:when (equal? level mod-level))
                                        mod))
                       unique-blamed-mods))
                 (values (length untyped-blamed-mods) mutant)))

(define (format-summary summary)
  (match-define (hash-table ['total-bt-count stat:total-bt-count]
                            ['total-mutant-count stat:mutants]
                            ['outcomes stat:outcome-counts]
                            ['start-outcomes stat:start-outcome-counts]
                            ['end-outcomes stat:end-outcome-counts]
                            ['trails-ending-with-empty-blamed stat:trails-ending-with-empty-blamed]
                            ['stack-search-failures stat:runtime-err-search-failures]
                            ['blame-but-none-in-program stat:blame-but-none-in-program]
                            ['resource-limits stat:resource-limits]
                            ['0-length-bts stat:0-length-bts]
                            ['bt-failure-outcomes stat:bt-failure-outcomes]
                            ['bt-failures-blaming-typed-code stat:bt-failures-blaming-typed-code]
                            ['blamed-sizes stat:blamed-sizes]
                            ['blamed-sizes-untyped stat:blamed-sizes-untyped])
    summary)
  (define failing-trail-count (apply + (map length (hash-values stat:bt-failure-outcomes))))
  (define trails-ending-with-empty-blamed-count (length stat:trails-ending-with-empty-blamed))
  (define runtime-err-disappearances-count (length stat:runtime-err-search-failures))
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

      Total run outcome counts:                    @(pretty-format/indent stat:outcome-counts 45)

      Blamed mod list size counts:                 @(pretty-format/indent
                                                     (summary->counts stat:blamed-sizes)
                                                     45)
      Blamed untyped mod list size counts:         @(pretty-format/indent
                                                     (summary->counts stat:blamed-sizes-untyped)
                                                     45)

      Trail starting outcome counts:               @(pretty-format/indent stat:start-outcome-counts 45)
      Trail ending outcome counts:                 @(pretty-format/indent stat:end-outcome-counts 45)

      Total blame trail failures:                  @failing-trail-count

      Blame trail failures by outcome:             @(pretty-format/indent
                                                     (summary->counts stat:bt-failure-outcomes)
                                                     45)

      Failed trails ending with...
      blame on typed code:                         @(length stat:bt-failures-blaming-typed-code)
      blame, but empty:                            @trails-ending-with-empty-blamed-count
      blame outside program:                       @blamed-but-empty-count
      runtime-error + empty (filtered) stack:      @runtime-err-disappearances-count
      resource limits:                             @resource-limit-disappearances-count

      ... remaining trail failures:                @(- failing-trail-count
                                                       (length stat:bt-failures-blaming-typed-code)
                                                       trails-ending-with-empty-blamed-count
                                                       runtime-err-disappearances-count
                                                       blamed-but-empty-count
                                                       resource-limit-disappearances-count)

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
