#lang at-exp rscript

(provide summarize
         format-summary)

(require bex/mutation-analysis/mutation-analysis-summaries
         bex/configurables/configurables
         bex/experiment/blame-trail-data
         bex/runner/mutation-runner-data
         bex/util/for
         bex/configurables/program-instrumentation/type-interface-module-names

         "plot-common.rkt"
         "read-data.rkt"
         "bt-length-distributions.rkt"

         text-table
         syntax/parse/define)

(define-runtime-paths
  [check-for-missing-mutants-or-trails.rkt "check-for-missing-mutants-or-trails.rkt"]
  [check-for-errors.rkt "check-for-errors.rkt"])

(define summary/c
  (hash/c symbol? any/c))

;; path-to-existant-directory? . -> . summary/c
(define (summarize data-dir summaries-db-path)
  (define blame-trails-by-mutator/across-all-benchmarks
    (directory->blame-trails-by-mutator/across-all-benchmarks
     data-dir
     #:summaries-db summaries-db-path))

  (define all-bts (append* (hash-values blame-trails-by-mutator/across-all-benchmarks)))

  (define stat:total-bt-count
    (length all-bts))
  (define stat:mutant-count
    (length (group-by blame-trail-mutant-id all-bts)))
  (define stat:outcome-counts (count-outcomes all-bts))
  (define stat:start-outcome-summary (start-outcome-summary all-bts))
  (define stat:end-outcome-summary (end-outcome-summary all-bts))
  (define stat:bts-switching-runtime-error->blame (blame-switch-bts all-bts))
  (define stat:runtime-error-only-bts (runtime-error-only-bts all-bts))
  (define stat:trails-ending-with-no-blame
    (trails-ending-with (match-lambda** [{(struct* run-status ([blamed (or #f '())]
                                                               [outcome 'blamed]))
                                          _}
                                         #t]
                                        [{_ _} #f])
                        all-bts))
  (define stat:runtime-err-search-failures
    (trails-ending-with (match-lambda** [{(struct* run-status ([context-stack (? list? ctx)]
                                                               [outcome 'runtime-error]))
                                          config}
                                         (for/and ([{mod level} (in-hash config)]
                                                   #:when (member mod ctx))
                                           (equal? level 'types))]
                                        [{_ _} #f])
                        all-bts))
  (define stat:blame-but-none-in-program
    (trails-ending-with (match-lambda** [{(struct* run-status ([blamed (and (not (or #f '()))
                                                                            blamed)]
                                                               [outcome 'blamed]))
                                          config}
                                         (set-empty? (set-intersect blamed
                                                                    ;; lltodo: this is kind of a hack. Really the right thing to do is to get ahold of the benchmark modules with benchmark->mutatable-modules
                                                                    (list* type-interface-file-name
                                                                           type-interface-file-rename
                                                                           (hash-keys config))))]
                                        [{_ _} #f])
                        all-bts))
  (define stat:resource-limits (summarize-resource-limit-occurrences all-bts))
  (define stat:0-length-bts (summarize-0-length-trails all-bts))
  (define stat:bt-failure-outcomes (summarize-bt-failure-outcomes all-bts))

  (define stat:bt-failures-blaming-typed-code (bt-failures-blaming-typed-code all-bts))

  (define stat:uncategorized-bt-failures (set-subtract (append* (hash-values stat:bt-failure-outcomes))
                                                       stat:trails-ending-with-no-blame
                                                       stat:bt-failures-blaming-typed-code
                                                       stat:runtime-err-search-failures
                                                       stat:blame-but-none-in-program
                                                       (append* (hash-values stat:resource-limits))))

  (define stat:blamed-sizes (summarize-blamed-sizes all-bts))
  (define stat:blamed-sizes-untyped (summarize-blamed-sizes all-bts #:filter-by-mod-level 'none))
  (define stat:root-stack-sizes (summarize-root-context-sizes all-bts))

  (hash 'total-bt-count stat:total-bt-count
        'total-mutant-count stat:mutant-count
        'outcomes stat:outcome-counts
        'start-outcomes stat:start-outcome-summary
        'end-outcomes stat:end-outcome-summary
        'bts-switching-runtime-error->blame stat:bts-switching-runtime-error->blame
        'runtime-error-only-bts stat:runtime-error-only-bts
        'trails-ending-with-empty-blamed stat:trails-ending-with-no-blame
        'stack-search-failures stat:runtime-err-search-failures
        'blame-but-none-in-program stat:blame-but-none-in-program
        'resource-limits stat:resource-limits
        '0-length-bts stat:0-length-bts
        'bt-failure-outcomes stat:bt-failure-outcomes
        'bt-failures-blaming-typed-code stat:bt-failures-blaming-typed-code
        'blamed-sizes stat:blamed-sizes
        'blamed-sizes-untyped stat:blamed-sizes-untyped
        'root-stack-sizes stat:root-stack-sizes
        'uncategorized-bt-failures stat:uncategorized-bt-failures))

(define (lengths-summary bts)
  (define (len bt) (bt-length bt #t))
  (for/hash ([group (in-list (group-by len bts))])
    (values (len (first group))
            (length group))))

(define (count-outcomes bts)
  (for/hash/fold ([bt (in-list bts)]
                  #:when #t
                  [mutant (in-list (blame-trail-mutant-summaries bt))])
    #:combine +
    #:default 0
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _) mutant)
    (values outcome 1)))

(define (start-outcome-summary bts)
  (for/hash/fold ([bt (in-list bts)])
    #:combine cons
    #:default empty
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _)
      (last (blame-trail-mutant-summaries bt)))
    (values outcome bt)))

(define (end-outcome-summary bts)
  (for/hash/fold ([bt (in-list bts)])
    #:combine cons
    #:default empty
    (match-define (mutant-summary _ (struct* run-status ([outcome outcome])) _)
      (first (blame-trail-mutant-summaries bt)))
    (values outcome bt)))

(define mutant-outcome (compose1 run-status-outcome mutant-summary-run-status))
(define (blame-switch-bts bts)
  (filter (λ (bt) (match (blame-trail-mutant-summaries bt)
                    [(list _ ...
                           (app mutant-outcome (== 'blamed))
                           _ ...
                           (app mutant-outcome (== 'runtime-error)))
                     #t]
                    [else #f]))
          bts))

(define (runtime-error-only-bts bts)
  (filter (λ (bt)
            (match (blame-trail-mutant-summaries bt)
              [(list (app mutant-outcome (or (== 'runtime-error) (== 'type-error)))
                     (app mutant-outcome (== 'runtime-error)) ..1)
               #t]
              [(list (app mutant-outcome (== 'runtime-error)) ...)
               #t]
              [else #f]))
          bts))

(define (bt-failures-blaming-typed-code bts)
  (filter-not false?
              (for/list ([bt (in-list bts)]
                         #:unless (satisfies-BT-hypothesis? bt))
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
       "blame disappeared (empty)"]
      [(mutant-summary _
                       (struct* run-status ([outcome outcome]
                                            [blamed (and (not '()) (? list? mods))]))
                       config)
       #:when (and (for/and ([blamed-mod (in-list mods)])
                     (equal? 'types (hash-ref config blamed-mod 'types)))
                   (not (equal? mods (list type-interface-file-name))))
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
      [(mutant-summary _
                       (struct* run-status ([outcome (and outcome (or 'type-error 'completed))]))
                       _)
       @~a{immediate @outcome}]
      [(mutant-summary _
                       (struct* run-status ([outcome 'blamed]
                                            [blamed blamed]))
                       _)
       #:when (not (set-empty? (set-intersect blamed (list type-interface-file-name
                                                           type-interface-file-rename))))
       @~a{immediate blame on interface}]
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

(define (summarize-root-context-sizes bts)
  (for/hash/fold ([bt (in-list bts)])
    #:combine cons
    #:default empty
    (match (blame-trail-mutant-summaries bt)
      [(list _ ... (struct* mutant-summary
                            ([run-status (struct* run-status
                                                  ([context-stack (? list? stack)]))])))
       (values (length (remove-duplicates stack)) bt)]
      [else (values 'N/A bt)])))

(define (default-summary-formatter v)
  (define (summary->counts hash)
    (for/hash ([{k bts} (in-hash hash)])
      (values k (length bts))))
  (match v
    [(hash-table [_ (? list?)] ...)
     (summary->counts v)]
    [(? list? l)
     (length l)]
    [other other]))

(begin-for-syntax
  (require syntax/parse)
  (define-splicing-syntax-class stat-row
    #:commit
    (pattern {~seq #:section section:str}
             #:with parts #'section)
    (pattern {~seq descr:str stat-id:id {~optional {~seq #:format formatter} #:defaults ([formatter #'default-summary-formatter])}}
             #:with parts #'(list 'stat-id descr formatter))))

(define-simple-macro (define-stat-table {table-id keys-id}
                       #:from stat-table:id
                       r:stat-row ...)
  (begin
    (define-values {temp table-id}
      (for/lists {l1 l2}
                 ([parts (list r.parts ...)]
                  [i (in-naturals)])
                 (if (string? parts)
                     (values #f
                             (list "\n\n=" (~a "\n\n" parts) "\n\n="))
                     (values (list i (first parts))
                             (list (~a "[" i "]")
                                   (~a (second parts) ":")
                                   ((third parts)
                                    (hash-ref stat-table (first parts))))))))
    (define keys-id (for/hash ([maybe-index+key (in-list temp)]
                               #:when maybe-index+key)
                      (values (first maybe-index+key)
                              (second maybe-index+key))))))

(define (format-summary summary #:with-keys [with-keys? #f])
  (match-define (hash-table ['runtime-error-only-bts stat:runtime-error-only-bts]
                            ['bt-failure-outcomes stat:bt-failure-outcomes]
                            ['resource-limits stat:resource-limits]
                            _ ...)
    summary)
  (define summary*
    (hash-set* summary
               'mutants-for-runtime-error-only-bts
               (remove-duplicates (map blame-trail-mutant-id stat:runtime-error-only-bts))


               'bt-failure-count
               (apply + (map length (hash-values stat:bt-failure-outcomes)))

               'resource-limit-failure-count
               (apply + (map length (hash-values stat:resource-limits)))))
  (define-stat-table {table index->key}
    #:from summary*
    #:section "Overall summary"
    "Total mutants"                                total-mutant-count
    "Total blame trails"                           total-bt-count
    "Total blame trail failures"                   bt-failure-count

    #:section "Failing bt stats"
    "Blame trail failures by outcome"              bt-failure-outcomes
    #:section "Failed trails ending with"
    "... blame on typed code"                      bt-failures-blaming-typed-code
    "... blame, but empty"                         trails-ending-with-empty-blamed
    "... blame outside program"                    blame-but-none-in-program
    "... runtime-error + empty (filtered) stack"   stack-search-failures
    "... resource limits"                          resource-limit-failure-count
    "... remaining trail failures"                 uncategorized-bt-failures

    #:section "General run stats"
    "Total run outcome counts"                     outcomes
    "Blamed list size distribution"                blamed-sizes
    "Untyped-only blamed list size distribution"   blamed-sizes-untyped

    #:section "General bt stats"
    "Trail starting outcomes"                      start-outcomes
    "Trail ending outcome counts"                  end-outcomes
    "Trails start w/ runtime-err, end w/ blame"    bts-switching-runtime-error->blame
    "... lengths"                                  bts-switching-runtime-error->blame #:format lengths-summary
    "Trails consisting of only runtime-errs"       runtime-error-only-bts
    "... lengths"                                  runtime-error-only-bts #:format lengths-summary
    "# of mutants having runtime-err-only trails"  mutants-for-runtime-error-only-bts
    "0-length blame trails"                        0-length-bts
    "Root stack sizes (w/o duplicates)"            root-stack-sizes)

  (define str (simple-table->string table
                                    #:->string (λ (v)
                                                 (pretty-format v 50))))
  (if with-keys?
      (values str index->key)
      str)

  ;; (define failing-trail-count )
  ;; (define trails-ending-with-empty-blamed-count (length stat:trails-ending-with-empty-blamed))
  ;; (define runtime-err-disappearances-count (length stat:runtime-err-search-failures))
  ;; (define resource-limit-disappearances-count )
  ;; (define blamed-but-empty-count (length stat:blame-but-none-in-program))
  ;; (define (summary->counts hash)
  ;;   (for/hash ([{k bts} (in-hash hash)])
  ;;     (values k (length bts))))
  ;; (define (pretty-format/indent v indent)
  ;;   (define str (pretty-format v))
  ;;   (string-replace str "\n" (~a "\n" (make-string indent #\space))))
  ;; @~a{
  ;;     [key] description:                               value
  ;;     ======================================================
  ;;     [_] Total mutants:                               @stat:mutants
  ;;     [0] Total blame trails:                          @stat:total-bt-count

  ;;     [_] Total run outcome counts:                    @(pretty-format/indent stat:outcome-counts 45)

  ;;     [1] Blamed mod list size counts:                 @(pretty-format/indent
  ;;                                                        (summary->counts stat:blamed-sizes)
  ;;                                                        45)
  ;;     [2] Blamed untyped mod list size counts:         @(pretty-format/indent
  ;;                                                        (summary->counts stat:blamed-sizes-untyped)
  ;;                                                        45)

  ;;     [3] Trail starting outcome counts:               @(pretty-format/indent
  ;;                                                        (summary->counts stat:start-outcome-summary)
  ;;                                                        45)
  ;;     [4] Trail ending outcome counts:                 @(pretty-format/indent
  ;;                                                        (summary->counts stat:end-outcome-summary)
  ;;                                                        45)
  ;;     [5] Trails start w/ runtime-err, end w/ blame:   @(length stat:bts-switching-runtime-error->blame)
  ;;     ... lengths:                                     @(pretty-format/indent
  ;;                                                        (lengths-summary stat:bts-switching-runtime-error->blame)
  ;;                                                        45)
  ;;     [6] Trails consisting of only runtime-errs:      @(length stat:runtime-error-only-bts)
  ;;     ... lengths:                                     @(pretty-format/indent
  ;;                                                        (lengths-summary stat:runtime-error-only-bts)
  ;;                                                        45)
  ;;     ... mutants for those trails:                @(length (remove-duplicates (map blame-trail-mutant-id stat:runtime-error-only-bts)))

  ;;     [_] Total blame trail failures:                  @failing-trail-count

  ;;     [7] Blame trail failures by outcome:             @(pretty-format/indent
  ;;                                                        (summary->counts stat:bt-failure-outcomes)
  ;;                                                        45)

  ;;     Failed trails ending with...
  ;;     [8] blame on typed code:                         @(length stat:bt-failures-blaming-typed-code)
  ;;     [9] blame, but empty:                            @trails-ending-with-empty-blamed-count
  ;;     [10]blame outside program:                       @blamed-but-empty-count
  ;;     [11]runtime-error + empty (filtered) stack:      @runtime-err-disappearances-count
  ;;     [12]resource limits:                             @resource-limit-disappearances-count

  ;;     [13]... remaining trail failures:                @(length stat:uncategorized-bt-failures)

  ;;     0-length blame trails:                       @(pretty-format/indent
  ;;                                                    (summary->counts stat:0-length-bts)
  ;;                                                    45)

  ;;     Root stack sizes (w/o duplicates):           @(pretty-format/indent
  ;;                                                    (summary->counts stat:root-stack-sizes)
  ;;                                                    45)
  ;;     }
  )

(main
 #:arguments ([(hash-table ['config   config-path]
                           ['summaries-db (app simple-form-path summaries-db-path)]
                           ['mutant-samples-db (or (and #f mutant-samples-db-path)
                                                   (app simple-form-path mutant-samples-db-path))]
                           ['root-samples-db (or (and #f root-samples-db-path)
                                                 (app simple-form-path root-samples-db-path))]
                           ['run-checks? run-checks?]
                           ['dump-values values-to-dump])
               (app (λ (l) (map simple-form-path l)) data-dirs)]
              #:once-each
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-S" "--mutant-summaries")
               'summaries-db
               ("Path to the db containing summaries of the mutants in the data."
                "(Typically, type-err-summaries.rktdb.)")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-s" "--mutant-samples")
               'mutant-samples-db
               ("Path to the db containing mutant samples."
                "Mandatory when -C provided.")
               #:collect {"path" take-latest #f}
               #:mandatory-unless (λ (flags) (not (member 'run-checks? flags)))]
              [("-r" "--bt-root-samples")
               'root-samples-db
               ("Path to the db containing bt root samples."
                "Mandatory when -C provided.")
               #:collect {"path" take-latest #f}
               #:mandatory-unless (λ (flags) (not (member 'run-checks? flags)))]
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
 (define full-config-path (simple-form-path config-path))

 (define racket-exe (simple-form-path (find-system-path 'exec-file)))

 (for ([data-dir (in-list data-dirs)])
   (displayln @~a{-------------------- @(basename data-dir) --------------------})
   (define summary (summarize data-dir summaries-db-path))
   (displayln (format-summary summary))
   (newline)
   (when run-checks?
     (define missing-mutants-output (open-output-string))
     (define errors-output (open-output-string))
     (parameterize ([current-output-port missing-mutants-output]
                    [current-error-port missing-mutants-output])
       (system* racket-exe
                check-for-missing-mutants-or-trails.rkt
                "-s" mutant-samples-db-path
                "-S" summaries-db-path
                "-r" root-samples-db-path
                "-c" full-config-path
                "-p" data-dir))
     (parameterize ([current-output-port errors-output]
                    [current-error-port errors-output])
       (system* racket-exe
                check-for-errors.rkt
                data-dir))
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
