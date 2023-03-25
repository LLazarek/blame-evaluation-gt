#lang at-exp rscript

(require (prefix-in db: bex/db/db)
         bex/mutation-analysis/mutation-analysis-summaries
         bex/configurations/configure-benchmark
         bex/util/progress-log
         bex/util/mutant-util
         bex/runner/mutation-runner
         process-queue/priority
         racket/random)

(define MUTANT-SAMPLE-SIZE 500)
(define CONFIG-SAMPLE-SIZE 1000)
(define CONFIG-SAMPLE-MAX-RETRIES 5)

(define-runtime-paths
  [erasure-config-path "../../bex/configurables/configs/erasure-stack-first.rkt"]
  [transient-config-path "../../bex/configurables/configs/transient-newest.rkt"]
  [natural-config-path "../../bex/configurables/configs/TR.rkt"])

;; lltodo: <2020-11-18 Wed>
;; Much of this is very similar to `filter-mutants-for-dynamic-errors.rkt`
;; They should really share the same library.
;; Not doing it now because no time to make sure I don't change the behavior of `filter-...`
;; in the process.

(define-logger comparison)

(define (enQ-dynamic-error-checker q mutant id benchmarks-dir
                                   #:log-progress log-progress!
                                   #:resample resample-mutant!
                                   #:resampled? [this-checker-has-been-resampled? #f])
  (define benchmark (find-mutant-benchmark mutant benchmarks-dir))
  (define max-configuration (make-max-bench-config benchmark))
  (define (random-configuration)
    (hash-set (for/hash ([mod (in-hash-keys max-configuration)])
                (values mod (random-ref '(none types))))
              (mutant-module mutant) 'none))

  (define all-modes-to-check (list natural-config-path
                                   transient-config-path
                                   erasure-config-path))
  (define (enQ-a-new-dynamic-error-checker q
                                           #:to-check modes-to-check
                                           #:results-so-far results-so-far
                                           #:retry-count retry-count)
    (log-comparison-info
     @~a{
         @mutant [@id] enQing checker attempt @retry-count / @CONFIG-SAMPLE-MAX-RETRIES @;
         with modes-to-check @(map basename modes-to-check)
         Queue state: [A: @(process-queue-active-count q), W: @(process-queue-waiting-count q)]
         })
    (cond [(> retry-count CONFIG-SAMPLE-MAX-RETRIES)
           (log-comparison-info @~a{Ran out of retries for @mutant, trying to resample...})
           (match (resample-mutant! mutant)
             [#f q]
             [new-mutant
              (enQ-dynamic-error-checker q new-mutant id benchmarks-dir
                                         #:log-progress log-progress!
                                         #:resample resample-mutant!
                                         #:resampled? #t)])]
          [else
           (define a-config (random-configuration))
           (process-queue-enqueue q
                                  (make-mutant-spawner a-config
                                                       modes-to-check
                                                       results-so-far
                                                       retry-count)
                                  (- (length modes-to-check)
                                     retry-count))]))
  (define (make-mutant-spawner run-configuration
                               modes-to-check
                               results-so-far
                               retry-count)
    (define-values {this-mode mode-to-run configuration-to-run}
      (match (first modes-to-check)
        [(and (== erasure-config-path)
              erasure-mode)
         ;; There should never be a type error since we're only doing racket ctc violations
         ;; So we can just run the untyped config under Natural
         (values erasure-mode
                 natural-config-path
                 (for/hash ([mod (in-hash-keys max-configuration)])
                   (values mod 'none)))]
        [another-mode
         (values another-mode another-mode run-configuration)]))
    (thunk
     (define (will current-q info)
       (define outcome (extract-outcome (process-info-data info)
                                        mutant
                                        run-configuration))
       (log-comparison-info
        @~a{
            @mutant [@id] outcome for mode @(basename this-mode) = @outcome
            })
       (define results+outcome (add-outcome outcome this-mode results-so-far))
       (if (and (TR? this-mode) (not (equal? outcome 'blamed)))
           (enQ-a-new-dynamic-error-checker current-q
                                            #:to-check all-modes-to-check
                                            #:results-so-far empty
                                            #:retry-count (add1 retry-count))
           (match modes-to-check
             [(list _)
              (log-comparison-info
               @~a{
                   @mutant [@id] That's the last mode, done!
                   })
              (log-progress! mutant id (list results+outcome run-configuration this-checker-has-been-resampled?))
              current-q]
             [(list* _ more-modes-to-check)
              (log-comparison-info
               @~a{
                   @mutant [@id] Checking remaining modes...
                   })
              (enQ-a-new-dynamic-error-checker current-q
                                               #:to-check more-modes-to-check
                                               #:results-so-far results+outcome
                                               #:retry-count retry-count)])))

     (define configured-benchmark
       (configure-benchmark benchmark
                            configuration-to-run))
     (define outfile (make-temporary-file @~a{@(benchmark->name benchmark)-~a}
                                          #f
                                          (working-dir)))
     (log-comparison-debug
      @~a{

          Calling spawn-mutant-runner for mutant @mutant in @(basename this-mode) with
          @(pretty-format
            (list (list 'configure-benchmark benchmark run-configuration)
                  (mutant-module mutant)
                  (mutant-index mutant)
                  outfile
                  mode-to-run))

          })
     (define ctl
       (parameterize ([mutant-error-log outfile])
         (spawn-mutant-runner configured-benchmark
                              (mutant-module mutant)
                              (mutant-index mutant)
                              outfile
                              mode-to-run)))
     (log-comparison-info
      @~a{
          @mutant [@id] checker attempt @retry-count / @CONFIG-SAMPLE-MAX-RETRIES @;
          launched
          })
     (process-info outfile ctl will)))

  (enQ-a-new-dynamic-error-checker q
                                   #:to-check all-modes-to-check
                                   #:results-so-far empty
                                   #:retry-count 0))

(define (add-outcome outcome mode results-so-far)
  (cons (list (basename mode) outcome) results-so-far))

(define (TR? mode)
  (string-suffix? (~a mode) "TR.rkt"))

(define (extract-outcome outfile mutant config)
  (define result (with-handlers ([exn:fail? (const #f)])
                   (file->value outfile)))
  (match result
    [(struct* run-status ([outcome outcome]))
     outcome]
    [else
     (log-comparison-error
      @~a{
          Unable to read result from mutant @mutant config @config @;
          Output file contents: @;
          "@(file->string outfile)"
          })
     '?]))

(define (find-mutant-benchmark mutant benchmarks-dir)
  (define path (build-path benchmarks-dir (mutant-benchmark mutant)))
  (unless (path-to-existant-directory? path)
    (raise-user-error 'find-mutant-benchmark
                      @~a{Can't find benchmark for @mutant in @benchmarks-dir}))
  (read-benchmark path))

;; (hash/c mod-name? summary?) string? -> (listof mutant?)
(define (summaries->mutants summaries benchmark-name)
  (for*/list ([{mod-name summary} (in-hash summaries)]
              [{mutator indices} (in-hash (summary-valid-indices summary))]
              [index (in-list indices)])
    (mutant benchmark-name mod-name index)))

(define ((add-to-list x) l) (cons x l))

(define working-dir (make-parameter "compare-error-detection-scratch"))
(main
 #:arguments ({(hash-table ['summaries-db summaries-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['progress-log progress-log-path]
                           ['working-dir _]
                           ['process-limit (app string->number process-limit)])
               args}
              #:once-each
              [("-s" "--summaries-db")
               'summaries-db
               ("Input: A database containing mutation analysis summaries for mutants to further"
                "analyze.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-b" "--benchmarks")
               'benchmarks-dir
               ("Directory containing benchmarks referenced by the summaries-db.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-l" "--progress-log")
               'progress-log
               ("Record progress in the given log file."
                "If it exists and is not empty, resume from the point reached in the log.")
               #:collect {"path" take-latest #f}
               #:mandatory]

              [("-w" "--working-dir")
               'working-dir
               ("Set the working directory for storing temporary data."
                @~a{Default: @(working-dir)})
               #:collect {"path" (set-parameter working-dir) (working-dir)}]
              [("-j" "--process-limit")
               'process-limit
               ("How many parallel processes to use."
                "Default: 1")
               #:collect {"N" take-latest "1"}])

 #:check [(db:path-to-db? summaries-db-path)
          @~a{Can't find db at @summaries-db-path}]

 (make-directory* (working-dir))

 (log-comparison-info @~a{
                          Starting comparison with summaries @summaries-db-path
                          and benchmarks @benchmarks-dir
                          and progress-log @progress-log-path
                          and working dir @(working-dir)
                          and process limit @process-limit
                          })
 (define progress-dict
   (match progress-log-path
     [(? file-exists? path)
      (log-comparison-info @~a{Pulling progress from log})
      (file->list path)]
     [else empty]))
 (define progress (make-immutable-hash progress-dict))

 (define-values {log-progress!/raw finalize-log!}
   (initialize-progress-log! progress-log-path
                             #:exists 'append))
 (define (log-progress! mutant id mode-outcomes)
   (log-progress!/raw (cons (list mutant id)
                            mode-outcomes)))
 (define (logged-progress mutant id)
   (hash-ref progress
             (list mutant id)
             #f))

 (define summaries-db (db:get summaries-db-path))
 (define all-mutants
   (for*/list ([benchmark (in-list (db:keys summaries-db))]
               [mutant (in-list (summaries->mutants (db:read summaries-db benchmark)
                                                    benchmark))])
     mutant))
 (define mutants-to-sample-from
   (match (hash-ref progress 'mutant-samples #f)
     [#f
      (define mutants (random-sample all-mutants
                                     MUTANT-SAMPLE-SIZE
                                     #:replacement? #f))
      (log-progress!/raw (cons 'mutant-samples mutants))
      mutants]
     [original-sample
      (for/fold ([mutants original-sample])
                ([{key value} (in-dict progress-dict)]
                 #:when (equal? key 'resample-mutant))
        (match-define (list original-mutant resampled-mutant) value)
        (define mutants-less-original (remove original-mutant mutants))
        (cons resampled-mutant mutants-less-original))]))
 (define resample-mutant!
   (let ([remaining-unsampled-mutants (list->mutable-set
                                       (set-subtract all-mutants
                                                     mutants-to-sample-from))])
     (λ (original-mutant)
      (cond [(set-empty? remaining-unsampled-mutants)
             (log-comparison-info @~a{Attempt to resample @original-mutant failed: out of mutants})
             #f]
            [else
             (define new-mutant (random-ref remaining-unsampled-mutants))
             (set-remove! remaining-unsampled-mutants new-mutant)
             (log-progress!/raw (cons 'resample-mutant (list original-mutant new-mutant)))
             (log-comparison-info
              @~a{Attempt to resample @original-mutant success: chose @new-mutant})
             new-mutant]))))

 (process-queue-wait
  (for/fold ([q (make-process-queue process-limit)])
            ([mutant (in-list mutants-to-sample-from)]
             [mutant-id-number (in-naturals)]
             #:when #t
             [config-number (in-range (/ CONFIG-SAMPLE-SIZE (length mutants-to-sample-from)))])
    (if (logged-progress mutant config-number)
        q
        (enQ-dynamic-error-checker q
                                   mutant
                                   (cons mutant-id-number config-number)
                                   benchmarks-dir
                                   #:log-progress log-progress!
                                   #:resample resample-mutant!))))
 (log-comparison-info "Comparison complete."))
