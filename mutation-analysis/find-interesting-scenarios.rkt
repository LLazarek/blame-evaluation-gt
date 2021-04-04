#lang at-exp rscript

;; Input: a mutation analysis summaries DB

;; Output: a DB containing scenarios (belonging to a subset of the mutants in the input DB)
;; which are /interesting/ according to the configuration `parameterizing-config`.
;; Specifically, /interesting/ means that running the scenario with
;; `parameterizing-config`
;; 1. produces an error, and
;; 2. the error context stack contains at least three distinct modules from the program

(require (prefix-in db: "../db/db.rkt")
         "mutation-analysis-summaries.rkt"
         "debugging-scenarios.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../util/progress-log.rkt"
         "../util/mutant-util.rkt"
         "../process-q/interface.rkt"
         "../process-q/priority.rkt"
         "../runner/mutation-runner.rkt")

(define-runtime-paths
  [default-parameterizing-config "../configurables/configs/TR--context.rkt"])
(define current-parameterizing-config (make-parameter default-parameterizing-config))

(define working-dir (make-parameter "find-mutant-dynamic-errors-scratch"))

(define-logger find-scenarios)

;; (listof mutant?)
;; path-to-existant-directory?
;; ->
;; (listof scenario?)
(define (mutants-interesting-scenarios mutants-with-type-errors
                                       benchmark-path
                                       #:log-progress log-progress!
                                       #:logged-progress logged-progress
                                       #:process-limit process-limit)
  (define benchmark (read-benchmark benchmark-path))
  (define benchmark-name (benchmark->name benchmark))
  (log-find-scenarios-info @~a{Starting @benchmark-name analysis})
  (define q
    (for/fold ([q (make-process-Q process-limit
                                  ; (listof mutant?)
                                  empty
                                  #:kill-older-than (* 5 60))])
              ([mutant (in-list mutants-with-type-errors)])
      (find-interesting-scenarios-for-mutant mutant
                                             benchmark
                                             q
                                             #:log-progress log-progress!
                                             #:logged-progress logged-progress)))
  (define interesting-scenarios
    (process-Q-get-data (process-Q-wait q)))
  (log-find-scenarios-info
   @~a{
       @benchmark-name analysis complete: @;
       found @;
       @(length interesting-scenarios) interesting scenarios @;
       across all @(length mutants-with-type-errors) mutants.
       })
  interesting-scenarios)

;; benchmark? -> (streamof config/c)
(define (benchmark-scenarios benchmark)
  (define (scenario-stream-with-mods mods)
    (match mods
      [(list* mod remaining)
       (define remaining-stream (scenario-stream-with-mods remaining))
       (stream-append (stream-map (λ (c) (hash-set c mod 'types))
                                  remaining-stream)
                      (stream-map (λ (c) (hash-set c mod 'none))
                                  remaining-stream))]
      ['() (stream (hash))]))
  (define max-config (make-max-bench-config benchmark))
  (scenario-stream-with-mods (hash-keys max-config)))

;; mutant?
;; benchmark?
;; (process-Q/c (listof scenario?))
(define (find-interesting-scenarios-for-mutant mutant
                                               benchmark
                                               q
                                               #:log-progress log-progress!
                                               #:logged-progress logged-progress)
  (define benchmark-name (benchmark->name benchmark))
  (for/fold ([q q])
            ([config (in-stream
                      (stream-filter (λ (config) (equal? (hash-ref config (mutant-module mutant))
                                                         'none))
                                     (benchmark-scenarios benchmark)))])
    (define the-scenario (scenario mutant config))
    (match (logged-progress benchmark-name the-scenario)
      ['? (process-Q-enq q
                         (interesting-scenario-checker benchmark
                                                       the-scenario
                                                       #:log-progress log-progress!)
                         2)]
      [#t (process-Q-update-data q (add-to-list the-scenario))]
      [#f q])))

(define ((config->interesting-run-status? config) rs)
  (match rs
    [(struct* run-status ([outcome 'blamed])) #t]
    [(struct* run-status ([outcome 'runtime-error]
                          [context-stack context]))
     (define context-mods-in-program
       (filter (λ (blamed) (hash-has-key? config blamed)) context))
     (define 3-unique-mods-on-stack?
       (>= (length (remove-duplicates context-mods-in-program)) 3))
     (log-find-scenarios-debug
      @~a{
          @mutant @(serialize-config config) interesting?, ctx: @context : @3-unique-mods-on-stack?
          })
     3-unique-mods-on-stack?]
    [else #f]))

(struct interesting-error ())
(struct other-outcome ())
(define (extract-outcome outfile mutant config)
  (define result (with-handlers ([exn:fail? (const #f)])
                   (file->value outfile)))
  (match result
    [(? (config->interesting-run-status? config))
     (log-find-scenarios-info @~a{
                                  @mutant @(serialize-config config) @;
                                  => @(run-status-outcome result) @;
                                  : accepted
                                  })
     (interesting-error)]
    [(? run-status?)
     (log-find-scenarios-info @~a{
                                  @mutant @(serialize-config config) @;
                                  => @(run-status-outcome result) @;
                                  : filtered
                                  })
     (other-outcome)]
    [else
     (log-find-scenarios-error
      @~a{
          Unable to read result from mutant @mutant config @config
          Output file contents:
          "@(file->string outfile)"
          })
     (void)]))

(define (interesting-scenario-checker benchmark a-scenario
                                      #:log-progress log-progress!)
  (define benchmark-name (benchmark->name benchmark))
  (match-define (scenario mutant run-configuration) a-scenario)

  (define ((mutant-spawner config will))
    (define configured-benchmark
      (configure-benchmark benchmark
                           config))
    (define outfile (make-temporary-file @~a{@(benchmark->name benchmark)-~a}
                                         #f
                                         (working-dir)))
    (define ctl
      (parameterize ([mutant-error-log outfile])
        (spawn-mutant-runner configured-benchmark
                             (mutant-module mutant)
                             (mutant-index mutant)
                             outfile
                             (current-parameterizing-config))))
    (process-info outfile ctl will))

  (define (will:record-outcome! q info)
    (match (extract-outcome (process-info-data info) mutant run-configuration)
      [(? interesting-error?)
       (log-progress! benchmark-name a-scenario #t)
       (process-Q-update-data q (add-to-list a-scenario))]
      [(? other-outcome?)
       (log-progress! benchmark-name a-scenario #f)
       q]
      [else q]))

  (mutant-spawner run-configuration
                  will:record-outcome!))

(define (process-Q-update-data q f)
  (process-Q-set-data q (f (process-Q-get-data q))))

;; (hash/c mod-name? summary?) -> (listof mutant?)
(define (summaries->mutants summaries)
  (for*/list ([{mod-name summary} (in-hash summaries)]
              [{mutator indices} (in-hash (summary-valid-indices summary))]
              [index (in-list indices)])
    (mutant #f mod-name index)))

(define ((add-to-list x) l) (cons x l))

;; (hash/c mod-name? summary?) (listof mutant?) -> (hash/c mod-name? summary?)
(define (narrow-summaries summaries mutants)
  (define mutants-by-mod
    (for/fold ([mutants-by-mod (hash)])
              ([mutant (in-list mutants)])
      (hash-update mutants-by-mod
                   (mutant-module mutant)
                   (add-to-list mutant)
                   empty)))
  (for/hash ([{mod-name mutants} (in-hash mutants-by-mod)])
    (define old-mod-summary (hash-ref summaries mod-name #f))
    (define new-indices-by-mutator
      (for/hash ([{mutator indices} (in-hash (summary-valid-indices old-mod-summary))])
        (define indices-as-mutants (map (λ (i) (mutant #f mod-name i)) indices))
        (define intersection-with-mutants
          (set-intersect indices-as-mutants mutants))
        (values mutator
                (map mutant-index intersection-with-mutants))))
    (values mod-name
            (struct-copy summary
                         old-mod-summary
                         [valid-indices new-indices-by-mutator]))))

(define current-mode (make-parameter 'erasure-interesting))

(main
 #:arguments ({(hash-table ['summaries-db summaries-db-path]
                           ['interesting-scenarios-db interesting-scenarios-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['progress-log progress-log-path]
                           ['working-dir _]
                           ['process-limit (app string->number process-limit)]
                           ['config _])
               args}
              #:once-each
              [("-s" "--summaries-db")
               'summaries-db
               ("Input: A database containing mutation analysis summaries for mutants to further"
                "analyze.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--interesting-scenarios-db")
               'interesting-scenarios-db
               ("Output: A database to be written with interesting scenarios"
                "from the mutants in the summaries-db.")
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

              [("-d" "--working-dir")
               'working-dir
               ("Set the working directory for storing temporary data."
                @~a{Default: @(working-dir)})
               #:collect {"path" (set-parameter working-dir) (working-dir)}]
              [("-j" "--process-limit")
               'process-limit
               ("How many parallel processes to use."
                "Default: 1")
               #:collect {"N" take-latest "1"}]
              [("-c" "--config")
               'config
               ("Config by which to parameterize the meaning of `interesting`:"
                "i.e. specifying the semantics under which to run candidate scenarios."
                @~a{Default: @(current-parameterizing-config)})
               #:collect {"path"
                          (set-parameter current-parameterizing-config)
                          (current-parameterizing-config)}])

 #:check [(db:path-to-db? summaries-db-path)
          @~a{Can't find db at @summaries-db-path}]

 (define progress
   (match progress-log-path
     [(? file-exists? path)
      (log-find-scenarios-info @~a{Pulling progress from log @path})
      (make-hash (file->list path))]
     [else (hash)]))
 (define-values {log-progress!/raw finalize-log!}
   (initialize-progress-log! progress-log-path
                             #:exists 'append))
 (define (serialize-scenario s)
   (list (scenario-mutant s)
         (serialize-config (scenario-config s))))
 (define (log-progress! benchmark-name scenario interesting-error?)
   (log-progress!/raw (cons (list benchmark-name (serialize-scenario scenario))
                            interesting-error?)))
 (define (logged-progress benchmark-name a-scenario)
   (hash-ref progress
             (list benchmark-name (serialize-scenario a-scenario))
             '?))


 (define summaries-db (db:get summaries-db-path))

 (when (directory-exists? (working-dir))
   (log-find-scenarios-info @~a{Deleting old working dir at @(working-dir)})
   (delete-directory/files (working-dir)))
 (make-directory* (working-dir))

 (unless (db:path-to-db? interesting-scenarios-db-path)
   (log-find-scenarios-info @~a{Creating db at @interesting-scenarios-db-path})
   (db:new! interesting-scenarios-db-path))
 (define interesting-scenarios-db (db:get interesting-scenarios-db-path))

 (define interesting-scenarios-by-benchmark
   (for/hash ([benchmark-name (in-list (db:keys summaries-db))])
     (define benchmark-path (build-path benchmarks-dir benchmark-name))

     (define base-summaries (db:read summaries-db benchmark-name))
     (define benchmark-mutants (summaries->mutants base-summaries))
     (values benchmark-name
             (mutants-interesting-scenarios benchmark-mutants
                                            benchmark-path
                                            #:log-progress log-progress!
                                            #:logged-progress logged-progress
                                            #:process-limit process-limit))))

 (finalize-log!)

 (log-find-scenarios-info @~a{Writing database})
 (void (db:write! interesting-scenarios-db interesting-scenarios-by-benchmark))

 (log-find-scenarios-info @~a{Cleaning up working dir})
 (delete-directory/files (working-dir))

 (log-find-scenarios-info @~a{Analysis complete.}))

(module test racket/base)
