#lang at-exp rscript

;; Input: a mutation analysis summaries DB

;; Output: a DB containing scenarios (belonging to a subset of the mutants in the input DB)
;; which are /interesting/ according to the configuration `parameterizing-config`.
;; In the current implementation, interesting means:
;; 1. An ancestor (config lower in the lattice reachable from this one) causes a runtime error
;;    when run under `parameterizing-config`, and
;; 2. it's unlikely that it causes a type error (specifically, it doesn't type a module that
;;    typing in the lowest possible config causes a type error).

;; Output DB format:
;; (benchmark-name? => (hash/c mutant? (listof serialized-config?)))

(require racket/hash
         (prefix-in db: "../db/db.rkt")
         "mutation-analysis-summaries.rkt"
         "debugging-scenarios.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "../util/progress-log.rkt"
         "../util/mutant-util.rkt"
         "../runner/mutation-runner.rkt"
         process-queue/priority)

(define-runtime-paths
  [default-parameterizing-config "../configurables/configs/TR.rkt"])
(define current-parameterizing-config (make-parameter default-parameterizing-config))

(define working-dir (make-parameter "find-mutant-dynamic-errors-scratch"))

(define-logger find-scenarios)

;; (listof mutant?)
;; path-to-existant-directory?
;; process-queue?
;; ->
;; process-queue?
(define (enq-mutants-interesting-scenarios-finders mutants-with-type-errors
                                                   benchmark-path
                                                   pq
                                                   #:log-progress log-progress!
                                                   #:logged-progress logged-progress)
  (define benchmark (read-benchmark benchmark-path))
  (define benchmark-name (benchmark->name benchmark))
  (log-find-scenarios-info @~a{Starting @benchmark-name analysis})
  (for/fold ([q pq])
            ([mutant (in-list mutants-with-type-errors)])
    (find-interesting-scenarios-for-mutant mutant
                                           benchmark
                                           q
                                           #:log-progress log-progress!
                                           #:logged-progress logged-progress)))

;; benchmark? -> (streamof config/c)
(define (benchmark-scenario-configs benchmark)
  (define max-config (make-max-bench-config benchmark))
  (define min-config (for/hash ([mod (in-hash-keys max-config)])
                       (values mod 'none)))
  (for/stream ([mod (in-hash-keys min-config)])
    (hash-set min-config mod 'types)))

(define (all-possible-configs-with mods)
  (match mods
    [(list* mod remaining)
     (define remaining-stream (all-possible-configs-with remaining))
     (stream-append (stream-map (λ (c) (hash-set c mod 'types))
                                remaining-stream)
                    (stream-map (λ (c) (hash-set c mod 'none))
                                remaining-stream))]
    ['() (stream (hash))]))

;; mutant?
;; benchmark?
;; (process-queue/c (listof scenario?))
(define (find-interesting-scenarios-for-mutant mutant
                                               benchmark
                                               q
                                               #:log-progress log-progress!
                                               #:logged-progress logged-progress)
  (define benchmark-name (benchmark->name benchmark))
  (for/fold ([q q])
            ([config (in-stream (benchmark-scenario-configs benchmark))])
    (define the-scenario (scenario mutant config))
    (match (logged-progress benchmark-name the-scenario)
      ['? (process-queue-enqueue
           q
           (interesting-scenario-checker benchmark
                                         the-scenario
                                         #:log-progress log-progress!)
           2)]
      [#t (process-queue-update-data q (add-to-list (list benchmark-name the-scenario)))]
      [#f q])))

(define ((config->interesting-run-status? config) rs)
  (match rs
    #;[(struct* run-status ([outcome 'blamed])) #t]
    #;[(struct* run-status ([outcome 'runtime-error]
                          [context-stack context]))
     ;; (define context-mods-in-program
     ;;   (filter (λ (blamed) (hash-has-key? config blamed)) context))
     ;; (define 3-unique-mods-on-stack?
     ;;   (>= (length (remove-duplicates context-mods-in-program)) 3))
     ;; (log-find-scenarios-debug
     ;;  @~a{
     ;;      ⇓ @(serialize-config config) interesting?, ctx: @context : @3-unique-mods-on-stack?
     ;;      })
     ;; 3-unique-mods-on-stack?
     #t]
    [(struct* run-status ([outcome (not 'type-error)])) #t]
    [else #f]))

(define (get-run-status outfile mutant config)
  (with-handlers ([exn:fail? (λ _
                              (log-find-scenarios-error
                               @~a{
                                   Unable to read result from mutant @mutant config @config
                                   Output file contents:
                                   "@(file->string outfile)"
                                   })
                              #f)])
    (file->value outfile)))

(struct interesting-error ())
(struct other-outcome ())
(define (extract-outcome result benchmark mutant config)
  (match result
    [(? (config->interesting-run-status? config))
     (log-find-scenarios-info @~a{
                                  @benchmark @mutant @(serialize-config config) @;
                                  => @(run-status-outcome result) @;
                                  : accepted
                                  })
     (interesting-error)]
    [(? run-status?)
     (log-find-scenarios-info @~a{
                                  @benchmark @mutant @(serialize-config config) @;
                                  => @(run-status-outcome result) @;
                                  : filtered
                                  })
     (other-outcome)]
    [else
     (void)]))

(define max-retries 3)
(define (interesting-scenario-checker benchmark a-scenario
                                      #:log-progress log-progress!
                                      #:retry-count [retry-count 0])
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
    (define (retry)
      (process-queue-enqueue
       q
       (interesting-scenario-checker benchmark
                                     a-scenario
                                     #:log-progress log-progress!
                                     #:retry-count (add1 retry-count))
       1))

    (define result (get-run-status (process-info-data info) mutant run-configuration))
    (match (extract-outcome result benchmark-name mutant run-configuration)
      [(? interesting-error?)
       (log-progress! benchmark-name a-scenario #t)
       (process-queue-update-data q (add-to-list (list benchmark-name a-scenario)))]
      [(? other-outcome?)
       #:when (match result
                [(struct* run-status ([outcome 'type-error]))
                 (< retry-count max-retries)]
                [else #f])
       (log-find-scenarios-info
        @~a{
            Retrying @benchmark-name @mutant @(serialize-config run-configuration) @;
            to confirm type error result @;
            (retry # @(add1 retry-count))
            })
       (retry)]
      [(? other-outcome?)
       (log-progress! benchmark-name a-scenario #f)
       q]
      [else
       #:when (< retry-count max-retries)
       (log-find-scenarios-info
        @~a{
            Retrying @benchmark-name @mutant @(serialize-config run-configuration) @;
            in response to failure @;
            (retry # @(add1 retry-count))
            })
       (retry)]
      [else
       (log-find-scenarios-error
        @~a{
            No more retries for @;
            @benchmark-name @mutant @(serialize-config run-configuration), @;
            giving up
            })
       q]))

  (mutant-spawner run-configuration
                  will:record-outcome!))

(define (process-queue-update-data q f)
  (process-queue-set-data q (f (process-queue-get-data q))))

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

 (install-configuration! (current-parameterizing-config))

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

 (define q
   (for/fold ([q (make-process-queue process-limit
                                 ; (listof (list/c benchmark-name? scenario?))
                                 empty
                                 <
                                 #:kill-older-than (* 5 60))])
             ([benchmark-name (in-list (db:keys summaries-db))])
     (define benchmark-path (build-path benchmarks-dir benchmark-name))

     (define base-summaries (db:read summaries-db benchmark-name))
     (define benchmark-mutants (summaries->mutants base-summaries))
     (enq-mutants-interesting-scenarios-finders benchmark-mutants
                                                benchmark-path
                                                q
                                                #:log-progress log-progress!
                                                #:logged-progress logged-progress)))

 (define interesting-base-scenarios
   (process-queue-get-data (process-queue-wait q)))

 (finalize-log!)

 (define interesting-base-scenarios-grouped-by-benchmark
   (group-by first interesting-base-scenarios))

 (log-find-scenarios-info
  "Collected interesting base scenarios; computing full interesting sub-lattices...")

 (define interesting-scenarios-by-mutant-per-benchmark
   (for/hash ([group (in-list interesting-base-scenarios-grouped-by-benchmark)])
     (define benchmark-name (first (first group)))
     (log-find-scenarios-info @~a{ ... @benchmark-name})
     (define benchmark-path (build-path benchmarks-dir benchmark-name))
     (define benchmark (read-benchmark benchmark-path))
     (define max-config (make-max-bench-config benchmark))
     (define all-typable-modules (hash-keys max-config))

     (define scenarios-grouped-by-mutant
       (group-by scenario-mutant (map second group)))
     (define interesting-scenarios-by-mutant
       (for/hash ([scenarios (in-list scenarios-grouped-by-mutant)])
         (define mutant (scenario-mutant (first scenarios)))
         (define modules-that-are-safe-to-type
           (set->list
            (for/set ([scenario (in-list scenarios)])
              (match (scenario-config scenario)
                [(hash-table [typed-mod 'types] [_ 'none] ...)
                 typed-mod]
                [else (error 'bad-scenario
                             (~s scenario))]))))
         (log-find-scenarios-info
          @~a{  sublattice has @(length modules-that-are-safe-to-type) modules, making its size: @(expt 2 (length modules-that-are-safe-to-type))})

         (define modules-that-are-not-safe-to-type (set-subtract all-typable-modules
                                                                 modules-that-are-safe-to-type))
         (define interesting-sub-lattice-configs
           (all-possible-configs-with modules-that-are-safe-to-type))

         (define unsafe-sub-config
           (for/hash ([m (in-list modules-that-are-not-safe-to-type)])
             (values m 'none)))

         (define interesting-sub-lattice-configs/with-missing-modules
           (stream-map (λ (c) (hash-union c unsafe-sub-config))
                       interesting-sub-lattice-configs))

         (values mutant
                 (stream->list
                  (stream-map serialize-config
                              interesting-sub-lattice-configs/with-missing-modules)))))
     (values benchmark-name
             interesting-scenarios-by-mutant)))

 (unless (db:path-to-db? interesting-scenarios-db-path)
   (log-find-scenarios-info @~a{Creating db at @interesting-scenarios-db-path})
   (db:new! interesting-scenarios-db-path))
 (log-find-scenarios-info @~a{Writing database})
 (define interesting-scenarios-db (db:get interesting-scenarios-db-path))
 (void (db:write! interesting-scenarios-db interesting-scenarios-by-mutant-per-benchmark))

 (log-find-scenarios-info @~a{Cleaning up working dir})
 (delete-directory/files (working-dir))

 (log-find-scenarios-info @~a{Analysis complete.}))

(module test racket/base)
