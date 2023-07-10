#lang at-exp racket

(require "../runner/mutation-runner.rkt"
         (only-in "../runner/mutation-runner-data.rkt" run-outcome/c)
         "../runner/unify-program.rkt"
         "../util/program.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "../util/optional-contracts.rkt"
         "../util/mutant-util.rkt"
         "../util/progress-log.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "mutant-factory-data.rkt"
         "blame-trail-data.rkt"
         "integrity-metadata.rkt"
         process-queue/priority
         racket/file
         racket/format
         racket/match
         racket/system
         racket/set
         racket/port
         racket/file
         racket/logging
         racket/date
         racket/runtime-path
         racket/list
         syntax/parse/define
         (for-syntax racket/base))

(module+ test
  (provide (struct-out mutant)
           (struct-out revivals)
           (struct-out mutant-process)
           (struct-out dead-mutant-process)
           (struct-out bench-info)
           (struct-out factory)
           (struct-out blame-trail)
           process-limit
           data-output-dir
           benchmarks-dir-path
           factory-logger
           abort-on-failure?
           default-memory-limit/gb
           default-timeout/s
           MAX-FAILURE-REVIVALS
           MAX-TYPE-ERROR-REVIVALS
           sample-size
           copy-factory
           mutant-error-log
           test-mutant-flag
           current-result-cache

           run-all-mutants*configs
           sample-blame-trails-if-max-config-result-ok
           sample-blame-trail-roots
           extend-blame-trail
           record-blame-trail!
           mutant-data-file-name
           follow-blame-from-dead-process
           make-blame-disappearing-fallback
           spawn-mutant
           read-mutant-result
           process-outcome
           mutant->process-will
           make-blame-following-will/fallback
           abort-suppressed?

           make-cached-results-for
           make-progress-logger

           record/check-configuration-outcomes?
           record/check-configuration-outcome!
           setup-configuration-outcome-record/checking!))

(define debug:save-individual-mutant-outputs? #f)


(define MAX-CONFIG 'types)
(define MAX-FAILURE-REVIVALS 10)
(define MAX-TYPE-ERROR-REVIVALS 3)
(struct no-recorded-outcome () #:transparent)

;; Outcomes in a blame trail that aren't one of these will go straight to the no-blame-handler
(define/contract normal-blame-trail-outcomes
  (listof run-outcome/c)
  '(type-error
    runtime-error
    blamed))

(define-runtime-path benchmarks-dir-path "../../gtp-benchmarks/benchmarks/")

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define abort-on-failure? (make-parameter #t))

(define current-result-cache (make-parameter (λ _ #f)))
(define current-progress-logger (make-parameter void))

(define/contract record/check-configuration-outcomes?
  (parameter/c (or/c #f
                     (list/c 'record (mutant/c config/c run-outcome/c . -> . any))
                     (list/c 'check  (mutant/c config/c . -> . (or/c run-outcome/c
                                                                     no-recorded-outcome?)))
                     (list/c (or/c 'record 'check)
                             path-string?)))
  (make-parameter #f))

(define-logger factory)
(define (log-factory-message level msg . vs)
  (when (log-level? factory-logger level)
    (log-message factory-logger
                 level
                 (apply
                  format
                  (string-append "[~a] "
                                 (if (member level '(fatal error warning))
                                     (failure-msg level msg)
                                     msg))
                  (date->string (current-date) #t)
                  vs)
                 #f)))
(define-syntax-rule (log-factory level msg v ...)
  (log-factory-message 'level msg v ...))
(define (failure-msg failure-type m)
  (string-append "***** " (~a failure-type) " *****\n" m "\n**********"))

;; Main entry point of the factory
(define/contract (run-all-gradual bench
                                  #:log-progress log-progress!
                                  #:load-progress load-result-cache)
  (benchmark/c
   #:log-progress (module-name? natural? natural? path-to-existant-file? . -> . any)
   #:load-progress (-> (module-name? natural? natural? . -> . (or/c #f path-to-existant-file?)))
   . -> .
   boolean? ; experiment complete and sanity checks pass?
   )

  (parameterize ([current-progress-logger log-progress!]
                 [current-result-cache (load-result-cache)])
    (log-factory info @~a{Running on benchmark @bench})

    (define max-config (make-max-bench-config bench))
    (log-factory info "Benchmark has mutatable modules:~n~a"
                 mutatable-module-names)

    (unless (directory-exists? (data-output-dir))
      (log-factory debug "Creating output directory ~a." (data-output-dir))
      (make-directory (data-output-dir)))

    (define N-samples-please
      ((configured:make-bt-root-sampler) (factory-bench the-factory)
                                         mutant-program))
    (define samples (N-samples-please (sample-size)))
    (define (resample a-factory)
      (first (N-samples-please 1)))
    (define select-mutants (configured:select-mutants))
    (define process-q
      (for/fold ([process-q (make-process-queue
                             (process-limit)
                             (factory (bench-info bench max-config)
                                      (hash)
                                      (hash)
                                      0)
                             < ;; lower priority value means schedule sooner (this was
                               ;; unconfigurable with the original implementation, now just
                               ;; stick to that original default)
                             #:kill-older-than (let-values ([{max-timeout _}
                                                             (increased-limits bench)])
                                                 (+ max-timeout 30)))])
                ([sampled-config (in-list samples)]
                 [sample-number (in-naturals)]
                 #:when #t
                 [test-set (in-list ???)])
        (run-all-mutants process-q
                         sampled-config
                         (λ (process-q)
                           (calculate-mutation-score)
                           ;; remove tests, add new processes
                           ))))

    (log-factory info "Finished enqueing all test mutants. Waiting...")
    (define process-q-finished (process-queue-wait process-q))
    (report-completion/sanity-checks bench
                                     select-mutants
                                     (load-result-cache))))

;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench   ;; benchmark/c
                 results ;; map from config to pair: number of failures, number of total mutants
                 total-mutants-spawned)
  #:transparent)

(define (run-all-mutants process-q
                         config
                         will)
  (define bench (bench-info-benchmark (factory-bench (process-queue-data process-q))))
  (define select-modules (configured:select-modules-to-mutate))
  (define mutatable-module-names (select-modules bench))
  (for/fold ([process-q process-q])
            ([module-to-mutate-name mutatable-module-names]
             #:when #t
             [mutation-index (select-mutants module-to-mutate-name
                                             bench)])
    (spawn-mutant process-q
                      module-to-mutate-name
                      mutation-index
                      config
                      will
                      #:test-mutant? #f)))

;; Main entry point of the factory
(define/contract (run-all-mutants*configs bench
                                          #:log-progress log-progress!
                                          #:load-progress load-result-cache)
  (benchmark/c
   #:log-progress (module-name? natural? natural? path-to-existant-file? . -> . any)
   #:load-progress (-> (module-name? natural? natural? . -> . (or/c #f path-to-existant-file?)))
   . -> .
   boolean? ; experiment complete and sanity checks pass?
   )

  (parameterize ([current-progress-logger log-progress!]
                 [current-result-cache (load-result-cache)])
    (log-factory info @~a{Running on benchmark @bench})

    (define select-modules (configured:select-modules-to-mutate))
    (define mutatable-module-names (select-modules bench))
    (define max-config (make-max-bench-config bench))
    (log-factory info "Benchmark has mutatable modules:~n~a"
                 mutatable-module-names)

    (unless (directory-exists? (data-output-dir))
      (log-factory debug "Creating output directory ~a." (data-output-dir))
      (make-directory (data-output-dir)))

    (define select-mutants (configured:select-mutants))
    (define process-q
      (for/fold ([process-q (make-process-queue
                             (process-limit)
                             (factory (bench-info bench max-config)
                                      (hash)
                                      (hash)
                                      0)
                             < ;; lower priority value means schedule sooner (this was
                               ;; unconfigurable with the original implementation, now just
                               ;; stick to that original default)
                             #:kill-older-than (let-values ([{max-timeout _}
                                                             (increased-limits bench)])
                                                 (+ max-timeout 30)))])
                ([module-to-mutate-name mutatable-module-names]
                 #:when #t
                 [mutation-index (select-mutants module-to-mutate-name
                                                 bench)])
        (sample-blame-trails-if-max-config-result-ok process-q
                                                     (mutant #f
                                                             module-to-mutate-name
                                                             mutation-index))))

    (log-factory info "Finished enqueing all test mutants. Waiting...")
    (define process-q-finished (process-queue-wait process-q))
    (report-completion/sanity-checks bench
                                     select-mutants
                                     (load-result-cache))))

(define/contract (report-completion/sanity-checks bench
                                                  select-mutants
                                                  logged-results-for)
  (benchmark/c
   any/c
   (module-name? natural? natural? . -> . (or/c #f path-to-existant-file?))
   . -> .
   boolean? ; sanity checks pass?
   )

  (log-factory info
               "All mutants dead. Performing sanity checks...")
  (define mutatable-module-names (benchmark->mutatable-modules bench))
  (define all-mutants-should-have-trails?
    (configured:all-mutants-should-have-trails?))
  (define (trail-recorded? module-to-mutate-name
                           mutation-index
                           trail-id)
    (and (logged-results-for module-to-mutate-name
                             mutation-index
                             trail-id)
         #t))
  (define all-trails-logged-for-all-mutants?
    (for*/and ([module-to-mutate-name mutatable-module-names]
               [mutation-index (select-mutants module-to-mutate-name
                                               bench)]
               [all-trails-should-be-recorded?
                (in-value (or all-mutants-should-have-trails?
                              ;; If the first trail is there, all of them should be there.
                              ;; Otherwise, none should be there
                              (trail-recorded? module-to-mutate-name
                                               mutation-index
                                               0)))]
               [trail-id (in-range (sample-size))])
      (define trail-present?
        (trail-recorded? module-to-mutate-name
                         mutation-index
                         trail-id))
      (define consistent?
        (or (and all-trails-should-be-recorded?
                 trail-present?)
            (and (not all-trails-should-be-recorded?)
                 (not trail-present?))))
      (unless consistent?
        (log-factory
         warning
         @~a{
             Expected @(if all-trails-should-be-recorded? "all" "no") trails for @;
             @module-to-mutate-name @"@" @mutation-index to be recorded, but {@trail-id} @;
             is @(if trail-present? "recorded" "missing").
             }))
      consistent?))
  (define unexpected-state-encountered?
    (unbox abort-suppressed?))
  (define mutants-have-error-output?
    (and (file-exists? (mutant-error-log))
         ;; things like `echo '' > <log>` make it have size 1 or 2, and any real error messages will
         ;; have much more than 1 or 2
         (> (file-size (mutant-error-log)) 2)))
  (define (or-empty bool msg)
    (if bool msg ""))

  (define all-checks-pass? (and all-trails-logged-for-all-mutants?
                                (not unexpected-state-encountered?)
                                (not mutants-have-error-output?)))
  (log-factory-message
   (if all-checks-pass? 'info 'error)
   @~a{

       Experiment complete, @(if all-checks-pass?
                                 "and basic sanity checks pass."
                                 "but with failing sanity checks.")
       @(or-empty (not all-trails-logged-for-all-mutants?)
                  "⚠ Not all mutants have all of the expected blame trail samples.\n")@;
       @(or-empty unexpected-state-encountered?
                  "⚠ Some unexpected states were encountered.\n")@;
       @(or-empty mutants-have-error-output?
                  "⚠ Some mutants logged error messages.\n")
       })
  all-checks-pass?)

;; Spawns a test mutant and if that mutant has a result at
;; max contract configuration, then samples the precision lattice
;; and spawns mutants for each samples point
;; Note that sampling the precision lattice is done indirectly by
;; just generating random configs
(define/contract (sample-blame-trails-if-max-config-result-ok process-q mutant-program)
  ((process-queue/c factory/c) mutant/c . -> . (process-queue/c factory/c))

  (match-define (mutant #f module-to-mutate-name mutation-index) mutant-program)
  (log-factory debug
               "  Trying to spawn test mutant for ~a @ ~a."
               module-to-mutate-name
               mutation-index)
  (define bench (factory-bench (process-queue-get-data process-q)))
  (define max-config (bench-info-max-config bench))
  (define should-sample-mutant-blame-trails? (configured:should-sample-mutant-blame-trails?))
  (define (will:sample-if-type-error current-process-q dead-proc)
    (define ok? (should-sample-mutant-blame-trails? (process-outcome dead-proc)))
    (log-factory info
                 @~a{
                     @"  "Mutant @module-to-mutate-name @"@" @mutation-index @;
                     has @(if ok? "" "un")acceptable result according to configured filter. @;
                     @(if ok? "Sampling..." "Discarding it.")
                     })
    (if ok?
        (sample-blame-trail-roots current-process-q
                                  mutant-program)
        current-process-q))
  (define (result-cache-has-trail? number)
    (and ((current-result-cache) module-to-mutate-name
                                 mutation-index
                                 number)
         #t))
  (define result-cache-trails-for-this-mutant
    (count result-cache-has-trail?
           (range (sample-size))))
  (match result-cache-trails-for-this-mutant
    [(== (sample-size))
     (log-factory
      info
      @~a{
          Skipping sampling of mutant @;
          @module-to-mutate-name @"@" @mutation-index @;
          because all @(sample-size) trails found in cache
          })
     process-q]
    [(and n (not 0))
     (log-factory
      info
      @~a{
          Resuming sampling of mutant @;
          @module-to-mutate-name @"@" @mutation-index @;
          because @n trails found in cache
          })
     (sample-blame-trail-roots process-q
                               mutant-program)]

    [0
     ;; No trails were recorded in the cache. This might either mean that the
     ;; mutant is irrelevant, or that the mutant wasn't processed yet.

     ;; lltodo: might be able to explicitly record in the cache information to
     ;; distinguish "haven't tried this mutant yet" from "decided this mutant is
     ;; irrelevant". It would avoid having to run the test for all irrelevant
     ;; mutants when resuming.

     (log-factory
      info
      @~a{
          Spawning test mutant for @;
          @module-to-mutate-name @"@" @mutation-index @;
          because no trails found in cache
          })
     (spawn-mutant process-q
                   module-to-mutate-name
                   mutation-index
                   max-config
                   will:sample-if-type-error
                   #:test-mutant? #t)]))

(define/contract (sample-blame-trail-roots process-q mutant-program)
  ((process-queue/c factory/c) mutant/c . -> . (process-queue/c factory/c))

  (define the-factory (process-queue-get-data process-q))
  (define N-samples-please
    ((configured:make-bt-root-sampler) (factory-bench the-factory)
                                       mutant-program))
  (define samples (N-samples-please (sample-size)))
  (define (resample a-factory)
    (first (N-samples-please 1)))
  (for/fold ([current-process-q process-q])
            ([sampled-config (in-list samples)]
             [sample-number (in-naturals)])
    (log-factory
     debug
     @~a{    Sample: trying to sample root @sample-number for @mutant-program})
    (spawn-blame-trail-root-mutant current-process-q
                                   mutant-program
                                   sampled-config
                                   resample
                                   sample-number)))

;; Spawns a mutant that attempts to follow a blame trail,
;; if the given `config` doesn't cause blame for `mutant-program`
;; then it calls `resample` to get a new configuration and try again.
(define/contract (spawn-blame-trail-root-mutant process-q
                                                mutant-program
                                                config
                                                resample
                                                sample-number)
  ((process-queue/c factory/c)
   mutant/c
   config/c
   (factory/c . -> . config/c)
   natural?
   . -> .
   (process-queue/c factory/c))

  (match-define (mutant #f module-to-mutate-name mutation-index)
    mutant-program)
  (match ((current-result-cache) module-to-mutate-name
                                 mutation-index
                                 sample-number)
    [#f
     (define this-trail
       (blame-trail sample-number
                    empty))
     (spawn-mutant process-q
                   module-to-mutate-name
                   mutation-index
                   config
                   (make-blame-following-will/fallback
                    (λ (current-process-q dead-proc)
                      (log-factory
                       info
                       "    Sample ~a (id [~a]) for ~a @ ~a failed to find blame."
                       sample-number
                       (dead-mutant-process-id dead-proc)
                       (mutant-module (dead-mutant-process-mutant dead-proc))
                       (mutant-index (dead-mutant-process-mutant dead-proc)))
                      (match ((configured:root-missing-blame-response)
                              (run-status-outcome (dead-mutant-process-result dead-proc)))
                        ['bt-failed
                         (terminate-and-record-blame-trail! current-process-q
                                                            this-trail
                                                            dead-proc)]
                        ['resample
                         ;; Try sampling another config
                         (define new-sample
                           (resample (process-queue-get-data current-process-q)))
                         (spawn-blame-trail-root-mutant current-process-q
                                                        mutant-program
                                                        new-sample
                                                        resample
                                                        sample-number)]
                        ['error
                         (maybe-abort "BT root is missing blame"
                                      (terminate-and-record-blame-trail! current-process-q
                                                                         this-trail
                                                                         dead-proc))])))
                   #:following-trail this-trail)]
    [path-to-data-file
     (log-factory
      info
      @~a{
          Blame trail @;
          @module-to-mutate-name @"@" @mutation-index {@sample-number} @;
          found in progress cache: @(pretty-path path-to-data-file)
          })
     process-q]))

;; Enqueues a mutant that follows the blame trail starting at `dead-proc`,
;; via `locations-selected-as-blamed`
(define/contract (follow-blame-from-dead-process the-process-q
                                                 dead-proc
                                                 handle-no-blame)
  (->i ([the-process-q                (process-queue/c factory/c)]
        [dead-proc                    dead-mutant-process/c]
        [handle-no-blame              ((process-queue/c factory/c)
                                       dead-mutant-process/c
                                       . -> .
                                       (process-queue/c factory/c))])
       [result (process-queue/c factory/c)])

  (match-define (dead-mutant-process (mutant #f mod index)
                                     config
                                     result
                                     id
                                     the-blame-trail
                                     _)
    dead-proc)
  (define the-blame-trail+dead-proc
    (extend-blame-trail the-blame-trail
                        dead-proc))

  (log-factory debug
               @~a{
                   [@id] completed with outcome @(run-status-outcome result), and
                     blamed:     @(run-status-blamed result)
                     errortrace: @(run-status-errortrace-stack result)
                     context:    @(run-status-context-stack result)
                   })

  (define select-blamed-locations-to-follow
    (configured:follow-blame))
  (define blame-trail-ended-normally? (configured:blame-trail-ended-normally?))
  (define normal-blame-trail-outcome?
    (member (run-status-outcome result)
            normal-blame-trail-outcomes))

  (match (and normal-blame-trail-outcome?
              (select-blamed-locations-to-follow result config))
    [(? list? locations-selected-as-blamed)
     #:when (blame-trail-ended-normally? dead-proc
                                         locations-selected-as-blamed
                                         log-factory-message)
     (log-factory debug
                  @~a{
                      Blame trail @mod @"@" @index @;
                      {@(blame-trail-id the-blame-trail+dead-proc)} @;
                      ended normally according to configured predicate. @;
                      (Configured blame follower chose locations: @locations-selected-as-blamed)
                      })

     ;; Log the trail and stop following.
     (define new-factory
       (record-blame-trail! (process-queue-get-data the-process-q)
                            the-blame-trail+dead-proc))
     (process-queue-set-data the-process-q
                         new-factory)]

    [(list locations-selected-as-blamed ..1)
     (log-factory debug
                  @~a{
                      Selected "blamed" modules to make successor of [@id]: @;
                      @locations-selected-as-blamed
                      })
     (define config/blamed-region-ctc-strength-incremented
       (increment-config-precision-for-all (set-intersect locations-selected-as-blamed
                                                          (hash-keys config))
                                           config
                                           #:increment-max-error? #f))
     (define (spawn-the-blame-following-mutant a-process-q
                                               #:timeout/s [timeout/s #f]
                                               #:memory/gb [memory/gb #f])
       (spawn-mutant a-process-q
                     mod
                     index
                     config/blamed-region-ctc-strength-incremented
                     will:keep-following-blame
                     #:following-trail the-blame-trail+dead-proc
                     #:timeout/s timeout/s
                     #:memory/gb memory/gb))
     (define will:keep-following-blame
       (make-blame-following-will/fallback
        (make-blame-disappearing-fallback dead-proc
                                          spawn-the-blame-following-mutant)))
     (spawn-the-blame-following-mutant the-process-q)]

    [else
     (log-factory debug
                  @~a{
                      [@id] has no modules selected as "blamed", and has not terminated normally: @;
                      calling handler for missing blame.
                      })
     (handle-no-blame the-process-q dead-proc)]))

(define/contract (extend-blame-trail a-blame-trail a-dead-proc)
  (blame-trail/c dead-mutant-process/c . -> . blame-trail/c)

  (match a-blame-trail
    [(blame-trail id parts)
     (blame-trail id
                  (cons a-dead-proc parts))]))

(define (increased-limits bench)
  (values (* 2 (default-timeout/s))
          (* 2 (default-memory-limit/gb))))

(define/contract (make-blame-disappearing-fallback dead-proc
                                                   respawn-mutant)
  (dead-mutant-process/c
   ((process-queue/c factory/c)
    #:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)
    . -> .
    (process-queue/c factory/c))
   . -> .
   ((process-queue/c factory/c) dead-mutant-process/c . -> . (process-queue/c factory/c)))

  (match-define (dead-mutant-process (mutant #f mod index)
                                     config
                                     result
                                     id
                                     (blame-trail blame-trail-id _)
                                     _)
    dead-proc)
  (λ (current-process-q dead-successor)
    (match-define
      (dead-mutant-process _
                           dead-succ-config
                           dead-succ-result
                           dead-succ-id
                           the-blame-trail
                           increased-limits?)
      dead-successor)
    (define (record-this-bt-failed!)
      (terminate-and-record-blame-trail! current-process-q
                                         the-blame-trail
                                         dead-successor))
    (match* {(run-status-outcome dead-succ-result) increased-limits?}
      [{(and outcome (or 'timeout 'oom)) #f}
       (log-factory info
                    "
Re-spawning mutant ~a @ ~a on blame trail {~a} with increased limits.
Previous mutant [~a] exceeded limits with: ~v
"
                    mod index blame-trail-id
                    dead-succ-id outcome)
       (define-values {timeout* memory*}
         (increased-limits
          (factory-bench (process-queue-get-data current-process-q))))
       (respawn-mutant current-process-q
                       #:timeout/s timeout*
                       #:memory/gb memory*)]
      [{(and outcome (or 'timeout 'oom)) #t}
       (log-factory warning
                    "
Unable to continue following blame trail ~a @ ~a {~a}.
Mutant: [~a] exceeded limits with: ~v
Giving up.
"
                    mod index blame-trail-id
                    dead-succ-id outcome)
       (record-this-bt-failed!)]
      [{(or 'blamed 'type-error) _}
       (log-factory
        info
        @~a{
            BT VIOLATION: @;
            All blame entered library code while following blame trail @;
            @mod @"@" @index {@blame-trail-id}. @;
            Giving up on following the trail.
            Mutant: [@dead-succ-id] and config:
            @~v[(serialize-config dead-succ-config)]

            Result: @(run-status-outcome result) on @(run-status-blamed result)
            })
       (record-this-bt-failed!)]
      [{'runtime-error _}
       (log-factory
        info
        @~a{
            BT VIOLATION: @;
            Unable to infer program location from runtime error on trail @;
            @mod @"@" @index {@blame-trail-id}. @;
            Giving up on following the trail.
            Mutant: [@dead-succ-id] and config:
            @~v[(serialize-config dead-succ-config)]

            Blamed: @(run-status-blamed result)
            })
       (record-this-bt-failed!)]
      [{(or 'completed 'syntax-error) _}
       (log-factory
        error
        @~a{
            Mutant in middle of blame trail completes or syntax-errors.
            Mutant: @mod @"@" @index [@dead-succ-id] {@blame-trail-id}.
            Config: @(serialize-config dead-succ-config)

            Predecessor (id [@id]) had result @result
            })
       (maybe-abort "Blame disappeared" (record-this-bt-failed!))]
      [{outcome _}
       (log-factory
        error
        @~a{
            Blame has disappeared for unexpected reasons.
            Mutant: @dead-proc
            })
       (maybe-abort "Blame disappeared" (record-this-bt-failed!))])))

(define/contract (make-blame-following-will/fallback no-blame-fallback)
  (mutant-will/c . -> . mutant-will/c)

  (λ (the-process-q dead-proc)
    (log-factory debug
                 @~a{
                     Pursuing blame trail @;
                     {@(blame-trail-id (dead-mutant-process-blame-trail dead-proc))} @;
                     from [@(dead-mutant-process-id dead-proc)]
                     })
    (follow-blame-from-dead-process the-process-q
                                    dead-proc
                                    no-blame-fallback)))


(define/contract (spawn-mutant process-q
                               module-to-mutate-name
                               mutation-index
                               precision-config
                               mutant-will
                               [revival-counts (revivals 0 0)]
                               #:timeout/s [timeout/s #f]
                               #:memory/gb [memory/gb #f]
                               #:following-trail [trail-being-followed #f]
                               #:test-mutant? [test-mutant? #f])
  (->i ([process-q              (process-queue/c factory/c)]
        [module-to-mutate-name  module-name?]
        [mutation-index         natural?]
        [precision-config       config/c]
        [mutant-will            mutant-will/c])
       ([revival-counts revivals/c]
        #:following-trail [trail-being-followed  (or/c #f blame-trail/c)]
        #:test-mutant?    [test-mutant?          boolean?]
        #:timeout/s       [t/s                   (or/c #f number?)]
        #:memory/gb       [m/gb                  (or/c #f number?)])
       #:pre/desc {trail-being-followed test-mutant?}
       (or (= 1 (count (disjoin unsupplied-arg? false?)
                       (list trail-being-followed test-mutant?)))
           @~a{
               Exactly one of @;
               #:following-trail (@trail-being-followed) @;
               and #:test-mutant? (@test-mutant?) @;
               must be specified.
               })
       [result (process-queue/c factory/c)])

  (define current-factory (process-queue-get-data process-q))
  (match-define (factory (bench-info the-benchmark _)
                         _
                         _
                         mutants-spawned)
    current-factory)
  (define outfile (build-path (data-output-dir)
                              (format "~a_index~a_~a.rktd"
                                      module-to-mutate-name
                                      mutation-index
                                      mutants-spawned)))
  (define the-benchmark-configuration
    (configure-benchmark the-benchmark
                         precision-config))

  (define mutant-id mutants-spawned)
  (define mutant-blame-trail
    (cond [trail-being-followed => values]
          [test-mutant?            (blame-trail test-mutant-flag
                                                '())]))
  (define (spawn-the-mutant)
    (define mutant-ctl
      (spawn-mutant-runner the-benchmark-configuration
                           module-to-mutate-name
                           mutation-index
                           outfile
                           (current-configuration-path)
                           #:timeout/s timeout/s
                           #:memory/gb memory/gb
                           #:save-output (and debug:save-individual-mutant-outputs?
                                              (build-path (data-output-dir)
                                                          (format "~a.rktd"
                                                                  mutant-id)))))
    (define mutant-proc
      (mutant-process (mutant #f module-to-mutate-name mutation-index)
                      precision-config
                      outfile
                      mutant-id
                      mutant-blame-trail
                      revival-counts
                      ;; coerce to bool
                      (and (or timeout/s memory/gb) #t)))
    (log-factory
     info
     "    Spawned mutant runner with id [~a] for ~a @ ~a > ~a."
     mutant-id
     module-to-mutate-name
     mutation-index
     (pretty-path outfile))
    (process-info mutant-proc
                  mutant-ctl
                  (mutant->process-will mutant-will)))
  (log-factory
   debug
   @~a{    Mutant [@mutant-id] has config @~v[(serialize-config precision-config)]})

  ;; lower priority means schedule sooner
  (define this-mutant-priority
    (if test-mutant?
        ; test mutants have the worst priority: we want to finish mutants faster
        1
        ; mutants progressing along a blame trail should be prioritized to finish the trail quickly
        (- (length (blame-trail-parts mutant-blame-trail)))))
  (process-queue-enqueue
   (process-queue-set-data process-q
                       (copy-factory current-factory
                                     [total-mutants-spawned
                                      (add1 mutants-spawned)]))
   spawn-the-mutant
   this-mutant-priority))

;; There is some common housekeeping that must be performed in every mutant
;; will, regardless of what kind of mutant or the details of its particular will.
;; In particular:
;; - Respawning the mutant a limited number of times if the mutant fails
;; - Otherwise, converting the mutant-process into a dead-mutant-process
;; - Logging the result of the mutant (and updating the factory with it)
(define/contract (mutant->process-will mutant-will)
  (mutant-will/c . -> . process-will/c)

  (define (outer-will process-q the-process-info)
    (match-define (and mutant-proc
                       (mutant-process (mutant #f mod index)
                                       config
                                       file
                                       id the-blame-trail
                                       revival-counts
                                       increased-limits?))
      (process-info-data the-process-info))
    ;; Read the result of the mutant before possible consolidation
    (define status ((process-info-ctl the-process-info) 'status))
    (define maybe-result
      ;; ll: Check before reading to reduce the number of warnings
      ;; emitted for an errored mutant, otherwise would warn wrong
      ;; output as well as error
      (if (equal? status 'done-ok)
          (read-mutant-result mutant-proc)
          (file->string (mutant-process-file mutant-proc))))
    (match (cons status maybe-result)
      [(or (cons 'done-error _)
           (cons 'done-ok (? eof-object?)))
       (maybe-revive-failed-mutant process-q
                                   mutant-proc
                                   status
                                   maybe-result
                                   mutant-will)]
      [(cons 'done-ok (? run-status? result))
       (log-factory info
                    @~a{
                        Sweeping up dead mutant [@id]: @mod @"@" @index, @;
                        result: @;
                        @(match result
                           [(struct* run-status
                                     ([outcome (and outcome
                                                    (or 'blamed
                                                        'type-error
                                                        'runtime-error))]
                                      [blamed (list (? string? mod-names) ...)]))
                            (~a outcome " blaming " mod-names)]
                           [(struct* run-status
                                     ([outcome (and outcome
                                                    'runtime-error)]
                                      [blamed #f]))
                            "runtime-error without inferred blame"]
                           [(struct* run-status
                                     ([outcome o]))
                            o]), @;
                        config: @~s[(serialize-config config)]
                        })
       (match (record/check-configuration-outcome! mutant-proc result)
         [#t
          (revive-type-error-mutant process-q
                                    mutant-proc
                                    status
                                    maybe-result
                                    mutant-will)]
         [#f
          (define dead-mutant-proc
            (dead-mutant-process (mutant #f mod index)
                                 config
                                 result
                                 id
                                 the-blame-trail
                                 increased-limits?))
          (delete-file file)
          (mutant-will process-q dead-mutant-proc)])]))
  outer-will)

(define/contract (maybe-revive-failed-mutant process-q
                                             a-mutant-process
                                             status
                                             maybe-result
                                             mutant-will)
  (->i ([process-q         (process-queue/c factory/c)]
        [a-mutant-process  mutant-process/c]
        [status            (or/c 'done-ok 'done-error)]
        [maybe-result      {status}
                           (match status
                             ['done-ok eof-object?]
                             ['done-error any/c])]
        [mutant-will       mutant-will/c])
       [result (process-queue/c factory/c)])

  (match-define (struct* mutant-process
                         ([id             id]
                          [blame-trail    the-blame-trail]
                          [mutant         (mutant #f mod index)]
                          [config         config]
                          [revival-counts (revivals for-failure
                                                    for-type-error)]))
    a-mutant-process)

  (cond [(>= for-failure MAX-FAILURE-REVIVALS)
         (log-factory error
                      "Runner errored all ~a / ~a tries on mutant:
 [~a] ~a @ ~a with config
~v"
                      for-failure MAX-FAILURE-REVIVALS
                      id mod index
                      (serialize-config config))
         (maybe-abort "Revival failed to resolve mutant errors"
                      process-q)]
        [else
         (log-factory warning
                      "Runner errored on mutant [~a] ~a @ ~a with config
~v

Exited with ~a and produced result: ~v

Attempting revival ~a / ~a
"
                      id mod index
                      (serialize-config config)
                      status maybe-result
                      (add1 for-failure) MAX-FAILURE-REVIVALS)
         (spawn-mutant process-q
                       mod
                       index
                       config
                       mutant-will
                       (revivals (add1 for-failure)
                                 for-type-error)
                       #:following-trail (match the-blame-trail
                                           [(? blame-trail? bt) bt]
                                           [else                #f])
                       #:test-mutant? (equal? the-blame-trail
                                              test-mutant-flag))]))

(define/contract (revive-type-error-mutant process-q
                                           a-mutant-process
                                           status
                                           maybe-result
                                           mutant-will)
  (->i ([process-q         (process-queue/c factory/c)]
        [a-mutant-process  mutant-process/c]
        [status            'done-ok]
        [maybe-result      (property/c run-status-outcome 'type-error)]
        [mutant-will       mutant-will/c])
       [result (process-queue/c factory/c)])

  (match-define (struct* mutant-process
                         ([id             id]
                          [blame-trail    the-blame-trail]
                          [mutant         (mutant #f mod index)]
                          [config         config]
                          [revival-counts (revivals for-failure
                                                    for-type-error)]))
    a-mutant-process)

  (log-factory info
               @~a{
                   Retrying @mod @"@" @index [@id] to verify it really has a type-error. @;
                   Retry # @(add1 for-type-error) / @MAX-TYPE-ERROR-REVIVALS
                   })
  (spawn-mutant process-q
                mod
                index
                config
                mutant-will
                (revivals for-failure (add1 for-type-error))
                #:following-trail (match the-blame-trail
                                    [(? blame-trail? bt) bt]
                                    [else                #f])
                #:test-mutant? (equal? the-blame-trail
                                       test-mutant-flag)))

(define (mutant-data-file-name mod-name mutation-index)
  @~a{
      @|mod-name|@;
      _@;
      @|mutation-index|@;
      .rktd
      })

(define/contract (record-blame-trail! the-factory the-blame-trail)
  (factory/c
   (and/c blame-trail/c
          (flat-named-contract
           'blame-trail-of-length-at-least-one
           (match-lambda [(blame-trail _ (not '())) #t]
                         [else #f])))
   . -> .
   factory/c)

  (match-define (and the-mutant
                     (mutant #f module-to-mutate-name mutation-index))
    (dead-mutant-process-mutant (first (blame-trail-parts the-blame-trail))))
  (define the-results (factory-results the-factory))
  (define mutant-data-file
    (match (hash-ref the-results the-mutant #f)
      [#f (build-path (data-output-dir)
                      (mutant-data-file-name module-to-mutate-name
                                             mutation-index))]
      [path path]))
  (append-blame-trail-to-mutant-data! mutant-data-file
                                      the-blame-trail)
  ((current-progress-logger) module-to-mutate-name
                             mutation-index
                             (blame-trail-id the-blame-trail)

                             mutant-data-file)
  (copy-factory the-factory
                [results (hash-set the-results the-mutant mutant-data-file)]))

(define (append-blame-trail-to-mutant-data! mutant-data-file
                                            the-blame-trail)
  (define summary (summarize-blame-trail the-blame-trail))
  (with-output-to-file mutant-data-file
    #:mode 'text
    #:exists 'append
    (λ _ (writeln summary))))

(define (summarize-blame-trail the-blame-trail)
  (define mutant-summaries
    (map summarize-dead-mutant-process
         (blame-trail-parts the-blame-trail)))
  (match-define (list* (struct* dead-mutant-process
                                ([mutant (mutant #f mod index)]))
                       _)
    (blame-trail-parts the-blame-trail))
  (blame-trail-summary mod
                       index
                       (blame-trail-id the-blame-trail)
                       mutant-summaries))

(define summarize-dead-mutant-process
  (match-lambda [(struct* dead-mutant-process
                          ([id id]
                           [result result]
                           [config config]))
                 (mutant-summary id
                                 result
                                 (serialize-config config))]))

;; Adds `dead-proc` at the end of `the-blame-trail/without-dead-proc` and
;; records the resulting trail
(define/contract (terminate-and-record-blame-trail! the-process-q
                                                    the-blame-trail/without-dead-proc
                                                    dead-proc)
  (->i ([the-process-q                      (process-queue/c factory/c)]
        [the-blame-trail/without-dead-proc  blame-trail/c]
        [dead-proc                          dead-mutant-process/c])
       [result (process-queue/c factory/c)])

  (define the-blame-trail+dead-proc
    (extend-blame-trail the-blame-trail/without-dead-proc
                        dead-proc))
  (define new-factory
    (record-blame-trail! (process-queue-get-data the-process-q)
                         the-blame-trail+dead-proc))
  (process-queue-set-data the-process-q
                          new-factory))


(define/contract (read-mutant-result mutant-proc)
  (mutant-process/c . -> . (or/c run-status? eof-object?))

  (define path (mutant-process-file mutant-proc))
  (define (report-malformed-output . _)
    (match-define (mutant-process (mutant _ mod index) config _ id _ _ _)
      mutant-proc)
    (log-factory error
                 "Result read from mutant output not of the expected shape.
Expected: a run-status with a valid pair of outcome/blamed
Found: ~v
If this has the right shape, it may contain an unreadable value.

Mutant: [~a] ~a @ ~a with config:
~v
"
                 (file->string path)
                 id mod index
                 (serialize-config config))
    eof)
  (with-handlers ([exn:fail:read? report-malformed-output])
    (match (with-input-from-file path read)
      [(and (or (struct* run-status
                         ([outcome (or 'completed
                                       'syntax-error
                                       'timeout
                                       'oom)]
                          [blamed #f]
                          [errortrace-stack #f]
                          [context-stack #f]))
                (struct* run-status
                         ([outcome 'type-error]
                          [blamed (not #f)]
                          [errortrace-stack #f]
                          [context-stack #f]))
                (struct* run-status
                         ([outcome (or 'blamed
                                       'runtime-error)]
                          [blamed (not #f)]
                          [errortrace-stack (? list?)]
                          [context-stack (? list?)]))
                (struct* run-status
                         ([outcome 'runtime-error]
                          [blamed #f]
                          [errortrace-stack (? list?)]
                          [context-stack (? list?)])))
            result/well-formed)
       result/well-formed]
      [else (report-malformed-output)])))

;; dead-mutant-process? -> run-outcome/c
(define (process-outcome dead-proc)
  (run-status-outcome (dead-mutant-process-result dead-proc)))

(define abort-suppressed? (box #f))
(define (maybe-abort reason continue-val #:force [force? #f])
  ;; Mark the mutant error file before it gets garbled with error
  ;; messages from killing the current active mutants
  (log-factory error "Received abort signal with reason: ~a" reason)
  (cond [(or (abort-on-failure?) force?)
         (call-with-output-file (mutant-error-log)
           #:exists 'append #:mode 'text
           (λ (out)
             (fprintf out
                      "
~n~n~n~n~n~n~n~n~n~n
================================================================================
                              Factory aborting
  Reason: ~a
================================================================================
~n~n~n~n~n~n~n~n~n~n
"
                      reason)))
         (exit 1)
         ;; Return continue-val in case of custom exit handler.
         ;; This should probably only matter for testing, so that the
         ;; contracts of various functions don't blow up.
         continue-val]
        [else
         (log-factory warning
                      "Continuing execution despite abort signal...")
         (set-box! abort-suppressed? #t)
         continue-val]))

(define (pretty-path p)
  (path->string (find-relative-path (simple-form-path (current-directory))
                                    (simple-form-path p))))

;; mutant-process? run-status? -> boolean?
;;
;; Checks the outcome of the given configuration, possibly recording or
;; reporting an error about it, and returns whether to retry the process or
;; continue with executing its will.
;;
;; A config needs to be retried a few times if it produces a type-error in order
;; to verify that it really produces a type-error, thanks to a hard-to-pin-down
;; bug in TR that very occaisonally causes some programs to raise "duplicate
;; annotation" type errors for no apparent reason whatsoever. We don't know what
;; causes it or how to fix it, so we need to work around it by checking a few
;; times that any type error really is a type-error.
(define (record/check-configuration-outcome! mutant-proc result)
  (match-define (struct* mutant-process ([mutant mutant]
                                         [config config]
                                         [revival-counts (revivals _ for-type-error)]))
    mutant-proc)
  (define (get-outcome outcome-for)
    (outcome-for mutant config))
  (define retry #t)
  (define continue #f)
  (match* {result (record/check-configuration-outcomes?)}
    [{(struct* run-status ([outcome 'type-error]))
      `(record ,record!)}
     #:when (< for-type-error MAX-TYPE-ERROR-REVIVALS)
     retry]
    [{(struct* run-status ([outcome outcome]))
      `(record ,record!)}
     (record! mutant config outcome)
     continue]

    [{(struct* run-status ([outcome 'type-error]))
      `(check ,(app get-outcome (not 'type-error)))}
     #:when (< for-type-error MAX-TYPE-ERROR-REVIVALS)
     retry]
    [{(struct* run-status ([outcome (and real-outcome (not (or 'timeout 'oom)))]))
      `(check ,(app get-outcome recorded-outcome))}
     #:when (not (outcome-compatible-with? recorded-outcome real-outcome))
     (maybe-abort
      @~a{
          Found that a configuration produces @real-outcome, but configuration outcomes db @;
          says that it should produce something compatible with @recorded-outcome
          Mutant: @mutant @(serialize-config config)
          }
      continue)]

    [{_ _}
     (match (record/check-configuration-outcomes?)
       [`(check ,checker)
        (log-factory debug @~a{outcome check: @result looks ok! checker says it should be: @(get-outcome checker)})]
       [else (void)])
     continue]))

(define (outcome-compatible-with? recorded actual)
  (match (list recorded actual)
    [(list (no-recorded-outcome) _)                 #t]
    [(list-no-order 'syntax-error _)                #f]
    [(list 'type-error 'type-error)                 #t]
    [(list-no-order 'type-error (not 'type-error )) #f]
    [else
     ;; all other modes should be 'weaker' than TR's recorded result
     (define ordering '(blamed runtime-error completed))
     (unless (and (index-of ordering actual)
                  (index-of ordering recorded))
       (log-factory error
                    @~a{Outcome checking: unrecognized outcome in @recorded or @actual ?}))
     (>= (index-of ordering actual)
         (index-of ordering recorded))]))

(define progress-log (make-parameter #f))
(define (make-cached-results-for progress-info-hash)
  (λ (module-to-mutate-name
      mutation-index
      sample-number)
    (hash-ref progress-info-hash
              (list module-to-mutate-name
                    mutation-index
                    sample-number)
              #f)))
(define (make-progress-logger log-progress!/raw)
  (λ (module-to-mutate-name
      mutation-index
      sample-number

      data-file)
    (log-progress!/raw (cons (list module-to-mutate-name
                                   mutation-index
                                   sample-number)
                             ;; ensure it's an absolute path in case we resume
                             ;; from another directory
                             (path->string
                              (simple-form-path data-file))))))

(define (setup-configuration-outcome-record/checking!)
  (match (record/check-configuration-outcomes?)
    [`(record ,path)
     (make-parent-directory* path)
     (define-values {log-outcome!/raw finalize-log!}
       (initialize-progress-log! path
                                 #:exists 'append))
     (define (log-outcome! mutant config outcome)
       (log-outcome!/raw (cons (list mutant (serialize-config config)) outcome)))
     (record/check-configuration-outcomes? `(record ,log-outcome!))
     finalize-log!]
    [`(check ,path)
     (define outcomes (make-immutable-hash (file->list path)))
     (define (outcome-for mutant config)
       (hash-ref outcomes
                 (list mutant (serialize-config config))
                 (thunk
                  (log-factory info
                               @~a{
                                   Outcome checking: no outcome found in log for @;
                                   @mutant @(serialize-config config)
                                   })
                  (no-recorded-outcome))))
     (record/check-configuration-outcomes? `(check ,outcome-for))
     void]
    [else void]))

(module+ main
  (require racket/cmdline
           (prefix-in db: "../db/db.rkt"))
  (define bench-path-to-run (make-parameter #f))
  (define metadata-file (make-parameter #f))
  (define configuration-path (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    path
    "Path to benchmark to run."
    (bench-path-to-run path)]
   [("-c" "--config")
    path
    "Path to the configuration to use."
    (configuration-path path)]
   [("-o" "--output-dir")
    dir
    "Data output directory."
    (data-output-dir dir)]
   [("-n" "--process-limit")
    n
    "Number of processes to have running at once."
    (process-limit (string->number n))]
   [("-e" "--error-log")
    path
    "File to which to append mutant errors. Default: ./mutant-errors.txt"
    (mutant-error-log path)]
   [("-k" "--keep-going")
    "Continue despite encountering failure conditions. Default: #f"
    (abort-on-failure? #f)]
   [("-s" "--sample-size")
    n
    "Number of blame trail roots to sample. Default: 96"
    (sample-size (string->number n))]
   [("-l" "--progress-log")
    path
    ("Record progress in the given log file."
     "If it exists and is not empty, resume from the point reached in the log.")
    (progress-log path)]
   [("-m" "--metadata")
    path
    ("Record metadata about the configuration used to run an experiment in the given file."
     "This is useful information, and it is used to prevent resuming an experiment with a"
     "different configuration than it was started with.")
    (metadata-file path)]
   [("-P" "--record-configuration-outcomes") ; p for parity
    log-path
    ("For every configuration visited, record in the given db (which may not exist yet) the"
     "configuration's outcome."
     "Upon completion, the db can be used with `-p` (which see).")
    (record/check-configuration-outcomes? `(record ,log-path))]
   [("-p" "--check-configuration-outcomes")
    log-path
    ("Check the outcome of every configuration visited against those recorded in the given db"
     "(produced by `-P`). If the outcome of a configuration does not match the outcome recorded"
     "in the db, signal a fatal error.")
    (record/check-configuration-outcomes? `(check ,log-path))])

  (unless (bench-path-to-run)
    (raise-user-error 'mutant-factory "Error: must provide benchmark to run."))
  (unless (configuration-path)
    (raise-user-error 'mutant-factory "Error: must provide a configuration."))

  (install-configuration! (configuration-path))

  (define bench-to-run (read-benchmark (bench-path-to-run)))

  (when (and (directory-exists? (data-output-dir))
             (not (progress-log)))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))

  (when (metadata-file)
    (define info
      (metadata-info (metadata-file)
                     (bench-path-to-run)
                     (configuration-path)
                     (record/check-configuration-outcomes?)))
    (unless (create/check-metadata-integrity! info)
      (raise-user-error
       'mutant-factory
       @~a{
           This appears to be a resumption of the experiment, but the metadata recorded at @;
           @(metadata-file) for the previous run does not match that of this run's configuration.
           Recorded info: @(~s (file->value (metadata-file)))
           This run info: @(~s (metadata-info->id info))
           Aborting.
           })))

  (define finalize-configuration-outcomes!
    (setup-configuration-outcome-record/checking!))

  (define (make-cached-results-function)
    (define progress-info-hash
      (match (progress-log)
        [(? file-exists? path) (make-immutable-hash (file->list path))]
        [else (hash)]))
    (make-cached-results-for progress-info-hash))
  (define-values {log-progress!/raw finalize-log!}
    (initialize-progress-log! (progress-log)
                              #:exists 'append))

  (log-factory info
               @~a{
                   Running experiment with config @;
                   @(configuration-path)
                   })

  (define completed+checks-pass?
    (parameterize ([date-display-format 'iso-8601])
      (run-all-mutants*configs bench-to-run
                               #:log-progress (make-progress-logger log-progress!/raw)
                               #:load-progress make-cached-results-function)))

  (finalize-log!)
  (finalize-configuration-outcomes!)

  (exit (if completed+checks-pass? 0 1)))
