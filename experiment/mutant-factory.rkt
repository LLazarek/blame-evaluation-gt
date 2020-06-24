#lang at-exp racket

(require "../runner/mutation-runner.rkt"
         "../runner/program.rkt"
         "../runner/unify-program.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "../util/optional-contracts.rkt"
         "../util/mutant-util.rkt"
         "../util/progress-log.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../process-q/interface.rkt"
         "../process-q/priority.rkt"
         "../configurables/configurables.rkt"
         "blame-trail-data.rkt"
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
           MAX-REVIVALS
           sample-size
           copy-factory
           mutant-error-log
           test-mutant-flag
           current-result-cache

           run-all-mutants*configs
           sample-blame-trails-if-type-error
           sample-blame-trail-roots
           extend-blame-trail
           record-blame-trail!
           mutant-data-file-name
           record-sampled-config
           follow-blame-from-dead-process
           make-blame-disappearing-fallback
           spawn-mutant
           read-mutant-result
           process-outcome
           try-get-blamed-modules
           try-get-type-error-module
           mutant->process-will
           make-blame-following-will/fallback))

(define debug:save-individual-mutant-outputs? #f)


(define MAX-CONFIG 'types)
(define MAX-REVIVALS 3)

(define-runtime-path benchmarks-dir-path "../../gtp-benchmarks/benchmarks/")

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define sample-size (make-parameter 96))
(define abort-on-failure? (make-parameter #t))

(define sample-without-replacement? (make-parameter #f))

(define current-result-cache (make-parameter (λ _ #f)))
(define current-progress-logger (make-parameter void))


(define-logger factory)
(define-syntax-rule (log-factory level msg v ...)
  (when (log-level? factory-logger 'level)
    (log-message factory-logger
                 'level
                 (format
                  (string-append "[~a] "
                                 (if (member 'level '(fatal error warning))
                                     (failure-msg 'level msg)
                                     msg))
                  (date->string (current-date) #t)
                  v ...)
                 #f)))
(define (failure-msg failure-type m)
  (string-append "***** " (~a failure-type) " *****\n" m "\n**********"))

;; see last result of `process`
(define process-ctl? procedure?)

;; module:    path-string?
;; index:     natural?
;; abstract?: boolean? ; is module a `module-name?` rather than a resolved path?
(struct mutant (module index abstract?) #:transparent)

(struct blame-trail (id parts) #:transparent)

;; will?           := (factory? dead-mutant-process? -> factory?)
;; result/c        := run-status/c (see `mutation-runner.rkt`)
;; ctc-level?      := symbol?
;; config?         := (hash path-string?
;;                         (hash (or symbol? path-string?) ctc-level?))
;; blame-trail-id? := (or natural? 'no-blame)

;; mutant:         mutant?
;; config:         config?
;; file:           path-string?
;; id:             natural?
;; blame-trail-id: blame-trail-id?
;; blame-trail:    (listof dead-mutant-process/c)
;; revival-count:  natural?
;; increased-limits?: boolean?
(struct mutant-process (mutant
                        config
                        file
                        id
                        blame-trail
                        revival-count
                        increased-limits?)
  #:transparent)

;; result: result/c
(struct dead-mutant-process (mutant
                             config
                             result
                             id
                             ;; INVARIANT: `blame-trail` does NOT contain this
                             ;; dead process.
                             blame-trail
                             increased-limits?)
  #:transparent)

(define mutant-results? (hash/c mutant?
                                path-to-existant-file?))

;; benchmark:  benchmark/c
;; max-config: config?
(struct bench-info (benchmark max-config) #:transparent)

;; bench: bench-info?
;; results: mutant-results?
;;     The map from mutant to blame trail data file
;; mutant-samples: mutant? |-> (set config?)
;;     The set of precision config samples checked for each mutant.
;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench
                 results
                 mutant-samples
                 total-mutants-spawned)
  #:transparent)


(define-simple-macro (copy-factory
                      a-factory:expr field-val-pair:expr ...)
  (struct-copy factory a-factory field-val-pair ...))

(define test-mutant-flag 'test)

(define blame-labels? (listof module-name?))
(define mutant/c
  (struct/dc mutant
             [module (abstract?) (if abstract?
                                     module-name?
                                     path-to-existant-file?)]
             [index natural?]
             [abstract? boolean?]))
(define result/c run-status/c)
(define blame-trail-id? (or/c natural?
                              test-mutant-flag))
(define blame-trail/c
  (struct/dc blame-trail
             [id     blame-trail-id?]
             [parts  (listof (recursive-contract dead-mutant-process/c #:chaperone))]))
(define mutant-process/c
  (struct/dc mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [file               path-string?]
             [id                 natural?]
             [blame-trail        blame-trail/c]
             [revival-count      natural?]
             [increased-limits?  boolean?]))
(define dead-mutant-process/c
  (struct/dc dead-mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [result             result/c]
             [id                 natural?]
             [blame-trail        blame-trail/c]
             [increased-limits?  boolean?]))
(define bench-info/c
  (struct/dc bench-info
             [benchmark   benchmark/c]
             [max-config  config/c]))
(define factory/c
  (struct/dc factory
             [bench                   bench-info/c]
             [results                 mutant-results?]
             [mutant-samples          (hash/c mutant/c (set/c config/c))]
             [total-mutants-spawned   natural?]))

(define mutant-will/c
  ((process-Q/c factory/c) dead-mutant-process/c . -> . (process-Q/c factory/c)))




;; Main entry point of the factory
(define/contract (run-all-mutants*configs bench
                                          #:log-progress log-progress!
                                          #:resume-cache cached-results-for)
  (benchmark/c
   #:log-progress (module-name? natural? natural? path-to-existant-file? . -> . any)
   #:resume-cache (module-name? natural? natural? . -> . (or/c #f path-to-existant-file?))
   . -> .
   mutant-results?)

  (parameterize ([current-progress-logger log-progress!]
                 [current-result-cache cached-results-for])
    (log-factory info
                 @~a{
                     Running experiment with config @;
                     @(current-configuration-path)
                     })

    (define mutatable-module-names (benchmark->mutatable-modules bench))
    (define max-config (make-max-bench-config bench))
    (log-factory info "Benchmark has modules:~n~a"
                 mutatable-module-names)

    (unless (directory-exists? (data-output-dir))
      (log-factory debug "Creating output directory ~a." (data-output-dir))
      (make-directory (data-output-dir)))

    (define select-mutants
      (load-configured (current-configuration-path)
                       "mutant-sampling"
                       'select-mutants))
    (define process-q
      (for/fold ([process-q (make-process-Q (process-limit)
                                            (factory (bench-info bench max-config)
                                                     (hash)
                                                     (hash)
                                                     0))])
                ([module-to-mutate-name mutatable-module-names]
                 #:when #t
                 [mutation-index (select-mutants module-to-mutate-name
                                                 bench)])
        (sample-blame-trails-if-type-error process-q
                                           (mutant module-to-mutate-name
                                                   mutation-index
                                                   #t))))

    (log-factory info "Finished enqueing all test mutants. Waiting...")
    (define process-q-finished (process-Q-wait process-q))
    (factory-results (process-Q-get-data process-q-finished))))

;; Spawns a test mutant and if that mutant has a blame result at
;; max contract configuration, then samples the precision lattice
;; and spawns mutants for each samples point
;; Note that sampling the precision lattice is done indirectly by
;; just generating random configs
(define/contract (sample-blame-trails-if-type-error process-q mutant-program)
  ((process-Q/c factory/c) mutant/c . -> . (process-Q/c factory/c))

  (match-define (mutant module-to-mutate-name mutation-index #t) mutant-program)
  (log-factory debug
               "  Trying to spawn test mutant for ~a @ ~a."
               module-to-mutate-name
               mutation-index)
  (define bench (factory-bench (process-Q-get-data process-q)))
  (define max-config (bench-info-max-config bench))
  (define (will:sample-if-type-error current-process-q dead-proc)
    (match (process-outcome dead-proc)
      ['type-error
       (log-factory info
                    "  Mutant ~a @ ~a has type error. Sampling..."
                    module-to-mutate-name
                    mutation-index)
       (sample-blame-trail-roots current-process-q
                                 mutant-program)]
      [else
       (log-factory info
                    "  Mutant ~a @ ~a has no type error; discarding."
                    module-to-mutate-name
                    mutation-index)
       current-process-q]))
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
  ((process-Q/c factory/c) mutant/c . -> . (process-Q/c factory/c))

  (define the-factory (process-Q-get-data process-q))
  (define max-config (bench-info-max-config (factory-bench the-factory)))
  (define samples (sample-config max-config (sample-size)))
  (define (resample a-factory)
    (define sample (first (sample-config max-config 1)))
    (cond
      ;; ll: by default, sample *with* replacement
      [(and (sample-without-replacement?)
            (set-member? (hash-ref (factory-mutant-samples a-factory)
                                   mutant-program)
                         sample))
       (resample a-factory)]
      [else
       (values sample
               (record-sampled-config a-factory mutant-program sample))]))
  (for/fold ([current-process-q process-q])
            ([sampled-config (in-list samples)]
             [sample-number (in-naturals)])
    (define current-factory (process-Q-get-data current-process-q))
    (define factory+sample
      (record-sampled-config current-factory mutant-program sampled-config))
    (log-factory
     debug
     @~a{    Sample: trying to sample root @sample-number for @mutant-program})
    (spawn-blame-trail-root-mutant (process-Q-set-data current-process-q
                                                       factory+sample)
                                   mutant-program
                                   sampled-config
                                   resample
                                   sample-number)))

(define/contract (record-sampled-config the-factory mutant-program new-sample)
  (factory/c mutant/c config/c . -> . factory/c)

  ;; This tracking is only necessary when sampling without replacement
  (cond [(sample-without-replacement?)
         (define mutant-samples (factory-mutant-samples the-factory))
         (define samples-for-mutant (hash-ref mutant-samples mutant-program
                                              (λ _ (set))))
         (define mutant-samples+sample
           (hash-set mutant-samples mutant-program
                     (set-add samples-for-mutant new-sample)))
         (copy-factory the-factory
                       [mutant-samples mutant-samples+sample])]
        [else the-factory]))

;; Spawns a mutant that attempts to follow a blame trail,
;; if the given `config` doesn't cause blame for `mutant-program`
;; then it calls `resample` to get a new configuration and try again.
(define/contract (spawn-blame-trail-root-mutant process-q
                                                mutant-program
                                                config
                                                resample
                                                sample-number)
  ((process-Q/c factory/c)
   mutant/c
   config/c
   (factory/c . -> . (values config/c factory/c))
   natural?
   . -> .
   (process-Q/c factory/c))

  (match-define (mutant module-to-mutate-name mutation-index #t)
    mutant-program)
  (match ((current-result-cache) module-to-mutate-name
                                 mutation-index
                                 sample-number)
    [#f
     (spawn-mutant process-q
                   module-to-mutate-name
                   mutation-index
                   config
                   (make-blame-following-will/fallback
                    (λ (current-process-q dead-proc)
                      ;; Try sampling another config
                      (log-factory
                       debug
                       "    Sample ~a (id [~a]) for ~a @ ~a failed to find blame."
                       sample-number
                       (dead-mutant-process-id dead-proc)
                       (mutant-module (dead-mutant-process-mutant dead-proc))
                       (mutant-index (dead-mutant-process-mutant dead-proc)))
                      (define-values {new-sample new-factory}
                        (resample (process-Q-get-data current-process-q)))
                      (define new-process-q (process-Q-set-data current-process-q
                                                                new-factory))
                      (spawn-blame-trail-root-mutant new-process-q
                                                     mutant-program
                                                     new-sample
                                                     resample
                                                     sample-number)))
                   #:following-trail (blame-trail sample-number
                                                  empty))]
    [path-to-data-file
     (log-factory
      info
      @~a{
          Blame trail @;
          @module-to-mutate-name @"@" @mutation-index {@sample-number} @;
          found in progress cache: @(pretty-path path-to-data-file)
          })
     process-q]))

;; Enqueues a mutant that follows the blame trail starting at `dead-proc`
;; ASSUMPTIONS:
;; - the output of `dead-proc` has a blame label
(define/contract (follow-blame-from-dead-process the-process-q
                                                 dead-proc
                                                 blamed/type-error-locations)
  (->i ([the-process-q                (process-Q/c factory/c)]
        [dead-proc                    dead-mutant-process/c]
        [blamed/type-error-locations  blame-labels?])
       #:pre/desc {dead-proc}
       (match dead-proc
         [(struct* dead-mutant-process
                   ([result (struct* run-status
                                     ([blamed (? (not/c #f))]))]))
          #t]
         [else "`dead-proc` should have a blame label"])
       [result (process-Q/c factory/c)])

  (match-define (dead-mutant-process (mutant mod index #t)
                                     config
                                     result
                                     id
                                     the-blame-trail
                                     _)
    dead-proc)
  (define the-blame-trail+dead-proc
    (extend-blame-trail the-blame-trail
                        dead-proc))
  (define all-blamed-at-max-precision?
    (andmap (λ (blamed) (config-at-max-precision-for? blamed config))
            blamed/type-error-locations))
  (define buggy-mod-type-error?
    (type-error-on-buggy-mod? blamed/type-error-locations result))
  (cond [(or buggy-mod-type-error?
             all-blamed-at-max-precision?)
         (log-factory debug
                      "Blame trail ~a @ ~a {~a} ended."
                      mod
                      index
                      (blame-trail-id the-blame-trail+dead-proc))

         (cond [(and buggy-mod-type-error?
                     (not all-blamed-at-max-precision?))
                ;; This should never happen: something has gone wrong, because
                ;; type errors only blame one location and that location must be
                ;; typed
                (log-factory
                 error
                 @~a{
                     Found a mutant with type error in the buggy module, @;
                     but the module is not typed?
                     Type error location: @blamed/type-error-locations

                     Mutant: @;
                     @mod @"@" @index [@id] {@(blame-trail-id the-blame-trail)}
                     With config:
                     @~v[config]
                     })]
               [(and (not buggy-mod-type-error?)
                     all-blamed-at-max-precision?)
                (log-factory
                 error
                 @~a{
                     BT VIOLATION: @;
                     Found mutant with blamed/type-error location at types @;
                     that is not the buggy module.
                     Blamed: @~v[blamed/type-error-locations]

                     Mutant: @;
                     @mod @"@" @index [@id] {@(blame-trail-id the-blame-trail)}
                     With config:
                     @~v[config]
                     })]
               [else (void)])
         ;; Blamed region is typed, so the path ends here.
         ;; Log the trail and stop following.
         (define new-factory
           (record-blame-trail! (process-Q-get-data the-process-q)
                                the-blame-trail+dead-proc))
         (process-Q-set-data the-process-q
                             new-factory)]
        [else
         (define config/blamed-region-ctc-strength-incremented
           (increment-config-precision-for-all blamed/type-error-locations config))
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
                                              blamed/type-error-locations
                                              spawn-the-blame-following-mutant)))
         (spawn-the-blame-following-mutant the-process-q)]))

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
                                                   blamed
                                                   respawn-mutant)
  (dead-mutant-process/c
   blame-labels?
   ((process-Q/c factory/c)
    #:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)
    . -> .
    (process-Q/c factory/c))
   . -> .
   ((process-Q/c factory/c) dead-mutant-process/c . -> . (process-Q/c factory/c)))

  (match-define (dead-mutant-process (mutant mod index #t)
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
                           _
                           increased-limits?)
      dead-successor)
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
          (factory-bench (process-Q-get-data current-process-q))))
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
       current-process-q]
      [{(or 'completed 'syntax-error) _}
       (log-factory
        error
        "BT VIOLATION: Blame disappeared while following blame trail ~a @ ~a {~a}.
Mutant: [~a] and config:
~v

produced result: ~v
=> ~a

Predecessor (id [~a]) blamed ~a and had config:
~v"
        mod index blame-trail-id
        dead-succ-id
        dead-succ-config
        dead-succ-result
        (if (equal? (run-status-outcome dead-succ-result)
                    'syntax-error)
            "Likely due to a buggy contract
   on the region blamed by the predecessor (see below) that crashed"
            "Something has gone very wrong")
        id blamed
        config)
       (maybe-abort "Blame disappeared" current-process-q)]
      [{(or 'blamed 'type-error) _}
       #:when (and (list? (run-status-blamed dead-succ-result))
                   (andmap library-path? (run-status-blamed dead-succ-result)))
       (log-factory
        error
        @~a{
            BT VIOLATION: @;
            Blame entered library code while following blame trail @;
            @mod @"@" @index {@blame-trail-id}. @;
            Giving up on following the trail.
            Mutant: [@dead-succ-id] and config:
            @~v[dead-succ-config]

            Blamed: @(run-status-blamed result)
            })
       current-process-q])))

(define/contract (make-blame-following-will/fallback no-blame-fallback)
  (mutant-will/c . -> . mutant-will/c)

  (λ (the-process-q dead-proc)
    (cond
      [(or (try-get-blamed-modules dead-proc)
           (try-get-type-error-module dead-proc))
       => (λ (blamed/type-error-locations)
            (log-factory debug
                         "Following blame trail {~a} from [~a] via ~a..."
                         (dead-mutant-process-id dead-proc)
                         (dead-mutant-process-id dead-proc)
                         blamed/type-error-locations)
            (follow-blame-from-dead-process the-process-q
                                            dead-proc
                                            blamed/type-error-locations))]
      [else
       (no-blame-fallback the-process-q dead-proc)])))


(define/contract (spawn-mutant process-q
                               module-to-mutate-name
                               mutation-index
                               precision-config
                               mutant-will
                               [revival-count 0]
                               #:timeout/s [timeout/s #f]
                               #:memory/gb [memory/gb #f]
                               #:following-trail [trail-being-followed #f]
                               #:test-mutant? [test-mutant? #f])
  (->i ([process-q              (process-Q/c factory/c)]
        [module-to-mutate-name  module-name?]
        [mutation-index         natural?]
        [precision-config       config/c]
        [mutant-will            mutant-will/c])
       ([revival-count natural?]
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
       [result (process-Q/c factory/c)])

  (define current-factory (process-Q-get-data process-q))
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
    (define mutant-blame-trail
      (cond [trail-being-followed => values]
            [test-mutant?            (blame-trail test-mutant-flag
                                                  '())]))
    (define mutant-proc
      (mutant-process (mutant module-to-mutate-name mutation-index #t)
                      precision-config
                      outfile
                      mutant-id
                      mutant-blame-trail
                      revival-count
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
   @~a{    Mutant [@mutant-id] has config @~v[precision-config]})

  (define number-of-procs-this-mutant-may-spawn
    (if test-mutant? (sample-size) 1))
  (process-Q-enq
   (process-Q-set-data process-q
                       (copy-factory current-factory
                                     [total-mutants-spawned
                                      (add1 mutants-spawned)]))
   spawn-the-mutant
   number-of-procs-this-mutant-may-spawn))

;; There is some common housekeeping that must be performed in every mutant
;; will, regardless of what kind of mutant or the details of its particular will
;; details.
;; In particular:
;; - Respawning the mutant a limited number of times if the mutant fails
;; - Otherwise, converting the mutant-process into a dead-mutant-process
;; - Logging the result of the mutant (and updating the factory with it)
(define/contract (mutant->process-will mutant-will)
  (mutant-will/c . -> . process-will/c)

  (define (outer-will process-q the-process-info)
    (match-define (and mutant-proc
                       (mutant-process (mutant mod index #t)
                                       config
                                       file
                                       id the-blame-trail
                                       revival-count
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
          'didnt-check-due-to-error))
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
                        config: @~s[config]
                        })
       (define dead-mutant-proc
         (dead-mutant-process (mutant mod index #t)
                              config
                              result
                              id
                              the-blame-trail
                              increased-limits?))
       (delete-file file)
       (mutant-will process-q dead-mutant-proc)]))
  outer-will)

(define/contract (maybe-revive-failed-mutant process-q
                                             a-mutant-process
                                             status
                                             maybe-result
                                             mutant-will)
  (->i ([process-q         (process-Q/c factory/c)]
        [a-mutant-process  mutant-process/c]
        [status            (or/c 'done-ok 'done-error)]
        [maybe-result      {status}
                           (match status
                             ['done-ok eof-object?]
                             ['done-error any/c])]
        [mutant-will       mutant-will/c])
       [result (process-Q/c factory/c)])

  (match-define (struct* mutant-process
                         ([id             id]
                          [blame-trail    the-blame-trail]
                          [mutant         (mutant mod index #t)]
                          [config         config]
                          [revival-count  revival-count]))
    a-mutant-process)

  (cond [(>= revival-count MAX-REVIVALS)
         (log-factory error
                      "Runner errored all ~a / ~a tries on mutant:
 [~a] ~a @ ~a with config
~v"
                      revival-count MAX-REVIVALS
                      id mod index
                      config)
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
                      config
                      status maybe-result
                      (add1 revival-count) MAX-REVIVALS)
         (spawn-mutant process-q
                       mod
                       index
                       config
                       mutant-will
                       (add1 revival-count)
                       #:following-trail (match the-blame-trail
                                           [(? blame-trail? bt) bt]
                                           [else                #f])
                       #:test-mutant? (equal? the-blame-trail
                                              test-mutant-flag))]))

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
                     (mutant module-to-mutate-name mutation-index #t))
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
                                ([mutant (mutant mod index #t)]))
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
                 (mutant-summary id result config)]))


(define/contract (read-mutant-result mutant-proc)
  (mutant-process/c . -> . (or/c run-status? eof-object?))

  (define path (mutant-process-file mutant-proc))
  (define (report-malformed-output . _)
    (match-define (mutant-process (mutant mod index _) config _ id _ _ _)
      mutant-proc)
    (log-factory warning
                 "Result read from mutant output not of the expected shape.
Expected: a run-status with a valid pair of outcome/blamed
Found: ~v
If this has the right shape, it may contain an unreadable value.

Mutant: [~a] ~a @ ~a with config:
~v
"
                 (file->string path)
                 id mod index
                 config)
    eof)
  (with-handlers ([exn:fail:read? report-malformed-output])
    (match (with-input-from-file path read)
      [(and (or (struct* run-status
                         ([outcome (or 'completed
                                       'syntax-error
                                       'timeout
                                       'oom
                                       'runtime-error)]
                          [blamed #f]))
                (struct* run-status
                         ([outcome (or 'blamed
                                       'type-error
                                       'runtime-error)]
                          [blamed (not #f)])))
            result/well-formed)
       result/well-formed]
      [else (report-malformed-output)])))

;; dead-mutant-process?
;; ->
;; (or/c 'blamed 'type-error 'completed 'syntax-error 'timeout 'oom)
(define (process-outcome dead-proc)
  (run-status-outcome (dead-mutant-process-result dead-proc)))

;; result/c -> module-name?
(define/match (try-get-blamed-modules/from-result result)
  [{(struct* run-status ([outcome 'blamed]
                         [blamed (and (list-no-order (? module-name?) _ ...)
                                      blamed-list)]))}
   (filter module-name? blamed-list)]
  [{(struct* run-status ([outcome 'runtime-error]
                         [blamed (and blamed
                                      (not #f))]))}
   blamed]
  [{(struct* run-status ([outcome _]))}
   #f])

;; dead-mutant-process? -> module-name?
(define/match (try-get-blamed-modules dead-proc)
  [{(dead-mutant-process _ _ result _ _ _)}
   (try-get-blamed-modules/from-result result)])

(define/contract (type-error-on-buggy-mod? blamed-mod-names result)
  ((listof module-name?) run-status/c . -> . boolean?)

  (match result
    [(struct* run-status ([outcome 'type-error]
                          [blamed (== blamed-mod-names)]))
     #t]
    [_ #f]))

(define/match (try-get-type-error-module/from-result result)
  [{(struct* run-status ([outcome 'type-error]
                         [blamed blamed]))}
   blamed]
  [{(struct* run-status ([outcome (not 'type-error)]))}
   #f])
(define/match (try-get-type-error-module dead-proc)
  [{(struct* dead-mutant-process
             ([result (? run-status? result)]))}
   (try-get-type-error-module/from-result result)]
  [{_} #f])

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
         continue-val]))

(define (pretty-path p)
  (path->string (find-relative-path (simple-form-path (current-directory))
                                    (simple-form-path p))))

(module+ main
  (require racket/cmdline)
  (define bench-to-run (make-parameter #f))
  (define progress-log (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    path
    "Path to benchmark to run."
    (bench-to-run path)]
   [("-c" "--config")
    path
    "Path to the configuration to use."
    (current-configuration-path path)]
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
    (progress-log path)])

  (unless (bench-to-run)
    (raise-user-error 'mutant-factory "Error: must provide benchmark to run."))
  (unless (current-configuration-path)
    (raise-user-error 'mutant-factory "Error: must provide a configuration."))

  (when (and (directory-exists? (data-output-dir))
             (not (progress-log)))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))

  (define progress-info
    (match (progress-log)
      [(? file-exists? path) (make-hash (file->list path))]
      [else (hash)]))
  (define-values {log-progress!/raw finalize-log!}
    (initialize-progress-log! (progress-log)
                              #:exists 'append))
  (define (log-progress! module-to-mutate-name
                         mutation-index
                         sample-number

                         data-file)
    (log-progress!/raw (cons (list module-to-mutate-name
                                   mutation-index
                                   sample-number)
                             ;; ensure it's an absolute path in case we resume
                             ;; from another directory
                             (path->string
                              (simple-form-path data-file)))))
  (define (cached-results-for module-to-mutate-name
                              mutation-index
                              sample-number)
    (hash-ref progress-info
              (list module-to-mutate-name
                    mutation-index
                    sample-number)
              #f))
  (parameterize ([date-display-format 'iso-8601])
    (log-factory info @~a{Running on benchmark @(bench-to-run)})
    (run-all-mutants*configs (read-benchmark (bench-to-run))
                             #:log-progress log-progress!
                             #:resume-cache cached-results-for))
  (finalize-log!))
