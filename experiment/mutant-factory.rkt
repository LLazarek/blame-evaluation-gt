#lang at-exp racket

;; lltodo:
;; - remove outdate signature comments

(require "../runner/mutation-runner.rkt"
         "../runner/instrumented-runner.rkt"
         "../mutate/mutate.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "program.rkt"
         racket/file
         racket/format
         racket/match
         racket/system
         racket/serialize
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
           (struct-out aggregate-mutant-result)
           (struct-out bench-info)
           (struct-out factory)
           process-limit
           data-output-dir
           benchmarks-dir-path
           factory-logger
           abort-on-failure?
           default-memory-limit/gb
           default-timeout/s

           run-all-mutants*configs
           spawn-mutants/of-module
           maybe-spawn-configured-mutants
           spawn-mutants/precision-sampling
           add-mutant-sample
           spawn-mutant/following-blame
           make-blame-disappearing-handler
           spawn-mutant
           sweep-dead-mutants
           process-dead-mutant
           add-mutant-result
           max-mutation-index-exceeded?
           append-mutant-result!
           read-mutant-result
           process-outcome
           try-get-blamed
           config-at-max-precision-for?
           increment-config-precision-for
           make-max-bench-config
           sample-size))


(define MAX-CONFIG 'types)
(define MAX-REVIVALS 3)


(define-runtime-path mutant-runner-path "mutant-runner.rkt")
(define-runtime-path benchmarks-dir-path "../../gtp-benchmarks/benchmarks/")
(define racket-path (find-executable-path (find-system-path 'exec-file)))

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define mutant-error-log (make-parameter "./mutant-errors.txt"))
(define sample-size (make-parameter 96))
(define abort-on-failure? (make-parameter #t))
(define default-memory-limit/gb (make-parameter 3))
(define default-timeout/s (make-parameter (* 5 60)))


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
                  v ...))))
(define (failure-msg failure-type m)
  (string-append "***** " (~a failure-type) " *****\n" m "\n**********"))

(define natural? exact-nonnegative-integer?)

;; see last result of `process`
(define process-ctl? procedure?)
(define (module-name? s)
  (and (string? s)
       (not (regexp-match? @regexp{[\/]} s))))

(define/contract (spawn-mutant-runner a-benchmark-configuration
                                      module-to-mutate-name
                                      mutation-index
                                      outfile
                                      #:timeout/s [timeout/s #f]
                                      #:memory/gb [memory/gb #f])
  ({benchmark-configuration/c
   module-name?
   natural?
   path-string?}
   {#:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)}
   . ->* .
   process-ctl?)

  (match-define (benchmark-configuration main others* base-dir)
    a-benchmark-configuration)
  (define module-to-mutate
    (resolve-configured-benchmark-module a-benchmark-configuration
                                         module-to-mutate-name))
  (define others (append others*
                         (directory-list base-dir #:build? #t)))
  (call-with-output-file outfile #:mode 'text
    (λ (outfile-port)
      (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
        (λ (error-log-port)
          (match-define (list #f runner-in _ #f runner-ctl)
            (process*/ports
             outfile-port #f error-log-port
             racket-path "--"
             mutant-runner-path
             "-m" main
             "-o" (~s others)
             "-M" module-to-mutate
             "-i" (~a mutation-index)
             "-t" (~a (if timeout/s
                          timeout/s
                          (default-timeout/s)))
             "-g" (~a (if memory/gb memory/gb (default-memory-limit/gb)))))
          (close-output-port runner-in)
          runner-ctl)))))


;; module:    path-string?
;; index:     natural?
;; abstract?: boolean? ; is module a `module-name?` rather than a resolved path?
(struct mutant (module index abstract?) #:transparent)

;; will?           := (factory? dead-mutant-process? -> factory?)
;; result?         := run-status? (see `mutation-runner.rkt`)
;; ctc-level?      := symbol?
;; config?         := (hash path-string?
;;                         (hash (or symbol? path-string?) ctc-level?))
;; blame-trail-id? := (or natural? 'no-blame)

;; mutant:         mutant?
;; config:         config?
;; file:           path-string?
;; ctl:            process-ctl-fn? (see `process` docs)
;; will:           will?
;;     A transformation of the factory to be performed upon death of the mutant
;; id:             natural?
;; blame-trail-id: blame-trail-id?
;; revival-count:  natural?
(struct mutant-process (mutant
                        config
                        file   ctl
                        will
                        id     blame-trail-id
                        revival-count
                        increased-limits?)
  #:transparent)

;; result: result?
(struct dead-mutant-process
  (mutant config result id blame-trail-id increased-limits?)
  #:transparent)
(struct aggregate-mutant-result (mutant file) #:transparent)

(define mutant-results? (hash/c mutant? aggregate-mutant-result?))

;; benchmark:  benchmark/c
;; max-config: config?
(struct bench-info (benchmark max-config) #:transparent)

;; bench: bench-info?
;; results: mutant-results?
;;     The map from mutant to completed processes (which contain a data file).
;; active-mutants: (set mutant-process?)
;;     The set of actively running mutant processes.
;; active-mutant-count: natural?
;;     The size of `active-mutants`.
;;     INVARIANT: (= active-mutant-count (set-count active-mutants))
;; mutant-samples: mutant? |-> (set config?)
;;     The set of precision config samples checked for each mutant.
;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench
                 results
                 active-mutants
                 active-mutant-count
                 mutant-samples
                 total-mutants-spawned)
  #:transparent)


(define-simple-macro (copy-factory
                      a-factory:expr field-val-pair:expr ...)
  (struct-copy factory a-factory field-val-pair ...))


(define blame-label? module-name?)
(define mutant/c
  (struct/dc mutant
             [module (abstract?) (if abstract?
                                     module-name?
                                     path-to-existant-file?)]
             [index natural?]
             [abstract? boolean?]))
(define will/c (factory? dead-mutant-process? . -> . factory?))
(define result? run-status?)
(define blame-trail-id? (or/c natural? 'no-blame))
(define mutant-process/c
  (struct/dc mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [file               path-to-existant-file?]
             [ctl                process-ctl?]
             [will               will/c]
             [blame-trail-id     natural?]
             [revival-count      blame-trail-id?]
             [increased-limits?  natural?]))
(define dead-mutant-process/c
  (struct/dc dead-mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [result             result?]
             [id                 natural?]
             [blame-trail-id     blame-trail-id?]
             [increased-limits?  boolean?]))
(define aggregate-mutant-result/c
  (struct/dc aggregate-mutant-result
             [mutant  mutant/c]
             [file    path-to-existant-file?]))
(define bench-info/c
  (struct/dc bench-info
             [benchmark   benchmark/c]
             [max-config  config/c]))
(define factory/c
  (struct/dc factory
             [bench                   bench-info/c]
             [results                 mutant-results?]
             [active-mutants (set/c   mutant-process/c)]
             [active-mutant-count     {active-mutants}
                                      (and/c natural?
                                             (=/c (set-count active-mutants)))]
             [mutant-samples (hash/c  mutant/c (set/c config/c))]
             [total-mutants-spawned   natural?]))





;; Main entry point of the factory
(define/contract (run-all-mutants*configs bench)
  (benchmark/c . -> . mutant-results?)

  (define mutatable-module-names (benchmark->mutatable-modules bench))
  (define max-config (make-max-bench-config bench))
  (log-factory info "Benchmark has modules:~n~a"
                    mutatable-module-names)

  (unless (directory-exists? (data-output-dir))
    (log-factory debug "Creating output directory ~a." (data-output-dir))
    (make-directory (data-output-dir)))

  (define factory-state
    (for/fold ([factory-state (factory (bench-info bench max-config)
                                       (hash) (set) 0 (hash) 0)])
              ([module-to-mutate-name mutatable-module-names])

      (log-factory info "Processing mutants of module ~a."
                        module-to-mutate-name)
      (spawn-mutants/of-module factory-state module-to-mutate-name)))

  (log-factory info "Finished spawning all mutant runners.")
  (babysit-mutants factory-state))

(define (resolve-any-benchmark-module-path-for a-benchmark a-module-name)
  (match a-benchmark
    [(struct* benchmark
              ([typed (list-no-order (? (path-ends-with a-module-name) path)
                                     _ ...)]))
     path]
    [else
     (error 'mutant-factory
            @~a{Unable to find module @a-module-name in @a-benchmark})]))

(define (resolve-configured-benchmark-module a-benchmark-configuration a-module-name)
  (findf (path-ends-with a-module-name)
         (list* (benchmark-configuration-main a-benchmark-configuration)
                (benchmark-configuration-others a-benchmark-configuration))))

;; factory?
;; string?
;; ->
;; factory?
(define/contract (spawn-mutants/of-module the-factory module-to-mutate-name)
  (factory/c module-name? . -> . factory/c)

  (define module-to-mutate-path
    (resolve-any-benchmark-module-path-for (factory-bench the-factory)
                                           module-to-mutate-name))
  ;; This must be an unbounded loop, number of mutants unknown
  (let next-mutant ([mutation-index 0]
                    [current-factory the-factory])
    (cond [(max-mutation-index-exceeded? module-to-mutate-path mutation-index)
           current-factory]

          [else
           (next-mutant
            (add1 mutation-index)
            (maybe-spawn-configured-mutants current-factory
                                            (mutant module-to-mutate-name
                                                    mutation-index
                                                    #t)))])))

;; factory? mutant? -> factory?
;; Spawns a test mutant and if that mutant has a blame result at
;; max contract configuration, then samples the precision lattice
;; and spawns mutants for each samples point
;; Note that sampling the precision lattice is done indirectly by
;; just generating random configs
(define/contract (maybe-spawn-configured-mutants the-factory mutant-program
                                                 #:increased-limits?
                                                 [increased-limits? #f])
  ({factory/c
    mutant/c}
   {#:increased-limits? boolean?}
   . ->* .
   factory/c)

  (match-define (mutant module-to-mutate-name mutation-index #t) mutant-program)
  (log-factory debug
               "  Trying to spawn test mutant for ~a @ ~a."
               module-to-mutate-name
               mutation-index)
  (define bench (factory-bench the-factory))
  (define max-config (bench-info-max-config bench))
  (define-values {timeout* memory*}
    (if increased-limits? (increased-limits bench) (values #f #f)))
  (spawn-mutant the-factory
                module-to-mutate-name
                mutation-index
                max-config
                ;; mutant will
                (λ (current-factory dead-proc)
                  (match (process-outcome dead-proc)
                    ;; lltodo: actually detect and produce type-error outcomes
                    ['type-error
                     (log-factory info
                                  "  Mutant ~a @ ~a has type error. Sampling..."
                                  module-to-mutate-name
                                  mutation-index)
                     (spawn-mutants/precision-sampling current-factory
                                                       mutant-program)]
                    [(or 'timeout 'oom)
                     #:when (not increased-limits?)
                     (log-factory
                      info
                      "  Mutant ~a @ ~a exceeds limits; retrying increased."
                      module-to-mutate-name
                      mutation-index)
                     (maybe-spawn-configured-mutants current-factory
                                                     mutant-program
                                                     #:increased-limits? #t)]
                    [(or 'timeout 'oom)
                     #:when increased-limits?
                     (log-factory
                      warning
                      "  Unable to make ~a @ ~a terminate normally. Giving up."
                      module-to-mutate-name
                      mutation-index)
                     current-factory]
                    [else
                     (log-factory info
                                  "  Mutant ~a @ ~a has no type error; discarding."
                                  module-to-mutate-name
                                  mutation-index)
                     current-factory]))
                #:timeout/s timeout*
                #:memory/gb memory*))

;; path-string? mutant? -> factory?
(define/contract (spawn-mutants/precision-sampling the-factory mutant-program)
  (factory/c mutant/c . -> . factory/c)

  (define max-config (bench-info-max-config (factory-bench the-factory)))
  (define samples (sample-config max-config (sample-size)))
  (define (resample a-factory)
    (define sample (set-first (sample-config max-config 1)))
    (define samples-seen (hash-ref (factory-mutant-samples a-factory)
                                   mutant-program))
    (cond
      ;; ll: We're sampling *with* replacement; uncomment for *without*
      #;[(set-member? samples-seen sample)
         (resample a-factory)]
      [else
       (values sample
               (add-mutant-sample a-factory mutant-program sample))]))
  (define new-factory
    (for/fold ([current-factory the-factory])
              ([sampled-config (in-set samples)])
      (define factory+sample
        (add-mutant-sample current-factory mutant-program sampled-config))
      (spawn-blame-trail-root-mutant factory+sample
                                     mutant-program
                                     sampled-config
                                     resample)))
  (log-factory info
               "    Completed sampling for mutant ~a @ ~a."
               (mutant-module mutant-program)
               (mutant-index mutant-program))
  new-factory)

(define/contract (add-mutant-sample the-factory mutant-program new-sample)
  (factory/c mutant/c config/c . -> . factory/c)

  (define mutant-samples (factory-mutant-samples the-factory))
  (define samples-for-mutant (hash-ref mutant-samples mutant-program
                                       (λ _ (set))))
  (define mutant-samples+sample
    (hash-set mutant-samples mutant-program
              (set-add samples-for-mutant new-sample)))
  (copy-factory the-factory
                [mutant-samples mutant-samples+sample]))

;; factory?
;; mutant?
;; config?
;; (factory? -> (values config? factory?))
;; ->
;; factory?
;;
;; Spawns a mutant that attempts to follow a blame trail,
;; if the given `config` doesn't cause blame for `mutant-program`
;; then it calls `resample` to get a new configuration and try again.
(define/contract (spawn-blame-trail-root-mutant the-factory
                                                mutant-program
                                                config
                                                resample)
  (factory/c
   mutant/c
   config/c
   (factory/c . -> . (values config/c factory/c))
   . -> .
   factory/c)

  (log-factory debug
               "    Sample: trying to spawn blame-following mutant")
  (spawn-mutant the-factory
                (mutant-module mutant-program)
                (mutant-index mutant-program)
                config
                (make-blame-following-will/with-fallback
                 (λ (current-factory dead-proc)
                   ;; Try sampling another config
                   (log-factory
                    debug
                    "    Sample [~a] for ~a @ ~a failed to find blame."
                    (dead-mutant-process-id dead-proc)
                    (mutant-module (dead-mutant-process-mutant dead-proc))
                    (mutant-index (dead-mutant-process-mutant dead-proc)))
                   (define-values {new-sample new-factory}
                     (resample current-factory))
                   (spawn-blame-trail-root-mutant new-factory
                                                  mutant-program
                                                  new-sample
                                                  resample)))))

;; factory? dead-mutant-process? blame-trail-id? -> factory?
;;
;; ASSUMPTIONS:
;; - the output of `dead-proc` has a blame label
;;
;; Spawns a mutant that follows the blame trail starting at `dead-proc`
(define/contract (spawn-mutant/following-blame the-factory
                                               dead-proc
                                               blamed/type-error-location)
  (->i ([the-factory factory/c]
        [dead-proc dead-mutant-process/c]
        [blamed/type-error-location blame-label?])
       #:pre/desc {dead-proc}
       (match dead-proc
         [(struct* dead-mutant-process
                   ([result (struct* run-status
                                     ([blamed (? (not/c #f))]))]))
          #t]
         [else "`dead-proc` should have a blame label"])
       [result factory/c])

  (match-define (dead-mutant-process (mutant mod index #t)
                                     config
                                     result
                                     id
                                     blame-trail-id
                                     _)
    dead-proc)
  (cond [(config-at-max-precision-for? blamed/type-error-location config)
         (log-factory debug
                      "Blame trail {~a} ended."
                      blame-trail-id)
         (unless (blamed-is-bug? blamed/type-error-location result)
           (log-factory
            error
            "Found mutant with blamed/type-error (~v) at types that is not bug:
~v
"
            blamed/type-error-location
            dead-proc))
         ;; Blamed region is typed, so the path ends here
         the-factory]
        [else
         (when (try-get-type-error-module dead-proc)
           (log-factory
            warning
            @~a{
                Found type error in module that isn't typed?
                Module: @blamed/type-error-location
                Config: @config
                }))

         (define config/blamed-region-ctc-strength-incremented
           (increment-config-precision-for blamed/type-error-location config))
         (define (spawn-the-blame-following-mutant a-factory
                                                   #:timeout/s [timeout/s #f]
                                                   #:memory/gb [memory/gb #f])
           (spawn-mutant a-factory
                         mod
                         index
                         config/blamed-region-ctc-strength-incremented
                         will:keep-following-blame
                         blame-trail-id
                         #:timeout/s timeout/s
                         #:memory/gb memory/gb))
         (define will:keep-following-blame
           (make-blame-following-will/with-fallback
            (make-blame-disappearing-handler dead-proc
                                             blamed/type-error-location
                                             spawn-the-blame-following-mutant)))
         (spawn-the-blame-following-mutant the-factory)]))

(define (increased-limits bench)
  (values (* 2 (default-timeout/s))
          (* 2 (default-memory-limit/gb))))

;; dead-mutant-process?
;; blamed?
;; (factory? #:timeout/s (or #f number?) #:memory/gb (or #f number?))
;; ->
;; (factory? dead-mutant-process? -> factory?)
(define/contract (make-blame-disappearing-handler dead-proc
                                                  blamed
                                                  respawn-mutant)
  (dead-mutant-process/c
   blame-label?
   (factory/c
    #:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)
    . -> .
    factory/c)
   . -> .
   (factory/c dead-mutant-process/c . -> . factory/c))

  (match-define (dead-mutant-process (mutant mod index #t)
                                     config
                                     result
                                     id
                                     blame-trail-id
                                     _)
    dead-proc)
  (λ (current-factory dead-successor)
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
         (increased-limits (factory-bench current-factory)))
       (respawn-mutant current-factory
                       #:timeout/s timeout*
                       #:memory/gb memory*)]
      [{(and outcome (or 'timeout 'oom)) #t}
       (log-factory warning
                    "
Unable to continue following blame trail {~a}.
Mutant: ~a @ ~a with id [~a] exceeded limits with: ~v
Giving up.
"
                    blame-trail-id
                    mod index dead-succ-id outcome)
       current-factory]
      [{(or 'completed 'crashed) _}
       (log-factory fatal
                    "Blame disappeared while following blame trail {~a}.
Mutant: ~a @ ~a with id [~a] and config:
~v

produced result: ~v
=> ~a

Predecessor (id [~a]) blamed ~a and had config:
~v"
                    blame-trail-id
                    mod index dead-succ-id
                    dead-succ-config
                    dead-succ-result
                    (if (equal? (run-status-outcome dead-succ-result)
                                'crashed)
                        "Likely due to a buggy contract
   on the region blamed by the predecessor (see below) that crashed"
                        "Something has gone very wrong")
                    id blamed
                    config)
       (maybe-abort "Blame disappeared" current-factory)])))

;; blame-trail-id?
;; (factory? dead-mutant-process? -> factory?)
;; ->
;; (factory? dead-mutant-process? -> factory?)
(define/contract (make-blame-following-will/with-fallback no-blame-fallback)
  ((factory/c dead-mutant-process/c . -> . factory/c)
   . -> .
   (factory/c dead-mutant-process/c . -> . factory/c))

  (λ (the-factory dead-proc)
    (cond
      [(or (try-get-blamed dead-proc)
           (try-get-type-error-module dead-proc))
       => (λ (blamed/type-error-location)
            (log-factory debug
                         "Following blame trail {~a} from [~a] via ~a..."
                         (dead-mutant-process-id dead-proc)
                         (dead-mutant-process-id dead-proc)
                         blamed/type-error-location)
            (spawn-mutant/following-blame the-factory
                                          dead-proc
                                          blamed/type-error-location))]
      [else
       (no-blame-fallback the-factory dead-proc)])))



(define/contract (spawn-mutant the-factory
                               module-to-mutate-name
                               mutation-index
                               precision-config
                               mutant-will
                               [blame-trail-id 'no-blame]
                               [revival-count 0]
                               #:timeout/s [timeout/s #f]
                               #:memory/gb [memory/gb #f])
  ({factory/c
    module-name?
    natural?
    config/c
    will/c}
   {blame-trail-id?
    natural?
    #:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)}
   . ->* .
   factory/c)

  (define idle-retry-limit
    ;; Wait 2 secs per try, so halve the limit
    (quotient (* 3 (default-timeout/s)) 2))
  (let try-again ([current-factory the-factory]
                  [idle-retry-count 0])
    (define active-mutant-count (factory-active-mutant-count current-factory))
    (cond [(>= active-mutant-count (process-limit))
           (log-factory
            debug
            "    Mutants (~a) at process limit (~a). (idle-retry: ~a)"
            active-mutant-count
            (process-limit)
            idle-retry-count)
           (when (>= idle-retry-count idle-retry-limit)
             (log-factory
              error
              "Mutants at limit but unable to sweep any after ~a minutes.
There are likely zombie mutants about.
Active mutant set (ids):
~v
"
              (/ idle-retry-limit 30)
              (set-map (factory-active-mutants current-factory)
                       mutant-process-id)))
           (sleep 2)
           (define sweeped (sweep-dead-mutants current-factory))
           (try-again sweeped
                      ;; Sweeping may cause a new mutant to be
                      ;; spawned, in which case we're no longer idle
                      (if (= (factory-total-mutants-spawned current-factory)
                             (factory-total-mutants-spawned sweeped))
                          ;; Also wrap the counter around so that
                          ;; idling doesn't cause above message to be
                          ;; logged every 2 sec
                          (modulo (add1 idle-retry-count) (add1 idle-retry-limit))
                          0))]
          [else
           (match-define (factory (bench-info the-benchmark _)
                                  _
                                  active-mutants
                                  active-mutant-count
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
           (define mutant-ctl (spawn-mutant-runner the-benchmark-configuration
                                                   module-to-mutate-name
                                                   mutation-index
                                                   outfile
                                                   #:timeout/s timeout/s
                                                   #:memory/gb memory/gb))
           (define mutant-proc
             (mutant-process (mutant module-to-mutate-name mutation-index)
                             precision-config
                             outfile
                             mutant-ctl
                             mutant-will
                             mutants-spawned
                             blame-trail-id
                             revival-count
                             ;; coerce to bool
                             (and (or timeout/s memory/gb) #t)))

           (log-factory
            info
            "    Spawned mutant runner with id [~a] for ~a @ ~a > ~a."
            mutants-spawned
            module-to-mutate-name
            mutation-index
            outfile)

           (copy-factory current-factory
                         [active-mutants (set-add active-mutants mutant-proc)]
                         [active-mutant-count (add1 active-mutant-count)]
                         [total-mutants-spawned (add1 mutants-spawned)])])))



;; factory? -> factory?
(define/contract (babysit-mutants the-factory)
  (factory/c . -> . factory/c)

  (let check-again ([current-factory the-factory])
    (define sweeped-factory (sweep-dead-mutants current-factory))
    (cond [(zero? (factory-active-mutant-count sweeped-factory))
           (log-factory info "Babysitting complete. All mutants are dead.")
           current-factory]
          [else
           (sleep 2)
           (check-again sweeped-factory)])))


;; factory? -> factory?
(define/contract (sweep-dead-mutants the-factory)
  (factory/c . -> . factory/c)

  (log-factory debug "      Checking active mutant set for dead mutants...")
  ;; ll: ugly way to filter a set, there's no set-filter
  (define a-freshly-dead-mutant
    (for/first ([mutant-proc (in-set (factory-active-mutants the-factory))]
                #:unless (equal? ((mutant-process-ctl mutant-proc) 'status)
                                 'running))
      mutant-proc))
  (if a-freshly-dead-mutant
      (process-dead-mutant the-factory a-freshly-dead-mutant)
      the-factory))

;; factory? mutant-process? -> factory?
;; Note that processing a dead mutant may cause new mutants to be spawned,
;; since it executes the will of the dead mutant.,
;; which, in turn, may cause more dead mutants to be processed!
(define/contract (process-dead-mutant the-factory mutant-proc)
  (factory/c mutant-process/c . -> . factory/c)

  (match-define (mutant-process (mutant mod index #t)
                                config
                                file ctl will
                                id orig-blame-trail
                                revival-count
                                increased-limits?)
    mutant-proc)
  (match-define (factory _ results active-mutants active-count _ _) the-factory)

  ;; Read the result of the mutant before possible consolidation
  (define status (ctl 'status))
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
     #:when (>= revival-count MAX-REVIVALS)
     (log-factory fatal
                  "Runner errored all ~a / ~a tries on mutant:
 [~a] ~a @ ~a with config
~v"
                  revival-count MAX-REVIVALS
                  id mod index
                  config)
     (maybe-abort "Revival failed to resolve mutant errors"
                  (copy-factory the-factory
                                [active-mutants (set-remove active-mutants
                                                            mutant-proc)]
                                [active-mutant-count (sub1 active-count)]))]
    [(or (cons 'done-error _)
         (cons 'done-ok (? eof-object?)))
     (log-factory warning
                  "Runner errored on mutant [~a] ~a @ ~a with config
~v

Exited with ~a and produced result: ~v

Attempting revival ~a / ~a
"
                  id mod index
                  config
                  (ctl 'status) maybe-result
                  (add1 revival-count) MAX-REVIVALS)
     (define new-factory
       (copy-factory the-factory
                     [active-mutants (set-remove active-mutants
                                                 mutant-proc)]
                     [active-mutant-count (sub1 active-count)]))
     (spawn-mutant new-factory
                   mod
                   index
                   config
                   will
                   orig-blame-trail
                   (add1 revival-count))]
    [(cons 'done-ok result)
     (log-factory info
                  "      Sweeping up dead mutant [~a]: ~a @ ~a, config ~a."
                  id mod index config)

     (define blame-trail-id
       (if (and (equal? orig-blame-trail 'no-blame)
                (try-get-blamed/from-result result))
           ;; This mutant wasn't following a pre-existing blame trail,
           ;; but it found blame, so it is the start of a fresh blame trail
           id
           orig-blame-trail))
     (log-factory info
                  "      Dead mutant [~a] result: ~v"
                  id
                  (run-status-outcome result))
     (define dead-mutant-proc
       (dead-mutant-process (mutant mod index #t)
                            config
                            result
                            id
                            blame-trail-id
                            increased-limits?))
     ;; Do the consolidation
     (define results+dead-mutant (add-mutant-result results
                                                    dead-mutant-proc
                                                    file))

     (define new-factory
       (copy-factory the-factory
                     [results results+dead-mutant]
                     [active-mutants (set-remove active-mutants
                                                 mutant-proc)]
                     [active-mutant-count (sub1 active-count)]))

     (will new-factory dead-mutant-proc)]))

;; mutant-results? dead-mutant-process? path-string? -> mutant-results?
(define/contract (add-mutant-result mutant-results
                                    dead-mutant-proc
                                    mutant-proc-file)
  (mutant-results?
   dead-mutant-process/c
   path-to-existant-file?
   . -> .
   mutant-results?)

  (define mutant (dead-mutant-process-mutant dead-mutant-proc))
  (cond [(hash-has-key? mutant-results mutant)
         (define aggregate-results (hash-ref mutant-results mutant))
         (append-mutant-result!
          (dead-mutant-process-blame-trail-id dead-mutant-proc)
          (dead-mutant-process-result dead-mutant-proc)
          aggregate-results)
         (delete-file mutant-proc-file)
         mutant-results]
        [else
         ;; First process for this mutant to die, so its file
         ;; becomes the aggregate data file
         (define aggregate (aggregate-mutant-result mutant mutant-proc-file))
         (create-aggregate-result-file! dead-mutant-proc aggregate)
         (hash-set mutant-results
                   mutant
                   aggregate)]))

(define/contract (create-aggregate-result-file! dead-mutant-proc aggregate-result)
  (dead-mutant-process/c aggregate-mutant-result/c . -> . any)

  (match-define (dead-mutant-process _ _ result _ blame-trail-id _)
    dead-mutant-proc)
  (define file (aggregate-mutant-result-file aggregate-result))
  (call-with-output-file file
    #:mode 'text
    #:exists 'replace
    (λ (out) (writeln (serialize (cons blame-trail-id result)) out))))

;; path-string? natural? -> boolean?
(define/contract (max-mutation-index-exceeded? module-to-mutate mutation-index)
  (path-to-existant-file?
   natural?
   . -> .
   boolean?)

  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (mutate-module (read-module module-to-mutate)
                   mutation-index)
    #f))

;; blame-trail-id? result? aggregate-mutant-result? -> void
(define/contract (append-mutant-result! blame-trail-id result aggregate-results)
  (blame-trail-id? result? aggregate-mutant-result/c . -> . any)

  (define aggregate-file (aggregate-mutant-result-file aggregate-results))
  (with-output-to-file aggregate-file
    #:exists 'append
    #:mode 'text
    (λ _ (writeln (serialize (cons blame-trail-id result))))))

(define/contract (read-mutant-result mutant-proc)
  (mutant-process/c . -> . (or/c run-status? eof-object?))

  (define path (mutant-process-file mutant-proc))
  (define (report-malformed-output . _)
    (match-define (mutant-process (mutant mod index _) config _ _ _ id _ _ _)
      mutant-proc)
    (log-factory warning
                 "Result read from mutant output not of the expected shape.
Expected: a run-status
Found: ~v
If this has the right shape, it may contain an unreadable value.

Mutant: [~a] ~a @ ~a with config:
~v
"
                 (file->string path)
                 id mod index
                 config)
    eof)
  ;; Unfortunately, `deserialize` doesn't check the validity of its
  ;; input before doing its thing, so the exceptions that might come
  ;; from deserializing the wrong value are arbitrary.
  (with-handlers ([exn:fail:read? report-malformed-output])
    (match (with-input-from-file path read)
      [(and (or (struct* run-status
                         ([outcome (or 'crashed
                                       'completed
                                       'timeout
                                       'oom
                                       'type-error)]
                          [blamed #f]))
                (struct* run-status
                         ([outcome 'blamed]
                          [blamed (not #f)])))
            result/well-formed)
       result/well-formed])))

;; dead-mutant-process?
;; ->
;; (or/c 'blamed 'type-error 'completed 'crashed 'timeout 'oom)
(define (process-outcome dead-proc)
  (run-status-outcome (dead-mutant-process-result dead-proc)))

;; result? -> (vector/c (or/c symbol? path-string?) path-string?)
(define/match (try-get-blamed/from-result result)
  [{(struct* run-status ([outcome 'blamed]
                         [blamed blamed]))}
   blamed]
  [{(struct* run-status ([outcome (not 'blamed)]))}
   #f])

;; dead-mutant-process? -> (vector (or/c symbol? path-string?))
(define/match (try-get-blamed dead-proc)
  [{(dead-mutant-process _ _ result _ _ _)}
   (try-get-blamed/from-result result)])

(define/match (try-get-type-error-module dead-proc)
  [{(struct* dead-mutant-process
             ([result
               (struct* run-status
                        ([outcome 'type-error]
                         [blamed blamed]))]))}
   blamed]
  [{_} #f])

(define (blamed-is-bug? blamed result)
  (match-define (vector blamed-id mod) blamed)
  (match result
    [(struct* run-status ([mutated-id (== blamed-id)]
                          [outcome 'blamed]
                          [blamed (== blamed)]))
     #t]
    [_ #f]))

(define (maybe-abort reason continue-val #:force [force? #f])
  ;; Mark the mutant error file before it gets garbled with error
  ;; messages from killing the current active mutants
  (log-factory fatal "Received abort signal with reason: ~a" reason)
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
         (exit 1)]
        [else
         (log-factory warning
                      "Continuing execution despite abort signal...")
         continue-val]))

(module+ main
  (require racket/cmdline)
  (define bench-to-run (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    name
    "Benchmark to run."
    (bench-to-run name)]
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
    (sample-size n)])
  (unless (bench-to-run)
    (error 'mutant-factory "Must provide benchmark to run."))
  (when (directory-exists? (data-output-dir))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))
  (parameterize ([date-display-format 'iso-8601])
    (log-factory info @~a{Running on benchmark @(bench-to-run)})
    (run-all-mutants*configs (read-benchmark (bench-to-run)))))
