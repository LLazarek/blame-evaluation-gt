#lang at-exp racket

(require ruinit
         racket/logging
         "mutant-factory-test-helper.rkt"
         "blame-trail-data.rkt"
         (submod "mutant-factory.rkt" test)
         "../runner/mutation-runner.rkt"
         "../runner/program.rkt"
         "../runner/unify-program.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../util/path-utils.rkt"
         "../process-q/interface.rkt"
         "../configurables/configurables.rkt")


(module mock racket
  (provide make-mock-Q
           make-recording-mock-Q)
  (require "../process-q/interface.rkt"
           (submod "../process-q/interface.rkt" internal))
  (define (make-mock-Q [data-init #f]
                       #:empty? [empty? (λ _ #f)]
                       #:enq [enq (λ (q . _) q)]
                       #:wait [wait (λ (q) q)]
                       #:active-count [active-count (λ (q) 1)]
                       #:waiting-count [waiting-count (λ (q) 1)]
                       #:get-data [get-data process-Q-data]
                       #:set-data [set-data (λ (q new)
                                              (struct-copy process-Q q
                                                           [data new]))])
    (process-Q empty?
               enq
               wait
               active-count
               waiting-count
               get-data
               set-data

               data-init))
  (define (make-recording-mock-Q [data-init #f]
                                 #:record-in h)
    (for ([k (in-list '(empty? enq wait active-count waiting-count))])
      (hash-set! h k 0))
    (define (add-call! name)
      (hash-update! h name add1 0))
    (make-mock-Q data-init
                 #:empty? (λ (q) (add-call! 'empty?) #f)
                 #:enq (λ (q v . _) (add-call! 'enq) q)
                 #:wait (λ (q) (add-call! 'wait) q)
                 #:active-count (λ (q) (add-call! 'active-count) 1)
                 #:waiting-count (λ (q) (add-call! 'waiting-count) 1)))
  (require ruinit)
  (test-begin
    #:name mock-test
    (ignore
     (define call-hash (make-hash))
     (define mock-q
       (make-mock-Q
        1
        #:empty? (λ _
                   (hash-set! call-hash 'empty? #t)
                   #f)
        #:enq (λ (q . _)
                (hash-set! call-hash 'enq #t)
                q)
        #:wait (λ (q)
                 (hash-set! call-hash 'wait #t)
                 q)
        #:active-count (λ _
                         (hash-set! call-hash 'active-count #t)
                         42)
        #:waiting-count (λ _
                          (hash-set! call-hash 'waiting-count #t)
                          24))))
    (begin (process-Q-empty? mock-q)
           (hash-ref call-hash 'empty? #f))
    (begin (process-Q-enq mock-q (thunk 42))
           (hash-ref call-hash 'enq #f))
    (begin (process-Q-wait mock-q)
           (hash-ref call-hash 'wait #f))
    (test-= (process-Q-active-count mock-q)
            42)
    (hash-ref call-hash 'active-count #f)
    (test-= (process-Q-waiting-count mock-q)
            24)
    (hash-ref call-hash 'waiting-count #f)
    (test-= (process-Q-get-data (process-Q-set-data mock-q 2))
            2)))

(current-configuration-path (simple-form-path "../configurables/test.config"))

(define e.rkt (file-name-string-from-path e-path))
(define main.rkt (file-name-string-from-path main-path))
(define loop.rkt (file-name-string-from-path loop-path))
(define dead-e-proc/crashed
  (dead-mutant-process (mutant e.rkt 0 #t)
                       (hash e.rkt 'none
                             main.rkt 'none
                             loop.rkt 'none)
                       (run-status e.rkt
                                   0
                                   'baz
                                   'blamed
                                   (list e.rkt)
                                   #f)
                       42
                       (blame-trail 95 '())
                       #f))
(define dead-e-proc/completed
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status e.rkt
                                   0
                                   'baz
                                   'completed
                                   #f
                                   #f)]))
(define dead-e-proc/blame-e
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status e.rkt
                                   0
                                   'baz
                                   'blamed
                                   (list e.rkt)
                                   #f)]))
(define dead-e-proc/type-error-in-d
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status main.rkt
                                   0
                                   'baz
                                   'type-error
                                   (list main.rkt)
                                   #f)]))
(define dead-e-proc/oom/no-increased-limits
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status main.rkt
                                   0
                                   'baz
                                   'oom
                                   #f
                                   #f)]))

(define mutant0-mod "mutant0.rkt")
(define mutant1-mod "mutant1.rkt")
(define mutant2-mod "mutant2.rkt")
(define mutant0 (mutant mutant0-mod 0 #t))
(define mutant1 (mutant mutant1-mod 1 #t))
(define/match (mp->dead a-mp [result (file->value (mutant-process-file a-mp))])
  [{(mutant-process mutant config _ id blame-trail _ increased-limits?)
    _}
   (dead-mutant-process mutant
                        config
                        result
                        id
                        blame-trail
                        increased-limits?)])
(define (make:mutant0-proc)
  (mutant-process mutant0
                  (hash mutant0-mod 'none
                        mutant1-mod 'none
                        mutant2-mod 'none
                        main.rkt 'none)
                  mutant0-path
                  1205
                  (blame-trail 42 '())
                  0
                  #f))
(define (make:mutant0-dead-proc)
  (mp->dead (make:mutant0-proc)))
(define (make:mutant0-proc/following-42)
  (struct-copy mutant-process (make:mutant0-proc)
               [config (hash mutant0-mod 'types
                             mutant1-mod 'none
                             mutant2-mod 'none
                             main.rkt 'none)]
               [blame-trail (blame-trail 42
                                         (list (make:mutant0-dead-proc)))]
               [id 1206]))
(define (make:mutant0-dead-proc/following-42)
  (mp->dead (make:mutant0-proc/following-42)))


(define make-test-benchmark (thunk (read-benchmark test-bench)))
(define make:m0-factory
  (thunk
   (for ([f (in-directory test-bench/ut)])
     (define name (file-name-string-from-path f))
     (define typed-version (build-path test-bench/t name))
     (unless (file-exists? typed-version)
       (copy-file f typed-version)))
   (define test-benchmark (make-test-benchmark))
   (factory (bench-info test-benchmark
                        (make-max-bench-config test-benchmark))
            (hash mutant0
                  partial-e-bt-aggregate-file)
            (hash)
            5)))


(test-begin
 #:name dead-process-blame/type-errors
 (test-equal? (try-get-blamed-modules dead-e-proc/crashed)
              (list e.rkt))
 (not/test (try-get-blamed-modules dead-e-proc/completed))
 (test-equal? (try-get-blamed-modules dead-e-proc/blame-e)
              (list e.rkt))
 (not/test (try-get-blamed-modules dead-e-proc/type-error-in-d))

 (not/test (try-get-type-error-module dead-e-proc/crashed))
 (not/test (try-get-type-error-module dead-e-proc/completed))
 (not/test (try-get-type-error-module dead-e-proc/blame-e))
 (test-equal? (try-get-type-error-module dead-e-proc/type-error-in-d)
              (list main.rkt))

 (test-equal? (process-outcome dead-e-proc/crashed)
              'blamed)
 (test-equal? (process-outcome dead-e-proc/completed)
              'completed)
 (test-equal? (process-outcome dead-e-proc/blame-e)
              'blamed)
 (test-equal? (process-outcome dead-e-proc/type-error-in-d)
              'type-error))


(test-begin/with-env
 #:name mutant-results
 (ignore
  (define mutant0-proc-result
    (file->value (mutant-process-file (make:mutant0-proc)))))
 (test-equal? (read-mutant-result (make:mutant0-proc))
              mutant0-proc-result)

 (ignore
  (define aggregate-trail-file partial-e-bt-aggregate-file)
  (define orig-aggregate-file-contents (file->list aggregate-trail-file))
  (define dead (make:mutant0-dead-proc/following-42))
  (define full-42-trail
    (extend-blame-trail (dead-mutant-process-blame-trail dead)
                        dead))
  (define new-factory
    (record-blame-trail! (make:m0-factory)
                         full-42-trail))
  (define expected-blame-trail-recording
    (blame-trail-summary
     mutant0-mod
     0
     42
     (let ([dm0 (make:mutant0-dead-proc)]
           [dm0/follow (make:mutant0-dead-proc/following-42)])
       (list
        (mutant-summary 1206
                        (dead-mutant-process-result dm0/follow)
                        (dead-mutant-process-config dm0/follow))
        (mutant-summary 1205
                        (dead-mutant-process-result dm0)
                        (dead-mutant-process-config dm0))))))
  (define new-aggregate-file-contents
    (file->list aggregate-trail-file)))
 (test-= (length new-aggregate-file-contents) 2)
 (test-equal? new-aggregate-file-contents
              (append orig-aggregate-file-contents
                      (list expected-blame-trail-recording))))


(require 'mock)
(parameterize ([data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name follow-blame-from-dead-process
   (ignore
    (define enqueued (box #f))
    (define mock-q
      (make-mock-Q (make:m0-factory)
                   #:enq (λ (q spawn-proc . _)
                           (define the-process-info (spawn-proc))
                           (set-box! enqueued (process-info-data the-process-info))
                           ((process-info-ctl the-process-info) 'kill)
                           q)))
    (follow-blame-from-dead-process mock-q
                                    dead-e-proc/blame-e
                                    (list e.rkt)))
   (extend-test-message
    (mutant-process? (unbox enqueued))
    "Didn't enqueue a mutant following blame on e.rkt when it should have")
   (equal? (mutant-process-config (unbox enqueued))
           (hash-set (dead-mutant-process-config dead-e-proc/blame-e)
                     e.rkt 'types))

   (ignore (set-box! enqueued #f)
           (follow-blame-from-dead-process mock-q
                                           dead-e-proc/blame-e
                                           (list main.rkt e.rkt)))
   (extend-test-message
    (mutant-process? (unbox enqueued))
    "Didn't enqueue a mutant following blame on e.rkt and main.rkt when it should have")
   (equal? (mutant-process-config (unbox enqueued))
           (hash-set
            (hash-set (dead-mutant-process-config dead-e-proc/blame-e)
                      e.rkt 'types)
            main.rkt 'types))

   (ignore
    (set-box! enqueued #f)
    (define dead-e-proc/blame-e/e-already-types
      (struct-copy dead-mutant-process dead-e-proc/blame-e
                   [config (hash-set (dead-mutant-process-config
                                      dead-e-proc/blame-e)
                                     e.rkt
                                     'types)]))
    (follow-blame-from-dead-process mock-q
                                    dead-e-proc/blame-e/e-already-types
                                    (list e.rkt)))
   (extend-test-message
    (not (unbox enqueued))
    "Enqueued a mutant following blame on e.rkt when it's already at max")

   (ignore
    (set-box! enqueued #f)
    (define dead-e-proc/blame-lib
      (struct-copy dead-mutant-process dead-e-proc/blame-e
                   [result
                    (struct-copy run-status
                                 (dead-mutant-process-result dead-e-proc/blame-e)
                                 [blamed '("../base/csp.rkt")])]))
    ((make-blame-following-will/fallback (λ (q _)
                                           (set-box! enqueued 'fallback)
                                           q))
     mock-q
     dead-e-proc/blame-lib))
   (extend-test-message
    (test-equal? (unbox enqueued) 'fallback)
    "\nEnqueued a mutant following blame into a library?")))

(parameterize ([abort-on-failure? #f])
  (test-begin/with-env
   #:name make-blame-disappearing-fallback
   (ignore
    (define increased-limits?-box (box #f))
    (define fallback/oom
      (make-blame-disappearing-fallback
       dead-e-proc/oom/no-increased-limits
       (list main.rkt)
       (λ (q #:timeout/s t #:memory/gb m)
         (set-box! increased-limits?-box (or t m))
         q)))
    (fallback/oom (make-mock-Q (make:m0-factory))
                  dead-e-proc/oom/no-increased-limits))
   (extend-test-message (unbox increased-limits?-box)
                        "oom process is not revived with increased limits")

   (ignore
    (set-box! increased-limits?-box #f)
    (define dead-e-proc/oom/increased-limits
      (struct-copy dead-mutant-process dead-e-proc/oom/no-increased-limits
                   [increased-limits? #t]))
    (fallback/oom (make-mock-Q (make:m0-factory))
                  dead-e-proc/oom/increased-limits))
   (extend-test-message
    (not (unbox increased-limits?-box))
    "oom^2 process is revived with increased limits, but should give up")

   (ignore
    (set-box! increased-limits?-box #f)
    (define dead-e-proc/completed
      (struct-copy dead-mutant-process dead-e-proc/oom/no-increased-limits
                   [result (run-status main.rkt
                                       0
                                       'baz
                                       'completed
                                       #f
                                       #f)]))
    (fallback/oom (make-mock-Q (make:m0-factory))
                  dead-e-proc/completed))
   (extend-test-message
    (not (unbox increased-limits?-box))
    "true blame disappearing not recognized")

   (ignore
    (set-box! increased-limits?-box #f)
    (define dead-e-proc/blamed-lib
      (struct-copy dead-mutant-process dead-e-proc/blame-e
                   [result
                    (struct-copy run-status
                                 (dead-mutant-process-result dead-e-proc/blame-e)
                                 [blamed '("../base/csp.rkt")])]))
    (fallback/oom (make-mock-Q (make:m0-factory))
                  dead-e-proc/blamed-lib))
   (extend-test-message
    (not (unbox increased-limits?-box))
    "blame disappearing into library not recognized")

   (ignore
    (set-box! increased-limits?-box #f)
    (define dead-e-proc/runtime-error-no-blamed
      (struct-copy dead-mutant-process dead-e-proc/blame-e
                   [result
                    (struct-copy run-status
                                 (dead-mutant-process-result dead-e-proc/blame-e)
                                 [outcome 'runtime-error]
                                 [blamed '()])])))
   (extend-test-message
    (with-handlers ([exn:fail? (const #f)])
      (fallback/oom (make-mock-Q (make:m0-factory))
                    dead-e-proc/runtime-error-no-blamed)
      #t)
    "runtime-error without inferred blame crashes factory")
   (extend-test-message
    (not (unbox increased-limits?-box))
    "runtime-error without inferred blame not recognized")))

(parameterize ([data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name spawn-mutant
   (ignore
    (define enqueued (box #f))
    (define mock-q
      (make-mock-Q (make:m0-factory)
                   #:enq (λ (q spawn-proc . _)
                           (define the-process-info (spawn-proc))
                           (set-box! enqueued (process-info-data the-process-info))
                           ((process-info-ctl the-process-info) 'kill)
                           q)))
    (define the-config (hash main.rkt 'none
                             e.rkt 'none
                             loop.rkt 'types))
    (define the-trail (blame-trail 101 '()))
    (spawn-mutant mock-q
                  main.rkt
                  42
                  the-config
                  (λ (q dead-proc)
                    q)
                  #:following-trail the-trail))
   (test-match (unbox enqueued)
               (mutant-process (mutant main.rkt 42 #t)
                               (== the-config)
                               _
                               _
                               (== the-trail)
                               0
                               #f))))

(define-test (test-mutant-will #:process-file process-file
                               #:status status
                               #:revival-count revival-count

                               #:test:should-respawn? should-respawn?
                               #:test:will-called? will-should-be-called?)
  (parameterize ([data-output-dir test-mutant-dir]
                 [abort-on-failure? #f])
    (define will-called?-box (box #f))
    (define (will:do-nothing q _)
      (set-box! will-called?-box #t)
      q)
    (define process-will:housekeeping+do-nothing
      (mutant->process-will will:do-nothing))
    (define respawned?-box (box #f))
    (define mock-Q
      (make-mock-Q (make:m0-factory)
                   #:enq (λ (q spawn-proc . _)
                           (set-box! respawned?-box #t)
                           q)))
    (process-will:housekeeping+do-nothing
     mock-Q
     (process-info (mutant-process (mutant main.rkt 42 #t)
                                   (hash main.rkt 'none
                                         e.rkt 'none
                                         loop.rkt 'types)
                                   process-file
                                   1505
                                   (blame-trail 95 '())
                                   revival-count
                                   #f)
                   (const status)
                   process-will:housekeeping+do-nothing))
    (match (cons should-respawn?
                 (unbox respawned?-box))
      [(cons #t #f) (fail "Mutant did not respawn when it should have")]
      [(cons #f #t) (fail "Mutant respawned when it shouldn't have")]
      [else (void)])
    (match (cons will-should-be-called?
                 (unbox will-called?-box))
      [(cons #t #f) (fail "Will was not called when it should have been")]
      [(cons #f #t) (fail "Will called when it shouldn't have been")]
      [else (void)])))
(test-begin/with-env
 #:name mutant->process-will
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-ok
                   #:revival-count 0

                   #:test:should-respawn? #f
                   #:test:will-called? #t)
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-error
                   #:revival-count 0

                   #:test:should-respawn? #t
                   #:test:will-called? #f)
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-error
                   #:revival-count MAX-REVIVALS

                   #:test:should-respawn? #f
                   #:test:will-called? #f)
 (test-mutant-will #:process-file empty-file-path
                   #:status 'done-ok
                   #:revival-count 1

                   #:test:should-respawn? #t
                   #:test:will-called? #f)
 (test-mutant-will #:process-file empty-file-path
                   #:status 'done-ok
                   #:revival-count MAX-REVIVALS

                   #:test:should-respawn? #f
                   #:test:will-called? #f))


(test-begin/with-env
 #:name cache-replay/resume
 (ignore
  (define calls (make-hash))
  (define mock-q (make-recording-mock-Q (make:m0-factory)
                                        #:record-in calls))
  (parameterize ([current-result-cache
                  (λ _ mutant0-path)])
    (sample-blame-trails-if-type-error mock-q
                                       (mutant main.rkt 0 #t))))
 (test-match calls
             (hash-table [_ 0] ___))

 (ignore
  (define remaining-procs 10)
  (parameterize ([current-result-cache
                  (match-lambda**
                   [{(== main.rkt) 0 (? (</c (- (sample-size) remaining-procs)))}
                    mutant0-path]
                   [{_ _ _} #f])])
    (sample-blame-trails-if-type-error mock-q
                                       (mutant main.rkt 0 #t))))
 (test-match calls
             (hash-table ['enq (== remaining-procs)] [_ 0] ___))

 (ignore
  (hash-set! calls 'enq 0)
  (parameterize ([current-result-cache
                  (λ _ #f)])
    (sample-blame-trails-if-type-error mock-q
                                       (mutant main.rkt 0 #t))))
 (test-match calls
             ;; only 1 because must spawn test mutant
             (hash-table ['enq 1] [_ 0] ___)))


(require racket/os)
;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit (match (gethostname)
                                [(regexp "quser[0-9]+") 1]
                                [else 6])]
               [sample-size (match (gethostname)
                                [(regexp "quser[0-9]+") 3]
                                [else 10])]
               [abort-on-failure? #f]
               [default-timeout/s (match (gethostname)
                                    [(regexp "quser[0-9]+") 240]
                                    [else 90])]
               [default-memory-limit/gb 1])
  (displayln @~a{



                 Running full run test. This will take a while.
                 })
  (test-begin/with-env
   #:name test:full-run

   ;; Mutations expected:
   ;; - Mutations: 19
   ;;   - main.rkt: 17
   ;;     - foo: negate cond (@0)
   ;;            foo -> main
   ;;            - -> +
   ;;            * -> /
   ;;            3 -> [-3
   ;;                  3.0
   ;;                  0
   ;;                  3+0.0i
   ;;                  #f]
   ;;     - main: swap bar args (@9)
   ;;             foo -> main
   ;;             2 -> [-2
   ;;                   2.0
   ;;                   0
   ;;                   2+0.0i
   ;;                   #f]
   ;;             "yes" -> #"yes"
   ;;   - second.rkt: 2
   ;;     - bar: negate cond (@0)
   ;;            - -> +
   ;; - Mutations that make type errors: 13
   ;;   - main.rkt: 11
   ;;     - foo: foo -> main
   ;;            * -> /
   ;;            3 -> 3.0
   ;;            3 -> 3+0.0i
   ;;            3 -> #f
   ;;     - main: swap bar args
   ;;             foo -> main
   ;;             2 -> 2.0
   ;;             2 -> 2+0.0i
   ;;             2 -> #f
   ;;             "yes" -> #"yes"
   ;;   - second.rkt: 2
   ;;     - bar: negate cond
   ;;            - -> +
   ;;
   ;; - Expect 13 decisions to sample (so 13 total files) (below)
   ;; - Expect (sample-size) samples for each
   (ignore
    (define expected-relevant-mutants
      '(("main.rkt" 1)
        ("main.rkt" 3)
        ("main.rkt" 5)
        ("main.rkt" 7)
        ("main.rkt" 8)
        ("main.rkt" 9)
        ("main.rkt" 10)
        ("main.rkt" 12)
        ("main.rkt" 14)
        ("main.rkt" 15)
        ("main.rkt" 16)

        ("second.rkt" 0)
        ("second.rkt" 1)))

    (match (mutant-error-log)
      [(? file-exists? path) (delete-file path)]
      [else (void)])
    (define bench (read-benchmark realistic-test-bench ))

    (define (run-with-mutated-module+ name index config)
      (match-define (hash-table
                     [(? (path-ends-with "main.rkt"))   main-level]
                     [(? (path-ends-with "second.rkt")) second-level])
        config)
      (define main-mod
        (findf (path-ends-with "main.rkt")
               (match main-level
                 ['types (benchmark-typed bench)]
                 ['none (benchmark-untyped bench)])))
      (define second-mod
        (findf (path-ends-with "second.rkt")
               (match second-level
                 ['types (benchmark-typed bench)]
                 ['none (benchmark-untyped bench)])))
      (define p (make-unified-program main-mod
                                      (list second-mod)))
      (define mod-to-mutate
        (find-unified-module-to-mutate (findf (path-ends-with name)
                                              (benchmark-typed bench))
                                       (list* (program-main p)
                                              (program-others p))))
      (displayln
       (run-with-mutated-module p
                                mod-to-mutate
                                index
                                ;; #:modules-base-path (current-directory)
                                ;; #:write-modules-to "mutant-factory-test-dump"
                                ;; #:on-module-exists 'replace
                                #:suppress-output? #f))
      (displayln "Written; Enter to contineu")
      (read-line))
    #;(run-with-mutated-module+ "second.rkt"
                              0
                              '#hash(("second.rkt" . none)
                                     ("main.rkt" . types)))
    (define mutant-results (run-all-mutants*configs bench
                                                    #:log-progress void
                                                    #:resume-cache (const #f)))
    ;; (displayln @~a{Mutant errors: @(file->string (mutant-error-log))})
    ;; (displayln "Data:")
    ;; (for ([{m aggregate-file} (in-hash mutant-results)])
    ;;   (displayln
    ;;    @~a{Mutant file @m contents:
    ;;               @(pretty-format
    ;;                 (file->list aggregate-file))


    ;;               }))
    )
   (test-= (length (directory-list test-mutant-dir))
           (length expected-relevant-mutants))
   (for/and/test ([f (in-directory test-mutant-dir)])
                 (not/test (test-= (file-size f) 0)))

   (ignore
    (define data
      (for/hash ([f (in-directory test-mutant-dir)])
        (values (file-name-string-from-path f)
                (for/list ([line (file->lines f)])
                  (call-with-input-string line read)))))
    (define (find-mutant-file mod-name mutation-index)
      (findf (λ (f) (regexp-match? (mutant-data-file-name mod-name
                                                          mutation-index)
                                   f))
             (hash-keys data)))
    (define (count-blame-trails mutant-file)
      (define parts (hash-ref data mutant-file))
      (define blame-trail-ids
        (remove-duplicates
         (filter-map (match-lambda
                       [(blame-trail-summary _
                                             _
                                             (? natural? bt-id)
                                             _)
                        bt-id]
                       [else #f])
                     parts)))
      (length blame-trail-ids)))

   (for/and/test
    ([mutant-info (in-list expected-relevant-mutants)])
    (define mutant-file (find-mutant-file (first mutant-info)
                                          (second mutant-info)))
    (and/test/message
     [mutant-file @~a{mutant file @mutant-info doesn't exist}]
     [(test-= (length (hash-ref data mutant-file))
              (sample-size))
      @~a{@mutant-file doesn't contain `(sample-size)` samples:}]
     [(test-= (count-blame-trails mutant-file)
              (length (hash-ref data mutant-file)))
      @~a{@mutant-file doesn't contain one summary per blame trail:}]
     [(test-= (count-blame-trails mutant-file)
              (sample-size))
      @~a{@mutant-file doesn't contain `(sample-size)` blame trails:}]))

   ;; check that mixed configurations work as expected:
   ;; this is just one mixed config that I know should produce a ctc violation
   (test-match (hash-ref data (find-mutant-file "main.rkt" 3))
               (list-no-order
                (struct* blame-trail-summary
                         ([mutants
                           (list-no-order
                            (mutant-summary _
                                            (struct* run-status
                                                     ([outcome 'blamed]
                                                      [blamed '("main.rkt")]))
                                            (== (hash "main.rkt" 'none
                                                      "second.rkt" 'types)))
                            _ ___)]))
                _ ___))

   (test-match data
               (hash-table [_ (list
                               (struct* blame-trail-summary
                                        ([mutants
                                          (list
                                           (mutant-summary _
                                                           (struct* run-status
                                                                    ([outcome
                                                                      (or 'type-error
                                                                          'blamed
                                                                          'runtime-error
                                                                          'completed
                                                                          'oom
                                                                          'timeout)]))
                                                           _)
                                           ___)]))
                               ___)] ___))))


;; (display-test-results)
