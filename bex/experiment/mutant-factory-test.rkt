#lang at-exp racket

(require ruinit
         racket/logging
         racket/runtime-path
         "mutant-factory-test-helper.rkt"
         "blame-trail-data.rkt"
         (submod "mutant-factory.rkt" test)
         "../runner/mutation-runner.rkt"
         "../runner/unify-program.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../util/program.rkt"
         "../util/path-utils.rkt"
         (rename-in process-queue/mock
                    [make-process-queue           make-mock-Q]
                    [make-recording-process-queue make-recording-mock-Q])
         "../configurables/configurables.rkt")

(define-runtime-path test-config "../configurables/configs/test.rkt")
(install-configuration! (simple-form-path test-config))

(define e.rkt (file-name-string-from-path e-path))
(define main.rkt (file-name-string-from-path main-path))
(define loop.rkt (file-name-string-from-path loop-path))
(define dead-e-proc/crashed
  (dead-mutant-process (mutant #f e.rkt 0)
                       (hash e.rkt 'none
                             main.rkt 'none
                             loop.rkt 'none)
                       (run-status e.rkt
                                   0
                                   'baz
                                   'runtime-error
                                   #f
                                   (list e.rkt)
                                   empty
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
                                   #f
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
                                   empty
                                   empty
                                   #f)]))
(define dead-e-proc/blame-main+e
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status e.rkt
                                   0
                                   'baz
                                   'blamed
                                   (list main.rkt e.rkt)
                                   empty
                                   empty
                                   #f)]))
(define dead-e-proc/type-error-in-d
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status main.rkt
                                   0
                                   'baz
                                   'type-error
                                   (list main.rkt)
                                   #f
                                   #f
                                   #f)]))
(define dead-e-proc/oom/no-increased-limits
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status main.rkt
                                   0
                                   'baz
                                   'oom
                                   #f
                                   #f
                                   #f
                                   #f)]))

(define mutant0-mod "mutant0.rkt")
(define mutant1-mod "mutant1.rkt")
(define mutant2-mod "mutant2.rkt")
(define mutant0 (mutant #f mutant0-mod 0))
(define mutant1 (mutant #f mutant1-mod 1))
(define/match (mp->dead a-mp [result (file->value (mutant-process-file a-mp))])
  [{(mutant-process mutant config _ id blame-trail _ increased-limits?)
    _}
   (dead-mutant-process mutant
                        config
                        result
                        id
                        blame-trail
                        increased-limits?)])
(define (make:mutant0-proc [m-revivals (revivals 0 0)])
  (mutant-process mutant0
                  (hash mutant0-mod 'none
                        mutant1-mod 'none
                        mutant2-mod 'none
                        main.rkt 'none)
                  mutant0-path
                  1205
                  (blame-trail 42 '())
                  m-revivals
                  #f))
(define (make:mutant0-result outcome [blaming (list mutant1-mod)])
  (run-status main.rkt
              0
              'baz
              outcome
              blaming
              empty
              empty
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

(displayln
 @~a{

     The following tests may display error messages as part of testing error handling.
     These messages can be safely ignored in the absence of *test failure* messages.
     A test failure will be identified by a block labeled with: ----- FAILURE -----


     })

(test-begin
 #:name dead-process-blame/type-errors
 (test-equal? (process-outcome dead-e-proc/crashed)
              'runtime-error)
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
                        (serialize-config (dead-mutant-process-config dm0/follow)))
        (mutant-summary 1205
                        (dead-mutant-process-result dm0)
                        (serialize-config (dead-mutant-process-config dm0)))))))
  (define new-aggregate-file-contents
    (file->list aggregate-trail-file)))
 (test-= (length new-aggregate-file-contents) 2)
 (test-equal? new-aggregate-file-contents
              (append orig-aggregate-file-contents
                      (list expected-blame-trail-recording))))


(parameterize ([data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name follow-blame-from-dead-process
   (ignore
    (define enqueued (box #f))
    (define mock-q
      (make-mock-Q 2
                   (make:m0-factory)
                   #:enq (λ (q spawn-proc . _)
                           (define the-process-info (spawn-proc))
                           (set-box! enqueued (process-info-data the-process-info))
                           ((process-info-ctl the-process-info) 'kill)
                           q)))
    (define no-blame-handler-called (box #f))
    (define (set-no-blame-handler-called! q dead-proc)
      (set-box! no-blame-handler-called #t)
      q)
    (follow-blame-from-dead-process mock-q
                                    dead-e-proc/blame-e
                                    set-no-blame-handler-called!))
   (extend-test-message
    (mutant-process? (unbox enqueued))
    "Didn't enqueue a mutant following blame on e.rkt when it should have")
   (extend-test-message
    (false? (unbox no-blame-handler-called))
    "Called no-blame handler")
   (equal? (mutant-process-config (unbox enqueued))
           (hash-set (dead-mutant-process-config dead-e-proc/blame-e)
                     e.rkt 'types))

   (ignore (set-box! enqueued #f)
           (set-box! no-blame-handler-called #f)
           (follow-blame-from-dead-process mock-q
                                           dead-e-proc/blame-main+e
                                           set-no-blame-handler-called!))
   (extend-test-message
    (mutant-process? (unbox enqueued))
    "Didn't enqueue a mutant following blame on e.rkt and main.rkt when it should have")
   (extend-test-message
    (false? (unbox no-blame-handler-called))
    "Called no-blame handler")
   (equal? (mutant-process-config (unbox enqueued))
           (hash-set
            (hash-set (dead-mutant-process-config dead-e-proc/blame-e)
                      e.rkt 'types)
            main.rkt 'types))

   ;; Check that getting multiple blames with some already at types is OK, and
   ;; it just strengthens the ones not already at types
   (ignore (set-box! enqueued #f)
           (set-box! no-blame-handler-called #f)
           (define dead-e-proc/main-types
             (struct-copy dead-mutant-process dead-e-proc/blame-main+e
                          [config (hash-set (dead-mutant-process-config
                                             dead-e-proc/blame-main+e)
                                            main.rkt
                                            'types)]))
           (follow-blame-from-dead-process mock-q
                                           dead-e-proc/main-types
                                           set-no-blame-handler-called!))
   (extend-test-message
    (mutant-process? (unbox enqueued))
    "Didn't enqueue a mutant following blame on e.rkt and main.rkt when it should have")
   (extend-test-message
    (false? (unbox no-blame-handler-called))
    "Called no-blame handler")
   (equal? (mutant-process-config (unbox enqueued))
           (hash-set (dead-mutant-process-config dead-e-proc/main-types)
                     e.rkt 'types))

   (ignore (set-box! enqueued #f)
           (set-box! no-blame-handler-called #f)
           (define dead-e-proc/blame-e/e-already-types
             (struct-copy dead-mutant-process dead-e-proc/blame-e
                          [config (hash-set (dead-mutant-process-config
                                             dead-e-proc/blame-e)
                                            e.rkt
                                            'types)]))
           (follow-blame-from-dead-process mock-q
                                           dead-e-proc/blame-e/e-already-types
                                           set-no-blame-handler-called!))
   (extend-test-message
    (not (unbox enqueued))
    "Enqueued a mutant following blame on e.rkt when it's already at max")
   (extend-test-message
    (false? (unbox no-blame-handler-called))
    "Called no-blame handler")

   (ignore (set-box! enqueued #f)
           (set-box! no-blame-handler-called #f)
           (follow-blame-from-dead-process mock-q
                                           dead-e-proc/oom/no-increased-limits
                                           set-no-blame-handler-called!))
   (extend-test-message
    (not (unbox enqueued))
    "Enqueued a mutant following blame on oom outcome")
   (extend-test-message
    (unbox no-blame-handler-called)
    "Didn't call no-blame handler when there wasn't any blame")

   (ignore (set-box! enqueued #f)
           (set-box! no-blame-handler-called #f)
           (define dead-e-proc/blame-lib+e
             (struct-copy dead-mutant-process dead-e-proc/blame-e
                          [result
                           (struct-copy run-status
                                        (dead-mutant-process-result dead-e-proc/blame-e)
                                        [blamed '("e.rkt" "module-not-in-benchmark.rkt")])])))
   (with-handlers ([exn:fail? (const #f)])
     (follow-blame-from-dead-process mock-q
                                     dead-e-proc/blame-lib+e
                                     set-no-blame-handler-called!)
     #t)
   (extend-test-message
    (unbox enqueued)
    "Didn't enqueue a mutant following blame on e.rkt")
   (extend-test-message
    (not (unbox no-blame-handler-called))
    "Called no-blame handler when there was blame")))

(test-begin/with-env
 #:name make-blame-following-will/fallback
 (ignore
  (define enqueued (box #f))
  (define mock-q
    (make-mock-Q 2
                 (make:m0-factory)
                 #:enq (λ (q spawn-proc . _)
                         (define the-process-info (spawn-proc))
                         (set-box! enqueued (process-info-data the-process-info))
                         ((process-info-ctl the-process-info) 'kill)
                         q))))
 (ignore
  (define dead-e-proc/blame-lib
    (struct-copy dead-mutant-process dead-e-proc/blame-e
                 [result
                  (struct-copy run-status
                               (dead-mutant-process-result dead-e-proc/blame-e)
                               [blamed '("module-not-in-benchmark.rkt")])]))
  (parameterize ([data-output-dir test-mutant-dir])
    ((make-blame-following-will/fallback (λ (q _)
                                           (set-box! enqueued 'fallback)
                                           q))
     mock-q
     dead-e-proc/blame-lib)))
 (extend-test-message
  (test-equal? (unbox enqueued) #f)
  "\nEnqueued a mutant following blame into a library?")

 (ignore
  (set-box! enqueued #f)
  (define dead-e-proc/type-error-in-lib
    (struct-copy dead-mutant-process dead-e-proc/blame-e
                 [result
                  (struct-copy run-status
                               (dead-mutant-process-result dead-e-proc/blame-e)
                               [outcome 'type-error]
                               [blamed '("module-not-in-benchmark.rkt")]
                               [errortrace-stack #f]
                               [context-stack #f])]))
  (parameterize ([data-output-dir test-mutant-dir])
    ((make-blame-following-will/fallback (λ (q _)
                                           (set-box! enqueued 'fallback)
                                           q))
     mock-q
     dead-e-proc/type-error-in-lib)))
 (extend-test-message
  (test-equal? (unbox enqueued) #f)
  "\nEnqueued a mutant following type-error into a library?"))

(parameterize ([abort-on-failure? #f]
               [data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name make-blame-disappearing-fallback
   (ignore
    (define increased-limits?-box (box #f))
    (define fallback/oom
      (make-blame-disappearing-fallback
       dead-e-proc/oom/no-increased-limits
       (λ (q #:timeout/s t #:memory/gb m)
         (set-box! increased-limits?-box (or t m))
         q)))
    (fallback/oom (make-mock-Q 2 (make:m0-factory))
                  dead-e-proc/oom/no-increased-limits))
   (extend-test-message (unbox increased-limits?-box)
                        "oom process is not revived with increased limits")

   (ignore
    (set-box! increased-limits?-box #f)
    (define dead-e-proc/oom/increased-limits
      (struct-copy dead-mutant-process dead-e-proc/oom/no-increased-limits
                   [increased-limits? #t]))
    (fallback/oom (make-mock-Q 2 (make:m0-factory))
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
                                       #f
                                       #f
                                       #f)]))
    (fallback/oom (make-mock-Q 2 (make:m0-factory))
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
    (fallback/oom (make-mock-Q 2 (make:m0-factory))
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
      (fallback/oom (make-mock-Q 2 (make:m0-factory))
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
      (make-mock-Q 2 (make:m0-factory)
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
               (mutant-process (mutant #f main.rkt 42)
                               (== the-config)
                               _
                               _
                               (== the-trail)
                               (revivals 0 0)
                               #f))))

(define-test (test-mutant-will #:process-file process-file
                               #:status status
                               #:revival-counts revival-counts

                               #:test:should-respawn? should-respawn?
                               #:test:will-called? will-should-be-called?)
  (parameterize ([data-output-dir test-mutant-dir]
                 [abort-on-failure? #f])
    (define process-file-contents (file->bytes process-file))
    (define will-called?-box (box #f))
    (define (will:do-nothing q _)
      (set-box! will-called?-box #t)
      q)
    (define process-will:housekeeping+do-nothing
      (mutant->process-will will:do-nothing))
    (define respawned?-box (box #f))
    (define mock-Q
      (make-mock-Q 2 (make:m0-factory)
                   #:enq (λ (q spawn-proc . _)
                           (set-box! respawned?-box #t)
                           q)))
    (process-will:housekeeping+do-nothing
     mock-Q
     (process-info (mutant-process (mutant #f main.rkt 42)
                                   (hash main.rkt 'none
                                         e.rkt 'none
                                         loop.rkt 'types)
                                   process-file
                                   1505
                                   (blame-trail 95 '())
                                   revival-counts
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
      [else (void)])
    ;; Restore the file if it was deleted by the process will
    (display-to-file process-file-contents process-file
                     #:exists 'replace)))
(test-begin/with-env
 #:name mutant->process-will
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-ok
                   #:revival-counts (revivals 0 0)

                   #:test:should-respawn? #f
                   #:test:will-called? #t)
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-error
                   #:revival-counts (revivals 0 0)

                   #:test:should-respawn? #t
                   #:test:will-called? #f)
 (test-mutant-will #:process-file mutant0-path
                   #:status 'done-error
                   #:revival-counts (revivals MAX-FAILURE-REVIVALS 0)

                   #:test:should-respawn? #f
                   #:test:will-called? #f)
 (test-mutant-will #:process-file empty-file-path
                   #:status 'done-ok
                   #:revival-counts (revivals 1 0)

                   #:test:should-respawn? #t
                   #:test:will-called? #f)
 (test-mutant-will #:process-file empty-file-path
                   #:status 'done-ok
                   #:revival-counts (revivals MAX-FAILURE-REVIVALS 0)

                   #:test:should-respawn? #f
                   #:test:will-called? #f)
 (parameterize ([record/check-configuration-outcomes? `(check ,(const 'blamed))])
   (test-mutant-will #:process-file mutant1-path/2
                     #:status 'done-ok
                     #:revival-counts (revivals 0 0)

                     #:test:should-respawn? #t
                     #:test:will-called? #f))
 (parameterize ([record/check-configuration-outcomes? `(check ,(const 'blamed))])
  (test-mutant-will #:process-file mutant1-path/2
                    #:status 'done-ok
                    #:revival-counts (revivals 0 MAX-TYPE-ERROR-REVIVALS)

                    #:test:should-respawn? #f
                    #:test:will-called? #t)))

(test-begin/with-env
 #:name cache-replay/resume
 (ignore
  (define calls (make-hash))
  (define mock-q (make-recording-mock-Q 2
                                        (make:m0-factory)
                                        #:record-in calls))
  (parameterize ([current-result-cache
                  (λ _ mutant0-path)])
    (sample-blame-trails-if-max-config-result-ok mock-q
                                                 (mutant #f main.rkt 0))))
 (test-match calls
             (hash-table [_ 0] ___))

 (ignore
  (define remaining-procs 10)
  (parameterize ([current-result-cache
                  (match-lambda**
                   [{(== main.rkt) 0 (? (</c (- (sample-size) remaining-procs)))}
                    mutant0-path]
                   [{_ _ _} #f])])
    (sample-blame-trails-if-max-config-result-ok mock-q
                                                 (mutant #f main.rkt 0))))
 (test-match calls
             (hash-table ['enq (== remaining-procs)] [_ 0] ___))

 (ignore
  (hash-set! calls 'enq 0)
  (parameterize ([current-result-cache
                  (λ _ #f)])
    (sample-blame-trails-if-max-config-result-ok mock-q
                                                 (mutant #f main.rkt 0))))
 (test-match calls
             ;; only 1 because must spawn test mutant
             (hash-table ['enq 1] [_ 0] ___)))


(parameterize ([record/check-configuration-outcomes? #f]
               [abort-on-failure? #f])
  (test-begin/with-env
   #:name configuration-outcome-record/check

   ;; record
   (ignore (define (current-log-contents)
             (make-immutable-hash (file->list config-outcomes-log)))
           (define (clear-log-contents!)
             (with-output-to-file config-outcomes-log #:exists 'replace
               (thunk (displayln ""))))
           (define-test (test-outcome-checking/recording mode
                                                         do-record/check!-tests
                                                         [test-log-contents void]
                                                         #:clear-first? [clear-first? #t])
             (when clear-first?
               (clear-log-contents!)
               (set-box! abort-suppressed? #f))
             (and/test
              (parameterize ([record/check-configuration-outcomes? `(,mode ,config-outcomes-log)])
                (let ([finalize! (setup-configuration-outcome-record/checking!)])
                  (begin0 (do-record/check!-tests)
                    (finalize!))))
              (test-log-contents (current-log-contents))))
           (match-define (struct* mutant-process ([mutant mutant]
                                                  [config (app serialize-config config)]))
             (make:mutant0-proc)))

   (test-outcome-checking/recording
    'record
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                       (make:mutant0-result 'runtime-error))
                  #f))
    (λ (contents)
      (test-match contents
                  (hash-table [(== (list mutant config))
                               'runtime-error]))))

   (test-outcome-checking/recording
    'record
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                       (make:mutant0-result 'blamed))
                  #f))
    (λ (contents)
      (test-match contents
                  (hash-table [(== (list mutant config))
                               'blamed]))))

   ;; type errors need to be retried before being recorded
   (ignore (clear-log-contents!))
   (for/and/test ([i MAX-TYPE-ERROR-REVIVALS])
     (test-outcome-checking/recording
      #:clear-first? #f
      'record
      (thunk
       (test-equal? (record/check-configuration-outcome! (make:mutant0-proc
                                                          (revivals 0 i))
                                                         (make:mutant0-result 'type-error))
                    #t))
      (λ (contents)
        (not/test (hash-has-key? contents
                                 (list mutant config))))))
   (test-outcome-checking/recording
    #:clear-first? #f
    'record
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc
                                                        (revivals 0 MAX-TYPE-ERROR-REVIVALS))
                                                       (make:mutant0-result 'type-error))
                  #f))
    (λ (contents)
      (test-match contents
                  (hash-table [(== (list mutant config))
                               'type-error]))))

   ;; ----- check -----
   (test-outcome-checking/recording
    'record
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                       (make:mutant0-result 'blamed))
                  #f)))
   (test-outcome-checking/recording
    #:clear-first? #f
    'check
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                       (make:mutant0-result 'blamed))
                  #f)))
   (not (unbox abort-suppressed?))


   ;; type-errors need to be retried before being declared failures
   (test-outcome-checking/recording
    'record
    (thunk (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                             (make:mutant0-result 'blamed))
                        #f)))
   (for/and/test ([i MAX-TYPE-ERROR-REVIVALS])
     (and/test (test-outcome-checking/recording
                #:clear-first? #f
                'check
                (thunk
                 (test-equal? (record/check-configuration-outcome! (make:mutant0-proc
                                                                    (revivals 0 i))
                                                                   (make:mutant0-result 'type-error))
                              #t)))
               (not (unbox abort-suppressed?))))
   (test-outcome-checking/recording
    #:clear-first? #f
    'check
    (thunk (test-equal? (record/check-configuration-outcome! (make:mutant0-proc
                                                              (revivals 0 MAX-TYPE-ERROR-REVIVALS))
                                                             (make:mutant0-result 'type-error))
                        #f)))
   (unbox abort-suppressed?)

   ;; Checking an outcome that hasn't been recorded shouldn't cause any errors
   (test-outcome-checking/recording
    'check
    (thunk
     (test-equal? (record/check-configuration-outcome! (make:mutant0-proc)
                                                       (make:mutant0-result 'blamed))
                  #f)))
   (not (unbox abort-suppressed?))))


(require racket/os)

;; This is just one mixed config that I know should produce a ctc violation,
;; use it to spot-check blame trail data
(define mutant-with-config-blaming-main.rkt:mod "main.rkt")
(define mutant-with-config-blaming-main.rkt:index 4)
(define mutant-with-config-blaming-main.rkt:config (hash "main.rkt" 'none
                                                         "second.rkt" 'types))
;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit (match (gethostname)
                                [(regexp "quser[0-9]+") 1]
                                [else 6])]
               [sample-size (match (gethostname)
                                [(regexp "quser[0-9]+") 3]
                                [else 7])]
               [abort-on-failure? #f]
               [default-timeout/s (match (gethostname)
                                    [(regexp "quser[0-9]+") 240]
                                    [else 90])]
               [default-memory-limit/gb 1]
               [configured:make-bt-root-sampler
                (let ([original-sampler (configured:make-bt-root-sampler)])
                  (λ (bench-info the-mutant)
                    (λ (n)
                      (match the-mutant
                        ;; Make sure this config is in the set of sampled bt-roots
                        ;; See test below that expects it
                        [(mutant _
                                 (== mutant-with-config-blaming-main.rkt:mod)
                                 (== mutant-with-config-blaming-main.rkt:index))
                         (define roots-1
                           ((original-sampler bench-info the-mutant) (sub1 n)))
                         (cons mutant-with-config-blaming-main.rkt:config
                               roots-1)]
                        [else
                         ((original-sampler bench-info the-mutant) n)]))))])
  (set-box! abort-suppressed? #f)
  (displayln @~a{



                 Running full run test. This will take a while.
                 })
  (test-begin/with-env
   #:name test:full-run

   ;; Mutations expected:
   ;; - Mutations: 21
   ;;   - main.rkt: 18
   ;;     - foo: negate cond (@0)
   ;;            force cond
   ;;            foo -> main
   ;;            - -> +
   ;;            * -> /
   ;;            3 -> [-3
   ;;                  3.0
   ;;                  0
   ;;                  3+0.0i
   ;;                  #f]
   ;;     - main: swap bar args (@10)
   ;;             foo -> main
   ;;             2 -> [-2
   ;;                   2.0
   ;;                   0
   ;;                   2+0.0i
   ;;                   #f]
   ;;             "yes" -> #"yes"
   ;;   - second.rkt: 3
   ;;     - bar: negate cond (@0)
   ;;            force cond
   ;;            - -> +
   ;; - Mutations that make type errors: 14
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
   ;;   - second.rkt: 3
   ;;     - bar: negate cond
   ;;            force cond
   ;;            - -> +
   ;;
   ;; - Expect 15 decisions to sample (so 15 total files) (below)
   ;; - Expect (sample-size) samples for each
   (ignore
    (define expected-relevant-mutants
      '(("main.rkt" 2)
        ("main.rkt" 4)
        ("main.rkt" 6)
        ("main.rkt" 8)
        ("main.rkt" 9)
        ("main.rkt" 10)
        ("main.rkt" 11)
        ("main.rkt" 13)
        ("main.rkt" 15)
        ("main.rkt" 16)
        ("main.rkt" 17)

        ("second.rkt" 0)
        ("second.rkt" 1)
        ("second.rkt" 2)))
    (unless (member (list mutant-with-config-blaming-main.rkt:mod
                          mutant-with-config-blaming-main.rkt:index)
                    expected-relevant-mutants)
      (raise-user-error
       'full-run-test
       @~a{
           The mutations / relevant mutants are misconfigured:
           mutant-with-config-blaming-main.rkt: @(list mutant-with-config-blaming-main.rkt:mod
                                                       mutant-with-config-blaming-main.rkt:index) @;
           is missing from expected-relevant-mutants.
           }))

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
                                       (program->mods p)))
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
    (define progress-log-hash
      (make-hash))
    (define log-progress:in-hash
      (match-lambda [(cons k v)
                     (hash-set! progress-log-hash k v)]))
    (define log-progress:nowhere void)
    (define (run-the-experiment! #:log-progress [log-progress log-progress:in-hash])
      (run-all-mutants*configs bench
                               #:log-progress
                               (make-progress-logger log-progress)
                               #:load-progress
                               (thunk (make-cached-results-for progress-log-hash))))
    ;; (displayln @~a{Mutant errors: @(file->string (mutant-error-log))})
    ;; (displayln "Data:")
    ;; (for ([{m aggregate-file} (in-hash mutant-results)])
    ;;   (displayln
    ;;    @~a{Mutant file @m contents:
    ;;               @(pretty-format
    ;;                 (file->list aggregate-file))


    ;;               }))
    )
   (extend-test-message
    (run-the-experiment!)
    "Sanity checks failed.")
   (test-= (length (directory-list test-mutant-dir))
           (length expected-relevant-mutants))
   (for/and/test ([f (in-directory test-mutant-dir)])
                 (not/test (test-= (file-size f) 0)))

   (ignore
    (define data
      (for/hash ([f (in-directory test-mutant-dir)])
        (values (file-name-string-from-path f)
                (file->list f)
                #;(for/list ([line (file->lines f)])
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

   (test-match (hash-ref data (find-mutant-file mutant-with-config-blaming-main.rkt:mod
                                                mutant-with-config-blaming-main.rkt:index))
               (list-no-order
                (struct* blame-trail-summary
                         ([mutants
                           (list-no-order
                            (mutant-summary _
                                            (struct* run-status
                                                     ([outcome 'blamed]
                                                      [blamed '("main.rkt")]))
                                            (== (serialize-config mutant-with-config-blaming-main.rkt:config)))
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
                               ___)] ___))



   ;; Now try resuming and check nothing changes
   (extend-test-message
    (run-the-experiment!)
    "Sanity checks failed.")
   (test-= (length (directory-list test-mutant-dir))
           (length expected-relevant-mutants))

   ;; Now try resuming but there's a missing blame trail that won't get logged
   (ignore
    (define a-data-file (build-path test-mutant-dir
                                    (find-mutant-file mutant-with-config-blaming-main.rkt:mod
                                                      mutant-with-config-blaming-main.rkt:index)))
    (define contents (file->list a-data-file))
    (define to-drop 1)
    (with-output-to-file a-data-file
      #:exists 'truncate
      (thunk (for ([el (in-list contents)]
                   [i (in-naturals)])
               (match el
                 ;; the second trail
                 [(cons (list (== mutant-with-config-blaming-main.rkt:mod)
                              (== mutant-with-config-blaming-main.rkt:index)
                              (== to-drop)) _) (void)]
                 [else (writeln el)]))))
    (hash-remove! progress-log-hash
                  (list mutant-with-config-blaming-main.rkt:mod
                        mutant-with-config-blaming-main.rkt:index
                        to-drop)))
   (extend-test-message
    (not (run-the-experiment! #:log-progress log-progress:nowhere))
    "Sanity checks succeeded but should have failed.")))


;; (display-test-results)
