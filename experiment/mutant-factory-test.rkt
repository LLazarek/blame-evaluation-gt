#lang at-exp racket

(module test-helper racket
  (require ruinit
           syntax/parse/define
           "../runner/mutation-runner.rkt"
           (only-in (submod "mutant-factory.rkt" test)
                    mutant-error-log
                    aggregated-result))
  (provide test-begin/with-env)

  (define rt-main-body
    @~a{
        (define (foo x)
          (if (positive? x)
              (foo (- x))
              (* x x 3)))
        (define (main)
          (bar (foo 2) "yes"))
        (main)
        })
  (define rt-second-body
    @~a{
        (provide bar)
        (define (bar x s)
          (if (positive? x)
              (- x)
              x))
        })
  (define-test-env (setup-test-env! cleanup-test-env!)
    #:directories ([test-mutant-dir (simplify-path "./test-mutants")]
                   [test-bench (simplify-path "./test-bench")]
                   [test-bench/ut (simplify-path "./test-bench/untyped")]
                   [test-bench/t (simplify-path "./test-bench/typed")] ; empty

                   [realistic-test-bench (simplify-path "./real-test-bench")]
                   [realistic-test-bench/ut (simplify-path "./real-test-bench/untyped")]
                   [realistic-test-bench/t (simplify-path "./real-test-bench/typed")])
    #:files ([e-path (build-path test-bench/ut "e.rkt")
                     @~a{
                         #lang racket

                         (provide baz)

                         (define (baz x y)
                           (if (even? x)
                               y
                               (/ y x)))
                         }]
             [main-path (build-path test-bench/ut "main.rkt")
                     @~a{
                         #lang typed/racket

                         (require "e.rkt" "loop.rkt")

                         (: foo (-> Number Number))
                         (define (foo x)
                           (- x))

                         (loop)
                         (foo (baz 0 22))
                         }]
             [loop-path (build-path test-bench/ut "loop.rkt")
                        @~a{
                            #lang typed/racket
                            (provide loop)
                            (: loop (-> Number))
                            (define (loop)
                              (if #f (loop) 42))
                            }]
             [mutant0-path "m0.rktd"
                           (format "~s\n"
                                   (run-status "m0.rkt"
                                               0
                                               'foo
                                               'crashed
                                               #f
                                               #f))]
             [mutant1-path/1 "m11.rktd"
                             (format "~s\n"
                                     (run-status "m1.rkt"
                                                 0
                                                 'foo
                                                 'blamed
                                                 "mutant-factory-test.rkt"
                                                 #f))]
             [mutant1-path/2 "m12.rktd"
                             (format "~s\n"
                                     (run-status "m1.rkt"
                                                 42
                                                 'foo
                                                 'type-error
                                                 "m1.rkt"
                                                 #f))]
             [mutant2-path "m2.rktd"
                           (format "~s\n"
                                   (run-status "m1.kt"
                                               42
                                               'foo
                                               'type-error
                                               "m2.rkt"
                                               #f))]
             [empty-file-path "empty-file.rktd"
                              ""]
             [error-log (mutant-error-log)
                        ""]

             [rt-main/ut (build-path realistic-test-bench/ut
                                     "main.rkt")
                         @~a{
                             #lang racket
                             (require "second.rkt")
                             @rt-main-body
                             }]
             [rt-second/ut (build-path realistic-test-bench/ut
                                       "second.rkt")
                           @~a{
                               #lang racket
                               @rt-second-body
                               }]
             [rt-main/t (build-path realistic-test-bench/t
                                     "main.rkt")
                         @~a{
                             #lang typed/racket
                             (require require-typed-check)
                             (require/typed/check
                              "second.rkt"
                              [bar (-> Integer String Nonpositive-Integer)])
                             (: foo (-> Integer Integer))
                             (: main (-> Integer))
                             @rt-main-body
                             }]
             [rt-second/t (build-path realistic-test-bench/t
                                       "second.rkt")
                           @~a{
                               #lang typed/racket
                               (: bar (-> Integer String Nonpositive-Integer))
                               @rt-second-body
                               }])

    #:provide)

  (define-simple-macro (test-begin/with-env #:name name
                                            e ...)
    (test-begin
      #:name name
      #:short-circuit
      #:before (setup-test-env!)
      #:after (cleanup-test-env!)
      e ...)))


(require ruinit
         racket/logging
         'test-helper
         (submod "mutant-factory.rkt" test)
         "../runner/mutation-runner.rkt"
         "../runner/instrumented-runner.rkt"
         "../runner/unify-program.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../util/path-utils.rkt")

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
                                   e.rkt
                                   #f)
                       42
                       'sample
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
                                   e.rkt
                                   #f)]
               [blame-trail-id 42]))
(define dead-e-proc/type-error-in-d
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status main.rkt
                                   0
                                   'baz
                                   'type-error
                                   main.rkt
                                   #f)]))
(test-begin
 #:name dead-process-blame/type-errors
 (test-equal? (try-get-blamed dead-e-proc/crashed)
              e.rkt)
 (not/test (try-get-blamed dead-e-proc/completed))
 (test-equal? (try-get-blamed dead-e-proc/blame-e)
              e.rkt)
 (not/test (try-get-blamed dead-e-proc/type-error-in-d))

 (not/test (try-get-type-error-module dead-e-proc/crashed))
 (not/test (try-get-type-error-module dead-e-proc/completed))
 (not/test (try-get-type-error-module dead-e-proc/blame-e))
 (test-equal? (try-get-type-error-module dead-e-proc/type-error-in-d)
              main.rkt)

 (test-equal? (process-outcome dead-e-proc/crashed)
              'blamed)
 (test-equal? (process-outcome dead-e-proc/completed)
              'completed)
 (test-equal? (process-outcome dead-e-proc/blame-e)
              'blamed)
 (test-equal? (process-outcome dead-e-proc/type-error-in-d)
              'type-error))

(test-begin/with-env
 #:name max-mutation-index-exceeded?

 (for/and/test ([i (in-range 3)])
               (not (max-mutation-index-exceeded? e-path i)))
 (max-mutation-index-exceeded? e-path 3)

 (not (max-mutation-index-exceeded? main-path 0))
 (max-mutation-index-exceeded? main-path 1)

 (for/and/test
  ([rt-main (in-list (list rt-main/t rt-main/ut))]
   [rt-second (in-list (list rt-second/t rt-second/ut))])
  (and/test/message
   [(for/and/test ([i (in-range 6)])
                  (extend-test-message
                   (not (max-mutation-index-exceeded? rt-main i))
                   @~a{(stopped at index @i)}))
    @~a{Not all expected mutations of @rt-main happening}]
   [(max-mutation-index-exceeded? rt-main 6)
    @~a{@rt-main has more mutations than expected}]
   [(for/and/test ([i (in-range 2)])
                  (extend-test-message
                   (not (max-mutation-index-exceeded? rt-second i))
                   @~a{(stopped at index @i)}))
    @~a{@rt-second doesn't have the expected mutations}]
   [(max-mutation-index-exceeded? rt-second 2)
    @~a{@rt-second has more mutations than expected}])))

(define mutant0-mod "mutant0.rkt")
(define mutant1-mod "mutant1.rkt")
(define mutant2-mod "mutant2.rkt")
(define mutant0-status (make-parameter 'running))
(define mutant1-status (make-parameter 'running))
(define mutant2-status (make-parameter 'running))
(define (empty-will f _) f)
(define mutant0 (mutant mutant0-mod 0 #t))
(define mutant0-proc (mutant-process mutant0
                                     (hash mutant0-mod 'types
                                           mutant1-mod 'none
                                           mutant2-mod 'none
                                           main.rkt 'none)
                                     mutant0-path
                                     (λ _ (mutant0-status))
                                     empty-will
                                     0
                                     'sample
                                     0
                                     #f))
(define mutant1 (mutant mutant1-mod 1 #t))
(define mutant1-proc/1 (mutant-process mutant1
                                       (hash mutant0-mod 'types
                                             mutant1-mod 'none
                                             mutant2-mod 'types
                                             main.rkt 'none)
                                       mutant1-path/1
                                       (λ _ (mutant1-status))
                                       empty-will
                                       1
                                       2
                                       0
                                       #f))
(define mutant1-proc/2 (mutant-process mutant1
                                       (hash mutant0-mod 'types
                                             mutant1-mod 'types
                                             mutant2-mod 'types
                                             main.rkt 'none)
                                       mutant1-path/2
                                       (λ _ (mutant1-status))
                                       empty-will
                                       2
                                       'sample
                                       0
                                       #f))

(define mutant2 (mutant mutant2-mod 2 #t))
(define mutant2-called? (make-parameter #f))
(define (mutant2-will a-factory dead-proc)
  (mutant2-called? #t)
  a-factory)
(define mutant2-proc (mutant-process mutant2
                                     (hash mutant0-mod 'none
                                           mutant1-mod 'none
                                           mutant2-mod 'none)
                                     mutant2-path
                                     (λ _ (mutant2-status))
                                     mutant2-will
                                     3
                                     'sample
                                     0
                                     #f))


(test-begin/with-env
 #:name mutant-results
 (ignore
  (define mutant1-proc/1-result (call-with-input-file mutant1-path/1 read)))

 (test-equal? (read-mutant-result mutant1-proc/1)
              mutant1-proc/1-result)

 (ignore
  (define aggregate-file (mutant-process-file mutant1-proc/2))
  (define aggregate-file-contents (call-with-input-file aggregate-file read))
  (define mutant1-proc/1-file (mutant-process-file mutant1-proc/1))
  (define mutant1-proc/1-file-contents
    (call-with-input-file mutant1-proc/1-file read))
  (define dead-mutant1-proc/1
    (dead-mutant-process mutant1
                         (mutant-process-config mutant1-proc/1)
                         mutant1-proc/1-result
                         (mutant-process-id mutant1-proc/1)
                         (mutant-process-blame-trail-id mutant1-proc/1)
                         #f))
  (append-mutant-result!
   dead-mutant1-proc/1
   (aggregate-mutant-result (mutant-process-mutant mutant1-proc/2)
                            (mutant-process-file mutant1-proc/2))))
 (test-equal?
  ;; ll: appending the file contents as opposed to the read-write
  ;; round trip causes an extra newline, so just add that to the expected output
  (call-with-input-file aggregate-file
    (λ (in) (list (read in) (read in))))
  (list aggregate-file-contents
        (aggregated-result (dead-mutant-process-blame-trail-id dead-mutant1-proc/1)
                           (dead-mutant-process-id dead-mutant1-proc/1)
                           mutant1-proc/1-file-contents
                           (dead-mutant-process-config dead-mutant1-proc/1)))))



(parameterize ([mutant2-called? #f])
  (test-begin/with-env
   #:name process-dead-mutant
   (ignore
    ;; aggregated result for mutant1/1
    (define mutant1-proc/1-result (call-with-input-file mutant1-path/1 read))
    ;; aggregated result for mutant2
    (define mutant2-proc-result (call-with-input-file mutant2-path read))
    (define aggregate-file (mutant-process-file mutant1-proc/2))
    (define aggregate-file-contents (file->value mutant1-path/2))
    ;; make the aggregate file be in the right state
    (call-with-output-file aggregate-file #:exists 'replace
      (λ (out) (writeln aggregate-file-contents out)))
    (define mutant1-aggregate (aggregate-mutant-result mutant1
                                                       aggregate-file))
    (define mutant1-proc/1-file (mutant-process-file mutant1-proc/1))
    (define mutant1-proc/1-file-contents
      (file->value mutant1-proc/1-file))
    (define mutant1-proc/1-file-contents+blame-trail
      (aggregated-result
       (mutant-process-blame-trail-id mutant1-proc/1)
       (mutant-process-id mutant1-proc/1)
       mutant1-proc/1-file-contents
       (mutant-process-config mutant1-proc/1)))
    (define mutant2-proc-file (mutant-process-file mutant2-proc))
    (define mutant2-proc-file-contents (file->value mutant2-proc-file))
    (define orig-results (hash mutant1
                               mutant1-aggregate
                               ;; extra garbage not relevant
                               mutant0
                               (aggregate-mutant-result mutant0
                                                        mutant0-path)))
    (define orig-factory (factory (bench-info (benchmark '() '() #f #f)
                                              (hash))
                                  orig-results
                                  (set mutant1-proc/1
                                       mutant2-proc)
                                  2
                                  (hash)
                                  5))

    (define new-factory/processed-mutant1-proc/1
      (parameterize ([mutant1-status 'done-ok])
        (process-dead-mutant orig-factory mutant1-proc/1)))
    (define new-factory/processed-mutant2
      (parameterize ([mutant2-status 'done-ok])
        (process-dead-mutant orig-factory mutant2-proc)))

    (define new-aggregate-file-contents
      (file->list aggregate-file)))

   (test-equal? new-aggregate-file-contents
                (list aggregate-file-contents
                      mutant1-proc/1-file-contents+blame-trail))
   ;; Due to consolidation, results map is unchanged
   (test-equal? (factory-results new-factory/processed-mutant1-proc/1)
                orig-results)
   ;; But the mutant's file is gone
   (not (file-exists? mutant1-proc/1-file))

   ;; Not so for mutant2, which wasn't in the results hash yet
   (test-equal? (factory-results new-factory/processed-mutant2)
                (hash mutant2
                      ;; The first proc for a mutant to die becomes the
                      ;; aggregate file
                      (aggregate-mutant-result
                       mutant2
                       (mutant-process-file mutant2-proc))

                      mutant1
                      mutant1-aggregate
                      ;; extra garbage not relevant
                      mutant0
                      (aggregate-mutant-result mutant0
                                               mutant0-path)))
   ;; and its file persists as the aggregate file
   (file-exists? mutant2-proc-file)
   ;; but it has its blame trail info included now
   (test-equal? (file->value mutant2-proc-file)
                (aggregated-result
                 ;; successful sample so id becomes blame trail
                 (mutant-process-id mutant2-proc)
                 (mutant-process-id mutant2-proc)
                 mutant2-proc-file-contents
                 (mutant-process-config mutant2-proc)))
   ;; and the will of mutant2 was executed
   (mutant2-called?)))


(define (with-log-message-counts thunk
          #:display-intercepted-msgs? [display-intercepted-msgs? #f])
  (define (make-box-hash keys init-val)
    (for/hash ([k (in-list keys)])
      (values k (box init-val))))
  (define message-counts (make-box-hash '(debug info warning error fatal) 0))
  (with-intercepted-logging
    #:logger factory-logger
    (match-lambda [(and (vector level _ _ _)
                        msg)
                   (when display-intercepted-msgs?
                     (displayln @~a{Intercepted: @~v[msg]}))
                   (define count-box
                     (hash-ref message-counts level))
                   (set-box! count-box (add1 (unbox count-box)))]
                  [other #f])
    thunk
    'debug)
  (for/hash ([(level count-box) (in-hash message-counts)])
    (values level (unbox count-box))))

(define-test (test-revival mutant-proc
                           mutant-status-param
                           mutant-status
                           expected-warnings
                           expect-exit?)
  (define exit-called? (box #f))

  (define the-factory
    (factory (bench-info (benchmark (list mutant2-mod
                                          mutant1-mod
                                          mutant0-mod
                                          main.rkt)
                                    (list mutant2-mod
                                          mutant1-mod
                                          mutant0-mod
                                          main.rkt)
                                    #f #f)
                         (hash))
             (hash)
             (set mutant-proc)
             1
             (hash)
             0))

  (define log-message-counts
    (with-log-message-counts
      (λ _
        (parameterize ([exit-handler (λ _ (set-box! exit-called? #t))]
                       [mutant-status-param mutant-status]
                       [process-limit 5]
                       [data-output-dir test-mutant-dir])
          (for/fold ([current-factory the-factory])
                    ([i (in-naturals)]
                     ;; ll: need some flexibility here so timing doesn't
                     ;; cause intermittent failures
                     #:break (or (> i 10)
                                 (unbox exit-called?)))
            (sleep 2)
            (sweep-dead-mutants current-factory))))))

  (define warning-count (hash-ref log-message-counts 'warning))
  (define error-msg? (not (zero? (hash-ref log-message-counts 'error))))

  (and/test/message [(test-= warning-count expected-warnings)
                     @~a{Not all warnings logged
                             expected @expected-warnings got @warning-count}]
                    [error-msg? "Error message not logged"]
                    [(test-equal? expect-exit? (unbox exit-called?))
                     "Abort call recorded does not match expected"]))
(test-begin/with-env
  #:name process-dead-mutant/revival/on-error
  (test-revival mutant0-proc mutant0-status 'done-error 3 #t))
(test-begin/with-env
 #:name process-dead-mutant/revival/no-output
 (test-revival (mutant-process mutant0
                               (hash mutant2-mod 'none
                                     mutant1-mod 'types
                                     mutant0-mod 'none
                                     main.rkt 'none)
                               empty-file-path
                               (λ _ (mutant0-status))
                               empty-will
                               0
                               'sample
                               0
                               #f)
               mutant0-status
               'done-ok
               4 ;; 3 revivals + final failure
               #t))

(test-begin/with-env
 #:name process-dead-mutant/revival/suppress-abort
 (parameterize ([abort-on-failure? #f])
   (test-revival (mutant-process mutant0
                                 (hash mutant2-mod 'none
                                       mutant1-mod 'types
                                       mutant0-mod 'none
                                       main.rkt 'none)
                                 empty-file-path
                                 (λ _ (mutant0-status))
                                 empty-will
                                 0
                                 'sample
                                 0
                                 #f)
                 mutant0-status
                 'done-ok
                 5 ;; 3 revival attempts + final failure + 1 ignored abort
                 #f)))



(define-test (test-blame-disappearing-handler
              outcome has-increased-limits?
              #:exit? expect-exit-called?
              #:respawn test-respawn-call
              #:logged test-logger)
  (define (make-dead-mutant result increased-limits?)
    (dead-mutant-process mutant1
                         (mutant-process-config mutant1-proc/1)
                         result
                         (mutant-process-id mutant1-proc/1)
                         (mutant-process-blame-trail-id mutant1-proc/1)
                         increased-limits?))
  (define orig-dead-mutant
    (make-dead-mutant
     (run-status mutant0-mod 0 'foo 'blamed mutant0-mod #f)
     #f))
  (define new-dead-mutant
    (make-dead-mutant (run-status mutant0-mod
                                  0
                                  'foo
                                  outcome
                                  (match outcome
                                    [(or 'blamed 'type-error)
                                     mutant0-mod]
                                    [else #f])
                                  #f)
                      has-increased-limits?))
  (define respawn-call (box #f))
  (define handler
    (make-blame-disappearing-handler orig-dead-mutant
                                     (file-name-string-from-path mutant1-path/1)
                                     (λ (f #:timeout/s timeout
                                           #:memory/gb memory)
                                       (set-box! respawn-call
                                                 (list timeout memory))
                                       f)))
  (define the-factory
    (factory (bench-info (benchmark (list mutant2-mod
                                          mutant1-mod
                                          mutant0-mod
                                          main.rkt)
                                    (list mutant2-mod
                                          mutant1-mod
                                          mutant0-mod
                                          main.rkt)
                                    #f #f)
                         (hash))
             (hash)
             (set)
             0
             (hash)
             0))
  (define exit-called? (box #f))
  (define logged
    (with-log-message-counts
      (thunk
       (parameterize ([exit-handler (λ _ (set-box! exit-called? #t))])
         (handler the-factory new-dead-mutant)))))

  (and/test (test-respawn-call (unbox respawn-call))
            (test-logger logged)
            (test-equal? (unbox exit-called?) expect-exit-called?)))

(test-begin/with-env
 #:name make-blame-disappearing-handler
 ;; a mutant that crashes while following blame trail is a clear bug
 (test-blame-disappearing-handler
  'crashed
  #f
  #:exit? #t
  #:respawn false?
  #:logged (λ (logged)
             (test-= (hash-ref logged 'error) 2)))

 ;; a mutant that times out or ooms should initially be retried with a warning
 (test-blame-disappearing-handler
  'timeout
  #f
  #:exit? #f
  #:respawn (λ (maybe-respawn-args)
              (test-equal? maybe-respawn-args
                           (list (* 2 (default-timeout/s))
                                 (* 2 (default-memory-limit/gb)))))
  #:logged (λ (logged)
             (and/test/message
              [(test-= (hash-ref logged 'info) 1)
               "info message count wrong"]
              [(test-= (hash-ref logged 'warning) 0)
               "warning message count wrong"]
              [(test-= (hash-ref logged 'error) 0)
               "error message count wrong"])))

 ;; a mutant that times out or ooms after have already been retried
 ;; should signal a warning and continue (without trying to respawn again)
 (test-blame-disappearing-handler
  'oom
  #t
  #:exit? #f
  #:respawn false?
  #:logged (λ (logged)
             (and/test/message
              [(test-= (hash-ref logged 'info) 0)
               "info message count wrong"]
              [(test-= (hash-ref logged 'warning) 1)
               "warning message count wrong"]
              [(test-= (hash-ref logged 'error) 0)
               "error message count wrong"])))
 (test-blame-disappearing-handler
  'timeout
  #t
  #:exit? #f
  #:respawn false?
  #:logged (λ (logged)
             (and/test/message
              [(test-= (hash-ref logged 'info) 0)
               "info message count wrong"]
              [(test-= (hash-ref logged 'warning) 1)
               "warning message count wrong"]
              [(test-= (hash-ref logged 'error) 0)
               "error message count wrong"]))))



(define orig-factory (factory (bench-info (benchmark (list e-path
                                                           main-path
                                                           loop-path)
                                                     (list e-path
                                                           main-path
                                                           loop-path)
                                                     #f #f)
                                          (hash e.rkt 'types
                                                main.rkt 'types
                                                loop.rkt 'types))
                              (hash)
                              (set mutant1-proc/1
                                   mutant2-proc)
                              2
                              (hash)
                              5))
(define e-proc-config (hash e.rkt 'types
                            main.rkt 'none
                            loop.rkt 'none))
(parameterize ([data-output-dir test-mutant-dir])
  (test-begin/with-env
   #:name spawn-mutant/simple
   (ignore
    (define new-factory (spawn-mutant orig-factory
                                      e.rkt
                                      0
                                      e-proc-config
                                      empty-will
                                      #:blame-trail-root? #t)))
   (test-= (set-count (factory-active-mutants new-factory))
           3)
   (test-= (factory-active-mutant-count new-factory)
           3)
   (test-= (factory-total-mutants-spawned new-factory)
           6)
   ;; spawning a mutant has no effect on the result map
   (test-equal? (factory-results new-factory)
                (factory-results orig-factory))
   ;; or the current bench
   (test-equal? (factory-bench new-factory)
                (factory-bench orig-factory))

   (ignore (match-define (list-no-order (== mutant1-proc/1)
                                        (== mutant2-proc)
                                        e-proc)
             (set->list (factory-active-mutants new-factory))))
   (test-match e-proc
               (mutant-process (mutant (== e.rkt) 0 #t)
                               (== e-proc-config)
                               _
                               _
                               (== empty-will)
                               _
                               'sample
                               0
                               #f)))
  (test-begin/with-env
   #:name spawn-mutant/trail-types
   (ignore
    (define-test (test-mutant-trail-type #:blame-trail-root? [bt-root? #f]
                                         #:following-trail [following #f]
                                         #:test-mutant? [test? #f])
      (define new-factory (spawn-mutant orig-factory
                                        e.rkt
                                        0
                                        e-proc-config
                                        empty-will
                                        #:blame-trail-root? bt-root?
                                        #:following-trail following
                                        #:test-mutant? test?))
      (match-define (list-no-order (== mutant1-proc/1)
                                   (== mutant2-proc)
                                   e-proc)
        (set->list (factory-active-mutants new-factory)))
      (define ctl (mutant-process-ctl e-proc))
      (ctl 'kill)
      (when (file-exists? (mutant-process-file e-proc))
        (delete-file (mutant-process-file e-proc)))
      (extend-test-message
       (test-equal? (mutant-process-blame-trail-id e-proc)
                    (cond [bt-root?     sample-flag]
                          [following => values]
                          [test?        test-mutant-flag]))
       @~a{Mutant spawned with @(cond [bt-root? '#:blame-trail-root?]
                                      [following '#:following-trail]
                                      [test? '#:test-mutant?]) has bad trail:})))
   (test-mutant-trail-type
    #:blame-trail-root? #t)
   (test-mutant-trail-type
    #:following-trail 42)
   (test-mutant-trail-type
    #:test-mutant? #t))
  (test-begin/with-env
   #:name spawn-mutant/extended-limits
   ;; Test that spawning a mutant with extended limits is recorded in
   ;; the new mutant-process
   (ignore
    (define new-factory* (spawn-mutant orig-factory
                                       e.rkt
                                       0
                                       e-proc-config
                                       empty-will
                                       #:timeout/s 5
                                       #:memory/gb 30
                                       #:following-trail 42))
    (match-define (list-no-order (== mutant1-proc/1)
                                 (== mutant2-proc)
                                 e-proc*)
      (set->list (factory-active-mutants new-factory*))))
   (test-match e-proc*
               (mutant-process (mutant (== e.rkt) 0 #t)
                               (== e-proc-config)
                               _
                               _
                               (== empty-will)
                               _
                               42
                               0
                               #t)))

  (test-begin/with-env
   #:name spawn-mutant/idle
   ;; Test that an idling factory with zombies logs error messages,
   ;; and at a reasonable rate
   (ignore
    (define logged-messages
      (parameterize ([mutant1-status 'running]
                     [mutant2-status 'running]
                     [process-limit 2]
                     [default-timeout/s 1]
                     [abort-on-failure? #f]
                     [sweep-delay-seconds 2]
                     [sweep-retry-limit 2])
        (with-log-message-counts
          (thunk
           (define thd
             (thread (thunk (spawn-mutant orig-factory
                                          e.rkt
                                          0
                                          e-proc-config
                                          empty-will
                                          #:following-trail 42))))
           (sleep (* (sweep-delay-seconds) (sweep-retry-limit) 2))
           (kill-thread thd))))))
   (test-match logged-messages
               ;; ll: need some flexibility here so timing doesn't
               ;; cause intermittent failures.
               ;; Should happen at least once, at most twice.
               (hash-table ['error (? (and/c number? (between/c 1 2)))]
                           ['warning 0]
                           _ ___))))


(test-begin/with-env
 #:name maybe-spawn-configured-mutants
 (ignore
  (define final-factory (box #f))
  (define logged
    (parameterize ([data-output-dir test-mutant-dir]
                   [default-timeout/s 1]
                   [abort-on-failure? #f])
      (with-log-message-counts
        #:display-intercepted-msgs? #f
        (thunk
         (define fact
           (maybe-spawn-configured-mutants
            (copy-factory orig-factory
                          [active-mutants (set)]
                          [active-mutant-count 0])
            (mutant
             loop.rkt
             0
             #t)))
         (set-box! final-factory
                   (for/fold ([current-factory fact])
                             ;; first sweep: retry, second: report failure
                             ;; add a third just in case
                             ([i (in-range 3)])
                     ;; it seems to take around 6 seconds on my machine to
                     ;; detect and sweep a mutant timing out
                     (sleep (* 6 (+ (default-timeout/s) (sweep-delay-seconds))))
                     (sweep-dead-mutants current-factory))))))))
 (test-= (hash-ref logged 'warning) 1) ;; warning about giving up on mutant
 (test-match (unbox final-factory)
             (struct* factory ([active-mutants (? set-empty?)]))))



(require racket/os)
;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit (match (gethostname)
                                [(regexp "quser[0-9]+") 1]
                                [else 4])]
               [sample-size (match (gethostname)
                                [(regexp "quser[0-9]+") 3]
                                [else 10])]
               [abort-on-failure? #f]
               [default-timeout/s (match (gethostname)
                                    [(regexp "quser[0-9]+") 20]
                                    [else 10])]
               [default-memory-limit/gb 1])
  (displayln @~a{



                 Running full run test. This will take a while.
                 })
  (test-begin/with-env
   #:name test:full-run

   ;; Mutations expected:
   ;; - Mutations: 8
   ;;   - main.rkt: 6
   ;;     - foo: negate cond (@0), - to + (@1), * to / (@2), 3 to 4 (@3)
   ;;     - main: swap bar args (@4), 2 to 3 (@5)
   ;;   - second.rkt: 2
   ;;     - bar: negate cond (@0), - to + (@1)
   ;; - Mutations that make type errors: 3
   ;;   - main.rkt: 2
   ;;     - foo: * to /
   ;;     - main: swap bar args
   ;;   - second.rkt: 1
   ;;     - bar: negate cond, - to +
   ;;
   ;; - Expect 4 decisions to sample:
   ;;   - main.rkt @ 2, main.rkt @ 4
   ;;   - second.rkt @ 0, second.rkt @ 1
   ;; - Expect (sample-size) samples for each
   ;; - Expect 8 total mutant files
   (ignore
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
    (define mutant-results (run-all-mutants*configs bench))
    (displayln @~a{Mutant errors: @(file->string error-log)})
    ;; (displayln "Data:")
    ;; (for ([{m aggregate-result} (in-hash mutant-results)])
    ;;   (displayln
    ;;    @~a{Mutant file @m contents:
    ;;               @(pretty-format
    ;;                 (file->list
    ;;                  (aggregate-mutant-result-file aggregate-result)))


    ;;               }))
    )
   (test-= (length (directory-list test-mutant-dir))
           8)
   (for/and/test ([f (in-directory test-mutant-dir)])
                 (not/test (test-= (file-size f) 0)))

   (ignore
    (define data
      (for/hash ([f (in-directory test-mutant-dir)])
        (values (file-name-string-from-path f)
                (for/list ([line (file->lines f)])
                  (call-with-input-string line read)))))
    (define (mutant-file-pattern mod-name mutation-index)
      @~a{@|mod-name|_index@|mutation-index|_[0-9]+\.rktd})
    (define (find-mutant-file mod-name mutation-index)
      (findf (λ (f) (regexp-match? (mutant-file-pattern mod-name mutation-index)
                                   f))
             (hash-keys data)))
    (define (count-blame-trails mutant-file)
      (define parts (hash-ref data mutant-file))
      (define blame-trail-ids
        (remove-duplicates
         (filter-map (match-lambda
                       [(aggregated-result
                         (? natural? bt-id)
                         (? natural? id)
                         (struct* run-status
                                  ([outcome (or 'type-error 'blamed)]
                                   [blamed (or (== "main.rkt")
                                               (== "second.rkt"))]))
                         _)
                        bt-id]
                       [(or (aggregated-result
                             (? symbol?)
                             (? natural? id)
                             (struct* run-status
                                      ([outcome (not (or 'type-error 'blamed))]))
                             _)
                            (aggregated-result
                             (== test-mutant-flag)
                             (? natural? id)
                             (? run-status?)
                             _))
                        #f])
                     parts)))
      (length blame-trail-ids)))

   (for/and/test
    ([mutant-info (in-list '(("main.rkt" 2)
                             ("main.rkt" 4)
                             ("second.rkt" 0)
                             ("second.rkt" 1)))])
    (define mutant-file (find-mutant-file (first mutant-info)
                                          (second mutant-info)))
    (and/test/message
     [mutant-file @~a{@mutant-file doesn't exist}]
     [(test->= (length (hash-ref data mutant-file))
               ;; add1 for the mutant validity test run
               (add1 (sample-size)))
      @~a{@mutant-file doesn't contain at least `(sample-size)` samples:}]
     [(test-= (count-blame-trails mutant-file)
              (sample-size))
      @~a{@mutant-file doesn't contain `(sample-size)` blame trails:}]))

   ;; check that mixed configurations work as expected:
   ;; this is just one mixed config that I know should produce a ctc violation
   (test-match (hash-ref data (find-mutant-file "main.rkt" 2))
               (list-no-order
                (struct* aggregated-result
                         ([config
                           (== (hash "main.rkt" 'none
                                     "second.rkt" 'types))]
                          [run-status
                           (struct* run-status
                                    ([outcome 'blamed]
                                     [blamed "main.rkt"]))]))
                _ ___))

   (test-match data
               (hash-table [_ (list
                               (struct* aggregated-result
                                        ([run-status
                                          (struct* run-status
                                                   ([outcome
                                                     (or 'type-error
                                                         'blamed
                                                         'completed
                                                         'oom
                                                         'timeout)]))]))
                               ___)] ___))))


;; (display-test-results)
