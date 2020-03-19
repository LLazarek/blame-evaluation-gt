#lang at-exp racket

(module test-helper racket
  (require ruinit
           syntax/parse/define
           "../runner/mutation-runner.rkt"
           (only-in (submod "mutant-factory.rkt" test)
                    mutant-error-log))
  (provide test-begin/with-env)

  (define-test-env (setup-test-env! cleanup-test-env!)
    #:directories ([test-mutant-dir (simplify-path "./test-mutants")]
                   [test-bench (simplify-path "./test-bench")]

                   [realistic-test-bench (simplify-path "./real-test-bench")]
                   [realistic-test-bench/ut (simplify-path "./real-test-bench/untyped")]
                   [realistic-test-bench/t (simplify-path "./real-test-bench/typed")])
    #:files ([e-path (build-path test-bench "e.rkt")
                     @~a{
                         #lang racket

                         (provide baz)

                         (define (baz x y)
                           (if (even? x)
                               y
                               (/ y x)))
                         }]
             [main-path (build-path test-bench "main.rkt")
                     @~a{
                         #lang typed/racket

                         (require "e.rkt" "loop.rkt")

                         (: foo (-> Number Number))
                         (define (foo x)
                           (- x))

                         (loop)
                         (foo (baz 0 22))
                         }]
             [loop-path (build-path test-bench "loop.rkt")
                        @~a{
                            #lang typed/racket
                            (provide loop)
                            (: loop (-> Number))
                            (define (loop)
                              (if #f (loop) 42))
                            }]
             [mutant0-path "m0.rktd"
                           (format "~s\n"
                                   (run-status #f
                                               'crashed
                                               "some-error"
                                               "m0.rkt"
                                               'foo
                                               0))]
             [mutant1-path/1 "m11.rktd"
                             (format "~s\n"
                                     (run-status #f
                                                 'blamed
                                                 "./mutant-factory-test.rkt"
                                                 "m1.rkt"
                                                 'foo
                                                 0))]
             [mutant1-path/2 "m12.rktd"
                             (format "~s\n"
                                     (run-status #f
                                                 'type-error
                                                 "m1.rkt"
                                                 "m1.rkt"
                                                 'foo
                                                 42))]
             [mutant2-path "m2.rktd"
                           (format "~s\n"
                                   (run-status #f
                                               'type-error
                                               "m2.rkt"
                                               "m1.kt"
                                               'foo
                                               42))]
             [empty-file-path "empty-file.rktd"
                              ""]
             [error-log (mutant-error-log)
                        ""]

             [rt-main/ut (build-path realistic-test-bench/ut
                                     "main.rkt")
                         @~a{
                             #lang racket
                             (require "second.rkt")
                             (define (foo x)
                               (if (positive? x)
                                   (foo (- x))
                                   (* x x)))
                             (define (main)
                               (bar (foo 2)))
                             }]
             [rt-second/ut (build-path realistic-test-bench/ut
                                       "second.rkt")
                           @~a{
                               #lang racket
                               (define (bar x)
                                 (- x))
                               }]
             [rt-main/t (build-path realistic-test-bench/t
                                     "main.rkt")
                         @~a{
                             #lang typed/racket
                             (require "second.rkt")
                             (: foo (-> Integer Integer))
                             (define (foo x)
                               (if (positive? x)
                                   (foo (- x))
                                   (* x x)))
                             (: main (-> Integer))
                             (define (main)
                               (bar (foo 2)))
                             }]
             [rt-second/t (build-path realistic-test-bench/t
                                       "second.rkt")
                           @~a{
                               #lang typed/racket
                               (: bar (-> Integer Integer))
                               (define (bar x)
                                 (- x))
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
                       (run-status #f
                                   'crashed
                                   #f
                                   e.rkt
                                   'baz
                                   0)
                       42
                       'no-blame
                       #f))
(define dead-e-proc/completed
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status #f
                                   'completed
                                   #f
                                   e.rkt
                                   'baz
                                   0)]))
(define dead-e-proc/blame-e
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status #f
                                   'blamed
                                   e.rkt
                                   e.rkt
                                   'baz
                                   0)]
               [blame-trail-id 42]))
(define dead-e-proc/type-error-in-d
  (struct-copy dead-mutant-process
               dead-e-proc/crashed
               [result (run-status #f
                                   'type-error
                                   main.rkt
                                   main.rkt
                                   'baz
                                   0)]))
(test-begin
 #:name dead-process-blame/type-errors
 (not/test (try-get-blamed dead-e-proc/crashed))
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
              'crashed)
 (test-equal? (process-outcome dead-e-proc/completed)
              'completed)
 (test-equal? (process-outcome dead-e-proc/blame-e)
              'blamed)
 (test-equal? (process-outcome dead-e-proc/type-error-in-d)
              'type-error))

(test-begin/with-env
 #:name test:max-mutation-index-exceeded?

 (for/and/test ([i (in-range 3)])
               (not (max-mutation-index-exceeded? e-path i)))
 (max-mutation-index-exceeded? e-path 3)

 (not (max-mutation-index-exceeded? main-path 0))
 (max-mutation-index-exceeded? main-path 1))

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
                                     'no-blame
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
                                       'no-blame
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
                                     'no-blame
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
   (dead-mutant-process-blame-trail-id dead-mutant1-proc/1)
   (dead-mutant-process-result dead-mutant1-proc/1)
   (aggregate-mutant-result (mutant-process-mutant mutant1-proc/2)
                            (mutant-process-file mutant1-proc/2))))
 (test-equal?
  ;; ll: appending the file contents as opposed to the read-write
  ;; round trip causes an extra newline, so just add that to the expected output
  (call-with-input-file aggregate-file
    (λ (in) (list (read in) (read in))))
  (list aggregate-file-contents
        (cons (dead-mutant-process-blame-trail-id dead-mutant1-proc/1)
              mutant1-proc/1-file-contents))))



(parameterize ([mutant2-called? #f])
  (test-begin/with-env
   #:name process-dead-mutant
   (ignore
    (define mutant1-proc/1-result (call-with-input-file mutant1-path/1 read))
    (define mutant2-proc-result (call-with-input-file mutant2-path read))
    (define aggregate-file (mutant-process-file mutant1-proc/2))
    (define aggregate-file-contents
      (cons (mutant-process-id mutant1-proc/2)
            (call-with-input-file aggregate-file read)))
    ;; make the aggregate file be in the right state
    (call-with-output-file aggregate-file #:exists 'replace
      (λ (out) (writeln aggregate-file-contents out)))
    (define mutant1-aggregate (aggregate-mutant-result mutant1
                                                       aggregate-file))
    (define mutant1-proc/1-file (mutant-process-file mutant1-proc/1))
    (define mutant1-proc/1-file-contents
      (call-with-input-file mutant1-proc/1-file read))
    (define mutant1-proc/1-file-contents+blame-trail
      (cons (mutant-process-id mutant1-proc/2)
            mutant1-proc/1-file-contents))
    (define mutant2-proc-file (mutant-process-file mutant2-proc))
    (define mutant2-proc-file-contents (call-with-input-file mutant2-proc-file read))
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
      (call-with-input-file aggregate-file
        (λ (in) (list (read in) (read in))))))

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
   (test-equal? (call-with-input-file mutant2-proc-file read)
                (cons 'no-blame mutant2-proc-file-contents))
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
  (define fatal-msg? (not (zero? (hash-ref log-message-counts 'fatal))))

  (and/test/message [(test-= warning-count expected-warnings)
                     "Not all warnings logged"]
                    [fatal-msg? "Fatal message not logged"]
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
                               'no-blame
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
                                 'no-blame
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
     (run-status #f 'blamed mutant0-mod mutant0-mod 'foo 0)
      #f))
  (define new-dead-mutant
    (make-dead-mutant (run-status #f
                                  outcome
                                  (match outcome
                                    [(or 'blamed 'type-error)
                                     mutant0-mod]
                                    [else #f])
                                  mutant0-mod
                                  'foo
                                  0)
                      has-increased-limits?))
  (define respawn-call (box #f))
  (define handler
    (make-blame-disappearing-handler orig-dead-mutant
                                     mutant1-path/1
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
             (test-= (hash-ref logged 'fatal) 2)))

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
              [(test-= (hash-ref logged 'fatal) 0)
               "fatal message count wrong"])))

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
              [(test-= (hash-ref logged 'fatal) 0)
               "fatal message count wrong"])))
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
              [(test-= (hash-ref logged 'fatal) 0)
               "fatal message count wrong"]))))



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
                                      empty-will)))
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
                               'no-blame
                               0
                               #f)))
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
                                       #:memory/gb 30))
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
                               'no-blame
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
                     [abort-on-failure? #f])
        (with-log-message-counts
          (thunk
           (define thd
             (thread (thunk (spawn-mutant orig-factory
                                          e.rkt
                                          0
                                          e-proc-config
                                          empty-will))))
           (sleep 10)
           (kill-thread thd))))))
   (test-match logged-messages
               ;; ll: need some flexibility here so timing doesn't
               ;; cause intermittent failures
               (hash-table ['error (? (and/c number? (>=/c 1) (<=/c 3)))]
                           ['fatal 0]
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
        #:display-intercepted-msgs? #t
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
                             ([i (in-range 3)])
                     ;; triple the max limit that should be imposed
                     (sleep 6)
                     (sweep-dead-mutants current-factory))))))))
 (test-= (hash-ref logged 'warning) 1) ;; warning about giving up on mutant
 (test-match (unbox final-factory)
             (struct* factory ([active-mutants (? set-empty?)]))))



;; Full run test
(parameterize ([data-output-dir test-mutant-dir]
               [process-limit 4]
               [sample-size 10]
               [abort-on-failure? #f]
               [default-timeout/s 10]
               [default-memory-limit/gb 1])
  (define test-mutant-total-mutation-count 6)
  (test-begin/with-env
   #:name test:full-run

   (ignore (define mutant-results (run-all-mutants*configs
                                   (read-benchmark realistic-test-bench ))))
   (test-= (length (directory-list test-mutant-dir))
           test-mutant-total-mutation-count)
   (for/and/test ([f (in-directory test-mutant-dir)])
                 (not/test (test-= (file-size f) 0)))

   (ignore
    (define data
      (for/fold ([data empty])
                ([f (in-directory test-mutant-dir)])
        (append data
                (for/list ([line (file->lines f)])
                  (call-with-input-string line read))))))

   ;; 3 for the 3 mutations of e.rkt, none of which cause blame
   ;; 2 for the 2 mutations of loop.rkt, none of which cause blame
   ;; 1*(sample-size) for the 1 mutation of main.rkt that causes blame
   (test->= (length data) (+ 3 2 (sample-size)))
   (test-match data
               (list
                (cons (or (? natural?) 'no-blame)
                      (struct* run-status
                               ([outcome (or 'type-error
                                             'blamed
                                             'completed
                                             'crashed
                                             'timeout
                                             'oom)]
                                [blamed (or (or (== main.rkt)
                                                (== e.rkt)
                                                (== loop.rkt))
                                            #f)]
                                [mutated-module (or (== main.rkt)
                                                    (== e.rkt)
                                                    (== loop.rkt))]
                                [index (? natural?)])))
                ___))))


;; (display-test-results)
