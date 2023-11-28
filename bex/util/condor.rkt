#lang at-exp racket

(provide current-run-with-condor-machines
         spawn-condor-mutant-runner)

(require racket/runtime-path
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "experiment-exns.rkt")

(define-runtime-path default-script-dir "../../../tmp")
(define this-racket-exe-path (simple-form-path (find-system-path 'exec-file)))

(define (getenv/default env-var default [transform values])
  (or (and (getenv env-var)
           (transform (getenv env-var)))
      default))
(define current-run-with-condor-machines
  (make-parameter (getenv/default "BEX_CONDOR_MACHINES" #f string-split)))
(define batch-size
  (make-parameter (getenv/default "BEX_CONDOR_BATCH_SIZE" 1 string->number)))
(define max-batch-waiting-time
  (make-parameter (getenv/default "BEX_CONDOR_BATCH_WAIT" 5 string->number)))
(define batch-temp-loc
  (make-parameter (getenv/default "BEX_CONDOR_BATCH_SCRIPT_DIR" default-script-dir)))

(define (make-system-executor exe-name)
  (define exe (find-executable-path exe-name))
  (λ args (apply system* exe args)))

(define current-condor_submit
  (make-parameter (make-system-executor "condor_submit")))
(define current-condor_q
  (make-parameter (make-system-executor "condor_q")))
(define current-condor_rm
  (make-parameter (make-system-executor "condor_rm")))

(define condor_q-cache-expiration-secs 1)

;; inexact? -> (or/c (listof job-id-string?) #f)
(define get-condor-job-info
  ;; To avoid polling condor too frequently, and parsing/reparsing the same
  ;; output, cache the results and only refresh periodically.
  ;;
  ;; This creates a tricky race condition tho: we might launch a condor job,
  ;; and then try to check its status before we've refreshed the cache.
  ;; Then we'd think that the job is finished when it isn't actually.
  ;; So we need to pass along a kind of freshness limiter that means
  ;; "only give me results at least as old as this timestamp";
  ;; otherwise, we return #f with the interpretation "come back later".
  ;;
  ;; An alternative design might just refresh the cache if the limiter is too
  ;; recent, but I'd rather wait a second extra than possibly overload condor.
  (let ([last-poll-time (current-inexact-monotonic-milliseconds)]
        [cached-output #f])
    (λ (min-time)
     (cond [(or (not cached-output)
                (> (- (current-inexact-monotonic-milliseconds)
                      last-poll-time)
                   (* condor_q-cache-expiration-secs 1000)))
            (define condor-dump
              (with-output-to-string (thunk ((current-condor_q)))))
            (set! cached-output
                  (and (regexp-match? "-- Schedd: peroni.cs.northwestern.edu" condor-dump)
                       (map first
                            (regexp-match* @pregexp{[1_]\s+[1_]\s+1 (\d{7}\.0)}
                                           condor-dump
                                           #:match-select rest))))
            cached-output]
           [(< last-poll-time min-time) #f]
           [else cached-output]))))

(struct mutant-run-info (args ; (listof string?)
                         timeout/s ; natural?
                         outfile ; path-string?
                         error-file ; path-string?
                         )
  #:transparent)

(struct batch-condor-job (id ; string?
                          script ; (or/c path-to-existant-file? #f)
                          submit-time ; inexact? from (current-inexact-monotonic-milliseconds)
                          )
  #:transparent)

(define batcher-thd
  (delay
    (let ([main-thd (current-thread)])
      (thread
       (thunk
        (local-require data/queue)
        (define q (make-queue))
        (define (queue-take-batch!)
          (for/list ([i (min (batch-size) (queue-length q))])
            (dequeue! q)))
        (define (new-timeout)
          (thread (thunk (sleep (max-batch-waiting-time)))))
        (let loop ([submission-timeout never-evt]
                   [current-batch-condor-job-id-box (box #f)])
          (sync (handle-evt (choice-evt (if (>= (queue-length q) (batch-size))
                                            always-evt
                                            never-evt)
                                        submission-timeout)
                            (λ _
                              (define condor-id+script (submit-condor-job! (queue-take-batch!)))
                              (set-box! current-batch-condor-job-id-box condor-id+script)
                              (loop (if (queue-empty? q) never-evt (new-timeout))
                                    (box #f))))
                (handle-evt (thread-receive-evt)
                            (λ _
                              (enqueue! q (thread-receive))
                              (thread-send main-thd current-batch-condor-job-id-box)
                              (loop (if (thread? submission-timeout)
                                        submission-timeout
                                        (new-timeout))
                                    current-batch-condor-job-id-box))))))))))

(define (enqueue-batched-mutant! a-mutant-run-info)
  (thread-send (force batcher-thd)
               a-mutant-run-info)
  (thread-receive))

(define (submit-condor-job! submissions)
  (unless (directory-exists? (batch-temp-loc))
    (make-directory* (batch-temp-loc)))
  (define script (make-temporary-file "batch-~a.sh"
                                      #:base-dir (batch-temp-loc)))
  (with-output-to-file script
    #:exists 'replace
    (thunk
     (displayln "#!/bin/bash\n")
     (for ([info (in-list submissions)])
       (match-define (mutant-run-info args timeout outfile error-file) info)
       ;; timeout+30: we want a `timeout` result rather than "user break"
       (displayln @~a{
                      timeout -k 5 @(inexact->exact (+ (round timeout) 30)) @;
                      '@this-racket-exe-path' @(string-join (map ~s args) " ") @;
                      > @(simple-form-path outfile) @;
                      2> @(simple-form-path error-file)

                      }))
     ;; workaround: there's some kind of race condition with jobs finishing and
     ;; their results not being written to disk yet
     (displayln "\nsleep 1s\n")))
  (file-or-directory-permissions script 511)
  (batch-condor-job
   (condor-submit!
    @~a{
        # Set the universe
        Universe = vanilla

        # Describe the target machine
        Requirements = (@(string-join (for/list ([name (in-list (current-run-with-condor-machines))])
                                        @~a{(Machine == "@|name|.cs.northwestern.edu")})
                                      " || "))

        Rank = TARGET.Mips
        Copy_To_Spool = False

        # Notification
        Notification = never

        # Set the environment
        Getenv = True

        Arguments = ""
        Executable = @(simple-form-path script)
        Error = @(build-path (batch-temp-loc) "batched-condor-script-errs.txt")
        Output = @(build-path (batch-temp-loc) "batched-condor-script-outs.txt")
        Log = condor-log.txt

        +IsWholeMachineJob = false
        +IsSuspensionJob = false

        Queue

        })
   script
   (current-inexact-monotonic-milliseconds)))

(define (spawn-condor-mutant-runner a-benchmark-configuration
                                    module-to-mutate
                                    mutation-index
                                    outfile
                                    config-path

                                    mutant-runner-path
                                    mutant-error-log
                                    #:timeout/s timeout/s
                                    #:memory/gb memory/gb
                                    #:log-mutation-info? [log-mutation-info? #f]
                                    #:save-output [output-path #f]

                                    #:write-modules-to [dump-dir-path #f]
                                    #:force-module-write? [force-module-write? #f])
  (define args
    (flatten (list
              (if log-mutation-info?
                  (list "-O" "info@mutate")
                  empty)
              "--"
              (~a mutant-runner-path)
              "-b" (serialize-benchmark-configuration a-benchmark-configuration)
              "-M" (~a module-to-mutate)
              "-i" (~a mutation-index)
              "-t" (~a timeout/s)
              "-g" (~a memory/gb)
              "-c" (~a (simple-form-path config-path))
              (if output-path
                  (list "-O" output-path)
                  empty)
              (if dump-dir-path
                  (list "-w" dump-dir-path)
                  empty)
              (if force-module-write?
                  '("-f")
                  empty))))
  (define id-box
    (enqueue-batched-mutant! (mutant-run-info args
                                              timeout/s
                                              outfile
                                              mutant-error-log)))
  (define (get-proc-status)
    (define id (unbox id-box))
    (match id
      [#f 'running] ; waiting in batcher queue
      [(batch-condor-job condor-id
                         script-path
                         (and launch-time (app get-condor-job-info maybe-condor-job-info)))
       (cond [(or (false? maybe-condor-job-info) ; too early
                  (member condor-id maybe-condor-job-info))
              'running]
             [else
              ;; Clean up the script now that it's done
              (when (path-string? script-path)
                (delete-file script-path)
                (set-box! id-box (batch-condor-job condor-id #f launch-time)))
              'done-ok])]))
  (match-lambda ['status
                 (get-proc-status)]
                ['kill
                 (when (unbox id-box)
                   (void (with-output-to-string
                           (thunk ((current-condor_rm) (batch-condor-job-id (unbox id-box)))))))]
                ['wait
                 (let loop ()
                   (when (equal? (get-proc-status) 'running)
                     (sleep 1)
                     (loop)))]
                [other (raise-internal-experiment-error
                        'spawn-mutant-runner
                        @~a{unimplemented process ctl for: @other})]))

;; string? -> string?
;; Submit `job-file-contents` as a job file to condor, parse and return the resulting job id.
(define (condor-submit! job-file-contents)
  (match (with-output-to-string
           (thunk
            (parameterize ([current-error-port (current-output-port)])
              (with-input-from-string job-file-contents
                (thunk ((current-condor_submit) "-verbose" "-"))))))
    [(regexp @pregexp{\*\* Proc ([\d.]+):} (list _ id))
     id]
    [something-else
     (raise-internal-experiment-error
      'spawn-mutant-runner
      @~a{
          failed to submit condor job, it said:
          @something-else
          })]))

(module+ test
  (require ruinit)
  (define-test-env {setup-test-env! cleanup-test-env!}
    #:directories ([test-bench "./test-mods"]
                   [ut "./test-mods/untyped"]
                   [t "./test-mods/typed"]
                   [b "./test-mods/base"])
    #:files ([main.rkt (build-path ut "main.rkt")
                    @~a{
                        #lang racket
                        (define x (λ () (/ 5 1) "2" 0))
                        (x)
                        (define (main)
                          (string-append "a" "b"))
                        (main)
                        }]
             [tmain.rkt (build-path t "main.rkt")
                     ""]))
  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (define-runtime-path mutant-runner-path "../experiment/mutant-runner.rkt")
  (install-configuration! test-config)
  (let ()
    (define delay-time 5)
    (define condor-subs empty)
    (define job-id "1234567.0")
    (define the-job
      @~a{foo0123 @job-id     1            _     _      1     _     1 @job-id})
    (define current-jobs "")
    (parameterize ([current-condor_submit (λ args
                                            (set! condor-subs (cons (port->string (current-input-port))
                                                                    condor-subs))
                                            (displayln @~a{
                                                           trash
                                                           ** Proc @|job-id|: 1
                                                           more trash
                                                           })
                                            #t)]
                   [current-condor_q
                    (λ args
                      (displayln
                       @~a{


                           -- Schedd: peroni.cs.northwestern.edu : <129.105.44.178:9618?...  11/20/23 13:43:55
                           OWNER BATCH_NAME      SUBMITTED   DONE   RUN    IDLE   HOLD  TOTAL JOB_IDS
                           @current-jobs

                           Total for query: 0 jobs; 0 completed, 0 removed, 0 idle, 0 running, 0 held, 0 suspended 
                           Total for foo0123: 0 jobs; 0 completed, 0 removed, 0 idle, 0 running, 0 held, 0 suspended 
                           Total for all users: 23 jobs; 0 completed, 0 removed, 1 idle, 22 running, 0 held, 0 suspended

                           }))]
                   [batch-size 2]
                   [current-run-with-condor-machines '("fix")]
                   [max-batch-waiting-time delay-time])
      (test-begin
        #:before (setup-test-env!)
        #:after (cleanup-test-env!)
        (ignore (define b (read-benchmark test-bench))
                (define bc (configure-benchmark b (hash "main.rkt" 'none)))
                (define out (build-path test-bench "out.txt"))
                (define ctl (spawn-condor-mutant-runner bc
                                                        "main.rkt"
                                                        0
                                                        out
                                                        test-config

                                                        mutant-runner-path
                                                        (build-path test-bench "errs.txt")
                                                        #:timeout/s 10
                                                        #:memory/gb 1)))
        (empty? condor-subs)
        (test-equal? (ctl 'status) 'running)
        (ignore (sleep (+ delay-time 1)))
        (test-= (length condor-subs) 1)
        (ignore (set! current-jobs the-job))
        (test-equal? (ctl 'status) 'running)
        (ignore (sleep condor_q-cache-expiration-secs)
                (set! current-jobs ""))
        (test-equal? (ctl 'status) 'done-ok)
        (ignore (set! condor-subs empty))


        (ignore (define ctls
                  (for/list ([i 5])
                    (spawn-condor-mutant-runner bc
                                                "main.rkt"
                                                0
                                                out
                                                test-config

                                                mutant-runner-path
                                                (build-path test-bench "errs.txt")
                                                #:timeout/s 10
                                                #:memory/gb 1))))
        (test-= (length condor-subs) 2)
        ;; because the first two jobs have been submitted but condor_q doesn't mention them yet, they should report being done
        ;; the last one hasn't been submitted yet tho, so it reports running
        (test-equal? (map (λ (c) (c 'status)) ctls) '(done-ok done-ok done-ok done-ok running))
        (ignore (sleep condor_q-cache-expiration-secs)
                (set! current-jobs the-job))
        ;; now condor_q does mention them
        (test-equal? (map (λ (c) (c 'status)) ctls) '(running running running running running))
        (ignore (sleep (+ delay-time 1)))
        (test-= (length condor-subs) 3)
        (test-equal? (map (λ (c) (c 'status)) ctls) '(running running running running running))
        (ignore (sleep condor_q-cache-expiration-secs)
                (set! current-jobs ""))
        (test-equal? (map (λ (c) (c 'status)) ctls) '(done-ok done-ok done-ok done-ok done-ok))
        ))))
