#lang racket

;; lltodo: consider using sandbox evaluators?

(provide (contract-out
          [run-with-limits
           ({(-> any)}
            {#:timeout/s (or/c #f (and/c real? (>=/c 0)))
             #:memory/gb (or/c #f (and/c real? (>=/c 0)))
             #:suppress-output? boolean?
             #:oom-result (-> any)
             #:timeout-result (-> any)
             #:exn-handler (any/c . -> . any)}
            . ->* .
            any)]))

(require racket/async-channel)


(struct result (type value) #:transparent)

(define ((make-async-outcome-reporter thunk outcome-channel))
  (define res
    (with-handlers ([(const #t) (λ (e) (result 'exn e))])
      (result 'normal (thunk))))
  (async-channel-put outcome-channel res))

(define (run-with-limits thunk
                         #:timeout/s [timeout-secs #f]
                         #:memory/gb [memory-limit-gb #f]
                         #:suppress-output? [suppress-output? #f]
                         #:oom-result [make-oom-result (λ () 'oom)]
                         #:timeout-result [make-timeout-result (λ () 'timeout)]
                         #:exn-handler [exn-handler (λ (e) (raise e))])
  (define run-custodian (make-custodian))
  (define shutdown-box
    (make-custodian-box run-custodian 'available))
  (when memory-limit-gb
    (custodian-limit-memory run-custodian
                            (exact-floor (* memory-limit-gb (expt 10 9)))))
  (define run
    (λ _
      (define /dev/null (open-output-nowhere))
      (parameterize ([current-output-port (if suppress-output?
                                              /dev/null
                                              (current-output-port))]
                     [current-error-port (if suppress-output?
                                             /dev/null
                                             (current-error-port))])
        (begin0 (thunk)
          (close-output-port /dev/null)))))
  ;; Channel for communicating result of the thunk
  (define outcome-channel (make-async-channel 1))
  (define thread-id
    ;; Threads inheret the current-custodian upon creation
    (parameterize ([current-custodian run-custodian])
        (thread (make-async-outcome-reporter run outcome-channel))))
  (define ended-event
    (sync/timeout/enable-break timeout-secs
                               thread-id
                               shutdown-box))
  (unless ended-event
    (kill-thread thread-id))
  ;; Sync to clean up the killed thread
  (sync/enable-break thread-id shutdown-box)

  ;; Force garbage collection so repeated oom runs don't cause memory exhaustion
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)

  (cond
    ;; Check if oom or timed out
    [(not (custodian-box-value shutdown-box))
     (make-oom-result)]
    [(not ended-event)
     (make-timeout-result)]
    ;; if not, and it completed normally, get the result of the thunk
    [(async-channel-try-get outcome-channel)
     =>
     (match-lambda [(result 'normal v) v]
                   [(result 'exn e) (exn-handler e)])]
    [else
     (raise-user-error
      'run-with-limits
      "Unable to extract result from thunk. Probably it does things I didn't expect.")]))

(module+ test
  (require ruinit)

  (test-begin
    #:name exn-in-thunk
    (test-exn exn:fail?
              (run-with-limits
               (thunk
                (error 'thunk "bad thunk"))
               #:timeout/s 60
               #:memory/gb 3))

    (test-exn exn:fail?
              (run-with-limits
               (thunk
                (with-handlers ([exn? (λ (e)
                                        (error 'handler
                                               "bad handler"))])
                  (raise-syntax-error 'inner
                                      "bad stx")))
               #:timeout/s 60
               #:memory/gb 3)))

  (test-begin
    #:name exn-handler
    (test-equal? (run-with-limits
                  (thunk
                   (error 'thunk "bad thunk"))
                  #:timeout/s 60
                  #:memory/gb 3
                  #:exn-handler (λ (e) 'foobar))
                 'foobar))

  (test-begin
    #:name oom-limits
    (ignore (define gb (expt 10 9))
            (define (human-readable bytes) (exact->inexact (/ bytes gb)))
            (define (make-incremental-memory-allocator total
                                                       [result #t])
              (define increment (exact-floor (* 0.1 gb)))
              (λ ()
                (define l
                  (for/list ([i (in-range 0 total increment)])
                    (collect-garbage)
                    (make-bytes increment)))
                (and (andmap bytes? l)
                     result))))
    (run-with-limits
     (make-incremental-memory-allocator (* 0.1 gb))
     #:timeout/s 30
     #:memory/gb 1
     #:oom-result (const (test-fail "Allocator runs oom despite sufficient memory"))
     #:timeout-result (const (test-fail "Allocator times out despite sufficient timeout")))
    (run-with-limits
     (make-incremental-memory-allocator (* 0.9 gb))
     #:timeout/s 30
     #:memory/gb 1
     #:oom-result (const (test-fail "Allocator runs oom despite sufficient memory"))
     #:timeout-result (const (test-fail "Allocator times out despite sufficient timeout")))
    (run-with-limits
     (make-incremental-memory-allocator (* 1.5 gb)
                                        (test-fail "Allocator finishes despite memory limit"))
     #:timeout/s 30
     #:memory/gb 1
     #:oom-result (const #t)
     #:timeout-result (const (test-fail "Allocator times out but should oom")))
    (run-with-limits
     (make-incremental-memory-allocator (* 3 gb)
                                        (test-fail "Allocator finishes despite memory limit"))
     #:timeout/s 30
     #:memory/gb 1
     #:oom-result (const #t)
     #:timeout-result (const (test-fail "Allocator times out but should oom")))
    (run-with-limits
     (make-incremental-memory-allocator (* 3 gb))
     #:timeout/s 30
     #:memory/gb 4
     #:oom-result (const (test-fail "Allocator runs oom despite sufficient memory"))
     #:timeout-result (const (test-fail "Allocator times out despite sufficient timeout")))))
