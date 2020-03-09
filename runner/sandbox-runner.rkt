#lang racket

(provide run-with-limits)

(require racket/async-channel)


(define ((make-async-outcome-reporter thunk outcome-channel))
  (async-channel-put outcome-channel
                     (thunk)))

(define (run-with-limits thunk
                         #:timeout/s [timeout-secs #f]
                         #:memory/gb [memory-limit-gb #f]
                         #:suppress-output? [suppress-output? #f]
                         #:oom-result [make-oom-result (λ () 'oom)]
                         #:timeout-result [make-timeout-result (λ () 'timeout)])
  (define run-custodian (make-custodian))
  (define shutdown-box
    (make-custodian-box run-custodian 'available))
  (when memory-limit-gb
    (custodian-limit-memory run-custodian
                            (exact-floor (* memory-limit-gb (expt 10 9)))))
  (define run
    (λ _
      (parameterize ([current-output-port (if suppress-output?
                                              (open-output-nowhere)
                                              (current-output-port))])
        (thunk))))
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
    ;; if not, get the result of the thunk
    [else
     (async-channel-get outcome-channel)]))
