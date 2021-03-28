#lang at-exp racket/base

(require "../util/optional-contracts.rkt"
         (except-in racket/contract
                    contract-out
                    define/contract))
(provide (contract-out
          [run-with-limits
           ({(-> any)}
            {#:timeout/s (or/c #f (and/c real? (>=/c 0)))
             #:memory/gb (or/c #f (and/c real? (>=/c 0)))
             #:suppress-output? boolean?
             #:oom-result (-> any)
             #:timeout-result (-> any)}
            . ->* .
            any)]))

(require racket/match
         racket/port
         racket/sandbox)

(define (run-with-limits thunk
                         #:timeout/s [timeout-secs #f]
                         #:memory/gb [memory-limit-gb #f]
                         #:suppress-output? [suppress-output? #f]
                         #:oom-result [make-oom-result (λ () 'oom)]
                         #:timeout-result [make-timeout-result (λ () 'timeout)])
  (define run
    (λ _
      (define /dev/null (open-output-nowhere))
      (define result
        (parameterize ([current-output-port (if suppress-output?
                                                /dev/null
                                                (current-output-port))]
                       [current-error-port (if suppress-output?
                                               /dev/null
                                               (current-error-port))])
          (thunk)))
      (close-output-port /dev/null)
      result))

  (define (resource-limit-result e)
    (match (exn:fail:resource-resource e)
      ['memory (make-oom-result)]
      [else (make-timeout-result)]))
  #;(define (terminated-result e)
    (displayln (exn:fail:sandbox-terminated-reason e))
    (match (exn:fail:sandbox-terminated-reason e)
      ['out-of-memory (make-oom-result)]
      [else (make-timeout-result)]))

  (define time-limit (and timeout-secs
                          (round timeout-secs)))
  (define memory-limit (and memory-limit-gb
                            (round (* memory-limit-gb 1024))))
  (define result
    ;; this seems to be pretty much equivalent to the much simpler
    ;; `call-with-limits` below
    #;(parameterize ([sandbox-path-permissions '((read #rx#".*"))]
                   [sandbox-memory-limit memory-limit]
                   [sandbox-eval-limits (list time-limit memory-limit)]

                   [sandbox-output (current-output-port)]
                   [sandbox-error-output (current-error-port)]
                   [sandbox-input (current-input-port)]

                   [sandbox-make-code-inspector current-code-inspector])
      (let ([sandbox-eval
             (make-evaluator 'racket/base)])
        (with-handlers ([exn:fail:resource? resource-limit-result]
                        [exn:fail:sandbox-terminated? terminated-result])
          (sandbox-eval (list run)))))
    (with-handlers ([exn:fail:resource? resource-limit-result])
      (call-with-limits time-limit
                        memory-limit
                        run)))

  ;; Force garbage collection so repeated oom runs don't cause memory exhaustion
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)

  result)

(module+ test
  (require ruinit
           racket)

  (test-begin
    #:name timeout
    (run-with-limits (λ _ (sleep 10) 42)
                     #:timeout/s 2))

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
     #:timeout/s 60
     #:memory/gb 4
     #:oom-result (const (test-fail "Allocator runs oom despite sufficient memory"))
     #:timeout-result (const (test-fail "Allocator times out despite sufficient timeout")))))
