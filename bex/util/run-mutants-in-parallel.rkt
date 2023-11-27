#lang at-exp racket

(require "../configurations/configure-benchmark.rkt"
         "../configurations/config.rkt"
         "progress-log.rkt"
         "mutant-util.rkt"
         process-queue/priority)

(define result-type/c any/c)
(provide (contract-out
          [collect-mutant-results
           ({(listof benchmark/c)
             path-string?
             (string? . -> . (listof natural?))
             (mutant? . -> . config/c)
             natural?
             (path-string? mutant? . -> . result-type/c) ; outfile->result
             #:progress-logging (or/c (list/c 'auto path-string?)
                                      (list/c
                                       ; log-progress!
                                       (string? string? natural? result-type/c . -> . any)
                                       ; logged-progress-for
                                       (string? string? natural? . -> . any/c)
                                       ; no-progress-logged result
                                       any/c)
                                      #f)
             #:working-dir path-string?}
            {#:failure-retries natural?
             ;; outfile->result can request a run with some valid result to be
             ;; retried up to this many times (e.g. to confirm a type error) by
             ;; returning an instance of the `retry` struct containing the
             ;; result. After the last such retry the inner result will be
             ;; committed.
             #:other-retries natural?}
            . ->* .
            (hash/c mutant? result-type/c))])
         (struct-out retry)
         raise-bad-mutant-result
         exn:fail:bad-mutant-result?
         mutant-results-logger)

(define (process-queue-update-data q f)
  (process-queue-set-data q (f (process-queue-get-data q))))

(struct exn:fail:bad-mutant-result exn:fail ())

(define (raise-bad-mutant-result [msg "<missing>"])
  (raise (exn:fail:bad-mutant-result msg
                                     (current-continuation-marks))))

(define-logger mutant-results)

(struct retry (result reason))

(define (collect-mutant-results benchmarks
                                experiment-config-path
                                mutants-for-mod
                                mutant->config-to-run
                                process-limit
                                outfile->result ;; should raise an exn:fail:bad-mutant-result? on error / bad/failure result
                                #:progress-logging maybe-progress-logging-fns
                                #:working-dir working-dir
                                #:failure-retries [failure-retries 3]
                                #:other-retries [other-retries 3])
  (define-values {log-progress!
                  logged-progress
                  no-recorded-progress-value
                  finalize-log!}
    (match maybe-progress-logging-fns
      [(list (? procedure? log) (? procedure? get) no-recorded-progress-value)
       (values log get no-recorded-progress-value void)]
      [(list 'auto (? path-string? path))
       (make-simple-logger path)]
      [#f
       (values void void (void) void)]))
  (define q
    (for*/fold ([q (make-process-queue process-limit
                                       ;; (hash/c mutant/c any/c)
                                       (hash))])
               ([a-benchmark (in-list benchmarks)]
                [benchmark-name (in-value (benchmark->name a-benchmark))]
                [mod-name (in-list (benchmark->mutatable-modules a-benchmark))]
                [index (in-list (mutants-for-mod mod-name))])
      (define the-mutant (mutant #f mod-name index))
      (match (logged-progress benchmark-name mod-name index)
        [(== no-recorded-progress-value)
         (process-queue-enqueue q
                                (runner a-benchmark
                                        experiment-config-path
                                        the-mutant
                                        (mutant->config-to-run the-mutant)
                                        outfile->result
                                        #:log-progress log-progress!
                                        #:working-dir working-dir
                                        #:failure-retries failure-retries
                                        #:other-retries other-retries)
                                0)]
        [already-computed-result
         (log-mutant-results-debug @~a{Pulling logged result for @the-mutant})
         (process-queue-update-data q (λ (h) (hash-set h the-mutant already-computed-result)))])))
  (begin0 (process-queue-get-data (process-queue-wait q))
    (finalize-log!)))

(define (make-simple-logger path)
  (define progress
    (if (file-exists? path)
        (make-hash (file->list path))
        (hash)))
  (define-values {log-progress!/raw finalize-log!}
    (initialize-progress-log! path
                              #:exists 'append))
  (define (log-progress! benchmark module-to-mutate-name mutation-index result)
    (log-progress!/raw (cons (list benchmark
                                   module-to-mutate-name
                                   mutation-index)
                             result)))
  (define no-result-sentinel
    (let ()
      (struct no-result-logged ())
      (no-result-logged)))
  (define (logged-results-for benchmark module-to-mutate-name mutation-index)
    (hash-ref progress
              (list benchmark module-to-mutate-name mutation-index)
              no-result-sentinel))
  (values log-progress!
          logged-results-for
          no-result-sentinel
          finalize-log!))

(define (runner the-benchmark
                experiment-config-path
                the-mutant
                config-to-run
                outfile->result
                #:log-progress log-progress!
                #:working-dir working-dir
                #:failure-retries failure-retries
                #:other-retries other-retries)
  (define the-benchmark-name (benchmark->name the-benchmark))
  (match-define (mutant _ mod-to-mutate index) the-mutant)
  (define the-mutant+bench (mutant the-benchmark-name mod-to-mutate index))
  (define the-benchmark-configuration (configure-benchmark the-benchmark
                                                           config-to-run))
  (define ((mutant-spawner will))
    (define outfile
      (make-temporary-file @~a{@|the-benchmark-name|-@|mod-to-mutate|-@|index|-~a}
                           #f
                           working-dir))
    (log-mutant-results-info @~a{Spawned @the-mutant+bench})
    (define ctl
      (parameterize ([mutant-error-log outfile])
        (spawn-mutant-runner the-benchmark-configuration
                             mod-to-mutate
                             index
                             outfile
                             experiment-config-path)))
    (process-info outfile ctl will))
  (define ((make-will:record-outcome! failure-retries-so-far other-retries-so-far) q info)
    (log-mutant-results-debug @~a{
                                  Queue size: { @;
                                  active: @(process-queue-active-count q), @;
                                  waiting: @(process-queue-waiting-count q) @;
                                  }
                                  })
    (log-mutant-results-info @~a{@the-mutant+bench done})
    (with-handlers ([exn:fail:bad-mutant-result?
                     (λ (e)
                       (log-mutant-results-info
                        @~a{Failed to extract result from @the-benchmark-name @the-mutant})
                       (cond [(< failure-retries-so-far failure-retries)
                              (log-mutant-results-info
                               @~a{
                                   Retrying @the-benchmark-name @the-mutant @;
                                   (@(add1 failure-retries-so-far) / @failure-retries)
                                   })
                              (process-queue-enqueue
                               q
                               (mutant-spawner (make-will:record-outcome! (add1 failure-retries-so-far)
                                                                          other-retries-so-far))
                               (+ 1 failure-retries-so-far other-retries-so-far))]
                             [else
                              (error 'collect-mutant-results
                                     @~a{
                                         Unable to extract result from @the-mutant @;
                                         despite @failure-retries tries. Failure reason: @;
                                         @(exn-message e)
                                         })]))])
      (match (outfile->result (process-info-data info) the-mutant)
        [(retry _ reason)
         #:when (< other-retries-so-far other-retries)
         (log-mutant-results-info
                               @~a{
                                   Retrying @the-benchmark-name @the-mutant @;
                                   for reason: @reason @;
                                   (@(add1 other-retries-so-far) / @other-retries)
                                   })
         (process-queue-enqueue
          q
          (mutant-spawner (make-will:record-outcome! 0
                                                     (add1 other-retries-so-far)))
          (+ 1 failure-retries-so-far other-retries-so-far))]
        [(or (retry result _)
             result)
         (log-progress! the-benchmark-name mod-to-mutate index result)
         (process-queue-update-data q (λ (h) (hash-set h the-mutant result)))])))

  (mutant-spawner (make-will:record-outcome! 0 0)))
