#lang at-exp racket

(require "../process-q/functional.rkt"
         "../configurations/configure-benchmark.rkt"
         "../mutate/mutate.rkt"
         "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "progress-log.rkt"
         "mutant-util.rkt"
         racket/runtime-path)

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))

(define-logger mutation-analysis)

(define/contract (mutation-info-for-all-mutants bench
                                                #:process-limit proc-limit
                                                #:log-progress log-progress!
                                                #:resume-cache cached-results-for)
  ({benchmark/c
    #:process-limit natural?

    #:log-progress
    (module-name? natural? boolean? string? . -> . any)

    #:resume-cache
    (module-name? natural? . -> . (or/c #f (list/c boolean? string?)))}
   {}
   . ->* . any)

  (define mutatable-module-names (benchmark->mutatable-modules bench))
  (define max-config (make-max-bench-config bench))

  (unless (directory-exists? (data-output-dir))
    (make-directory (data-output-dir)))

  (define q
    (for/fold ([q (make-process-Q proc-limit
                                  ; (hash/c name? (hash/c 'type-error natural? 'total natural?))
                                  (hash))])
              ([module-to-mutate-name mutatable-module-names]
               [i-1 (in-naturals)]
               #:when #t
               [index (in-mutation-indices module-to-mutate-name
                                           bench)]
               [i-2 (in-naturals)])
      (match (cached-results-for module-to-mutate-name index)
        [#f (enq-process q
                         (λ _ (mutation-info-for bench
                                                 module-to-mutate-name
                                                 index
                                                 (~a i-1 '- i-2)
                                                 #:progress-logger log-progress!)))]
        [(list type-error? mutation-type)
         (log-mutation-analysis-info "Pulling cached result:")
         (log-progress! module-to-mutate-name
                        index
                        type-error?
                        mutation-type)
         (add-mutation-type-result q type-error? mutation-type)])))

  (log-mutation-analysis-info
   @~a{
       Done enqueuing mutants. @;
       Q has @(process-Q-active-count q) active and @(process-Q-waiting-count q) waiting. @;
       Waiting...})

  (define q* (process-Q-wait q))
  (log-mutation-analysis-info "Done waiting.")
  (pretty-display (process-Q-data q*)))

(define (mutation-info-for bench
                           module-to-mutate-name
                           index
                           id
                           #:progress-logger log-progress!)
  (define max-config (make-max-bench-config bench))
  (define the-benchmark-configuration
    (configure-benchmark bench
                         max-config))
  (define outfile (build-path (data-output-dir)
                              (format "~a_index~a_~a.rktd"
                                      module-to-mutate-name
                                      index
                                      id)))
  (define ctl (spawn-mutant-runner the-benchmark-configuration
                                   module-to-mutate-name
                                   index
                                   outfile
                                   #:log-mutation-info? #t))
  (log-mutation-analysis-info
   @~a{Spawned mutant @module-to-mutate-name @"@" @index})
  (define (will:record-type-error q* info)
    (define-values {type-error? mutation-type}
      (extract-mutation-type-and-result outfile))
    (log-progress! module-to-mutate-name
                   index
                   type-error?
                   mutation-type)
    (add-mutation-type-result q*
                              type-error?
                              mutation-type))
  (process-info #f
                ctl
                will:record-type-error))

(define (add-mutation-type-result q type-error? mutation-type)
  (define (update-inner-hash h)
    (hash-update h mutation-type add1 0))
  (define (update h)
    (define h* (hash-update h "total" add1 0))
    (hash-update h*
                 (if type-error? "success" "fail")
                 update-inner-hash
                 (hash)))
  (process-Q-data-set q (update (process-Q-data q))))

(define (extract-mutation-type-and-result f)
  (define output-regexp
    (pregexp @~a{
                 mutate: type: (\S+)
                 mutate: Mutating.+
                 #s\(run-status "[^"]+" \d+ \S+ (\S+)
@; close "{
                 }))
  #;(displayln (list output-regexp
                   (file->string f)
                   (regexp-match output-regexp
                                 (file->string f))))
  (match (file->string f)
    [(regexp output-regexp
             (list _ mutation-type outcome))
     (define type-error? (string=? outcome "type-error"))
     (values type-error? mutation-type)]
    [other-contents
     (raise-user-error @~a{
                           Unable to match against file contents:
                           @other-contents
                           })]))

(module+ main
  (require racket/cmdline)
  (define bench-to-run (make-parameter #f))
  (define progress-log (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    path
    "Path to benchmark to run."
    (bench-to-run path)]
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
   [("-l" "--progress-log")
    path
    ("Record progress in the given log file."
     "If it exists and is not empty, resume from the point reached in the log.")
    (progress-log path)])
  (unless (bench-to-run)
    (error 'mutant-factory "Must provide benchmark to run."))
  (define progress
    (match (progress-log)
      [(? file-exists? path) (make-hash (file->list path))]
      [else (hash)]))
  (define-values {log-progress!/raw finalize-log!}
    (initialize-progress-log! (progress-log)))
  (define (log-progress! module-to-mutate-name mutation-index type-error? mutation-type)
    (log-mutation-analysis-info
     @~a{
         Mutant @module-to-mutate-name @"@" @mutation-index @;
         {@mutation-type} => @(if type-error?
                                  'hit
                                  'miss)
         })
    (log-progress!/raw (cons (list module-to-mutate-name
                                   mutation-index)
                             (list type-error?
                                   mutation-type))))
  (define (cached-results-for module-to-mutate-name mutation-index)
    (hash-ref progress
              (list module-to-mutate-name mutation-index)
              #f))
  (mutation-info-for-all-mutants (read-benchmark (bench-to-run))
                                 #:process-limit (process-limit)
                                 #:log-progress log-progress!
                                 #:resume-cache cached-results-for)
  (finalize-log!))
