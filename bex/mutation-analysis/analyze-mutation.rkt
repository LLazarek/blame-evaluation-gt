#lang at-exp rscript

(require "../configurations/configure-benchmark.rkt"
         "../runner/mutation-runner-data.rkt"
         "../util/path-utils.rkt"
         "../util/for.rkt"
         "../util/mutant-util.rkt"
         "../util/run-mutants-in-parallel.rkt"
         "../configurables/configurables.rkt"
         racket/hash)

(define-logger mutation-analysis)

(define/contract type-error-results
  (listof run-outcome/c)
  '(type-error))
(define/contract any-error-results
  (listof run-outcome/c)
  '(type-error runtime-error blamed))

(define/contract (mutation-info-for-all-mutants bench
                                                any-error? ; #f means type error only
                                                #:process-limit proc-limit
                                                #:working-dir working-dir
                                                #:log-progress progress-log-path)
  (benchmark/c
   boolean?
   #:process-limit natural?
   #:working-dir path-to-existant-directory?
   #:log-progress path-string?

   . -> . any)

  (define mutatable-module-names ((configured:select-modules-to-mutate) bench))

  (log-mutation-analysis-info
   @~a{Configured mutatable modules: @~s[mutatable-module-names]})

  (define (mutant-outfile->result outfile the-mutant)
    (cond [(file-exists? outfile)
           (define-values {result mutation-type}
             (extract-mutation-type-and-result outfile the-mutant))
           (define mutation-good?
             (member result
                     (if any-error?
                         any-error-results
                         type-error-results)))
           (log-mutation-analysis-info
            @~a{
                @the-mutant {@mutation-type} => @(if mutation-good? 'hit 'miss)
                })
           (if (equal? result 'type-error)
               (retry (list mutation-good? mutation-type) "confirm type error")
               (list mutation-good? mutation-type))]
          [else (raise-bad-mutant-result @~a{output @outfile doesn't exist})]))

  (define mutant-results
    (parameterize ([current-mutant-runner-log-mutation-info? #t]
                   [default-timeout/s (* 60 3)])
      (collect-mutant-results
       (list bench)
       (current-configuration-path)
       (λ (module-to-mutate-name)
         (range (add1 (max-mutation-index module-to-mutate-name bench))))
       (const (make-max-bench-config bench))
       proc-limit
       mutant-outfile->result
       #:progress-logging `(auto ,progress-log-path)
       #:working-dir working-dir
       #:failure-retries 3
       #:other-retries 2))) ;; confirm type errors

  (define success+fails
    (for/hash/fold ([{mutant result} (in-hash mutant-results)])
      #:combine (λ (a b) (hash-union a b #:combine +))
      #:default (hash)
      (values (if (first result)
                  'success
                  'fail)
              (hash (second result) 1))))
  (hash-set success+fails
            'total
            (for*/sum ([key '(success fail)]
                       [{_ n} (hash-ref success+fails key (hash))])
              n)))

(define (extract-mutation-type-and-result f mutant)
  (define trimmed-output
    #;(system/string @~a{grep -B 1 -E "mutate: Mutating|run-status" @f})
    (file->string f))
  (define output-regexp
    ;; assumption: mutation types don't have spaces in them
    (pregexp @~a{
                 (?m:@;
                 ^mutate: Mutating with ([^ ]+) : .* -> .*$
                 (#s\(run-status.+)$@;
                 )
           }))
  (match trimmed-output
    [(regexp output-regexp
             (list _ mutation-type rs-string))
     (define the-run-status (with-input-from-string rs-string read))
     (values (run-status-outcome the-run-status) mutation-type)]
    [else (raise-bad-mutant-result @~a{
                                       output (below) doesn't match expected shape
                                       @trimmed-output
                                       })]))

(main
 #:arguments {[flags args]
              #:once-each
              [("-b" "--benchmark")
               'bench-to-run
               "Path to benchmark to run. Mandatory."
               #:mandatory
               #:collect ["path" take-latest #f]]
              [("-c" "--config")
               'config-path
               "The config to use for generating mutants. Mandatory."
               #:mandatory
               #:collect ["path" take-latest #f]]
              [("-l" "--progress-log")
               'progress-log
               ("Record progress in the given log file."
                "If it exists and is not empty, resume from the point reached in the log."
                "Mandatory.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--output-dir")
               'data-output-dir
               "Data output directory. Mandatory."
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-n" "--process-limit")
               'process-limit
               "Number of processes to have running at once. Default: 2"
               #:collect ["N" take-latest "2"]]
              [("-e" "--error-log")
               'error-log
               "File to which to append mutant errors. Default: ./mutant-errors.txt"
               #:collect ["path" (set-parameter mutant-error-log) #f]]
              [("-a" "--any-error")
               'check-for-any-error?
               ("Consider a mutant that causes any kind of error in the top lattice-config to be good."
                "By default, only mutants that cause *type errors* are considered good.")
               #:record]}
 (install-configuration! (hash-ref flags 'config-path))
 (define data-output-dir (hash-ref flags 'data-output-dir))
 (make-directory* data-output-dir)
 (define result
   (mutation-info-for-all-mutants (read-benchmark (hash-ref flags 'bench-to-run))
                                  (hash-ref flags 'check-for-any-error?)
                                  #:process-limit (string->number (hash-ref flags 'process-limit))
                                  #:working-dir data-output-dir
                                  #:log-progress (hash-ref flags 'progress-log)))
 (log-mutation-analysis-info "Mutation analysis complete.")
 (pretty-display result))

(module test racket/base)
