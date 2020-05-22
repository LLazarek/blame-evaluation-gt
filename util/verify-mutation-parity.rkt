#lang at-exp rscript

(provide mutated-id-for-typed/untyped-agrees?)

(require "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../configurations/configure-benchmark.rkt"
         "../runner/program.rkt"
         "../mutate/mutate-program.rkt"
         "../process-q/priority.rkt"
         "../process-q/interface.rkt"
         "for-first-star.rkt"
         racket/runtime-path)

(module common rscript
  (provide INDEX-SEARCH-RANGE
           greatest-lower-bound-binary-search
           (struct-out result)
           (struct-out go-lower))

  (define INDEX-SEARCH-RANGE 20000)

  (struct result (index value) #:prefab)
  (struct go-lower () #:prefab)
  ;; index-result := (integer? -> (or/c '< '> any/c))
  (define ((greatest-lower-bound-binary-search index-result)
           #:min [base-min 0]
           #:max [base-max INDEX-SEARCH-RANGE]
           #:default-value [default-end-value (result #f #f)]
           #:increase-max? [increase-max? #f])
    (let search ([min base-min]
                 [max base-max]
                 [lowest-end-value-found default-end-value])
      (cond [(and (> min max)
                  increase-max?
                  (zero? (modulo max base-max)))
             (search min
                     (+ max base-max)
                     lowest-end-value-found)]
            [(> min max)
             lowest-end-value-found]
            [else
             (define index-to-try (+ min (quotient (- max min) 2)))
             (match (index-result index-to-try)
               [(go-lower)
                (search min
                        (sub1 index-to-try)
                        lowest-end-value-found)]
               [value
                (search (add1 index-to-try)
                        max
                        (result index-to-try
                                value))])])))

  (module+ test
    (require ruinit)
    (test-begin
      #:name greatest-lower-bound-binary-search
      (ignore (define search (greatest-lower-bound-binary-search
                              (match-lambda
                                [(? (<=/c 50) n) (- n)]
                                [else (go-lower)]))))
      (test-match (search)
                  (result 50 -50))
      (test-match (search #:min 60)
                  (result #f #f))
      (test-match (search #:max 30)
                  (result 30 -30))
      (test-match (search #:max 30
                          #:increase-max? #t)
                  (result 50 -50))
      (test-match (search #:min 60
                          #:default-value 'foobar)
                  'foobar))))

(module parity-check rscript
  (provide (struct-out no-more-mutants)
           process-parity-check-main
           mutated-id-for-typed/untyped-agrees?)

  (require "../configurations/configure-benchmark.rkt"
           "../mutate/mutate-program.rkt"
           "../runner/mutation-runner.rkt"
           "../runner/program.rkt"
           "../util/path-utils.rkt"
           (submod ".." common))

  (struct no-more-mutants () #:prefab)
  (define/contract (mutated-id-for-typed/untyped-agrees? a-benchmark
                                                         a-module-to-mutate
                                                         mutation-index)
    (benchmark/c
     module-name?
     natural?
     . -> .
     (or/c no-more-mutants?
           (list/c (or/c no-more-mutants? symbol?)
                   (or/c no-more-mutants? symbol?))
           #t))
    (define mutateds
      (for/list ([typed-or-untyped-files (in-list (list benchmark-typed
                                                        benchmark-untyped))])
        (define mod-path (pick-file-by-name (typed-or-untyped-files a-benchmark)
                                            a-module-to-mutate))
        (define mod (make-mod mod-path))
        (with-handlers ([mutation-index-exception? (const (no-more-mutants))])
          (define-values {_ id}
            (mutate-module (mod-stx mod) mutation-index))
          id)))
    (match mutateds
      [(list (no-more-mutants) (no-more-mutants))
       (no-more-mutants)]
      [(list (? symbol? a)     (? symbol? b))
       #:when (equal? a b)
       #t]
      [(list a b) (list a b)]))

  (define (parity-check/binary-search the-benchmark
                                      the-module-to-mutate)
    (define (result-for-index index)
      (match (mutated-id-for-typed/untyped-agrees? the-benchmark
                                                   the-module-to-mutate
                                                   index)
        [(no-more-mutants) (go-lower)]
        [#t #t]
        [(list a b) (list a b)]))
    (define search (greatest-lower-bound-binary-search result-for-index))
    (search))

  (define (process-parity-check-main)
    (define a-benchmark (apply benchmark (read)))
    (define a-module-to-mutate (read))
    (define result
      (parity-check/binary-search a-benchmark
                                  a-module-to-mutate))
    (writeln result)))

(define (process-checker-for a-benchmark
                             mod-to-mutate)
  (match-define (list stdout stdin _ #f ctl)
    (process/ports #f #f (current-error-port)
                   @~a{
                       rt @;
                       -e '(require @;
                            (submod (file "@this-file-path") @;
                                    parity-check))' @;
                       -e '(process-parity-check-main)'
                       }))
  (writeln (match a-benchmark
             [(benchmark t ut base both)
              (list (map path->string t)
                    (map path->string ut)
                    (if base (path->string base) base)
                    (if both (path->string both) both))])
           stdin)
  (writeln mod-to-mutate stdin)
  (close-output-port stdin)
  (values stdout ctl))




(define-logger parity)
(define-runtime-path this-file-path "verify-mutation-parity.rkt")
(struct parity-checker-process (stdout mod-name benchmark-name spawn-ms))
(struct done (result))
(require 'common)

(define (process-will:report-mutant-parity current-process-q
                                           info)
  (match-define (parity-checker-process stdout
                                        mod-to-mutate
                                        benchmark-name
                                        spawn-ms)
    (process-info-data info))
  (define mutated-id-results
    #;(let ([str (port->string stdout)])
        (displayln "output:")
        (displayln str)
        (displayln "---")
        (call-with-input-string str read))
    (read stdout))
  (close-input-port stdout)
  (define time-elapsed-mins
    (/ (- (current-inexact-milliseconds)
          spawn-ms)
       1000
       60))
  (define complete-msg
   @~a{@benchmark-name "@mod-to-mutate" complete in @time-elapsed-mins min})
  (define parity-worker-result
    (match mutated-id-results
      [(result index #t)
       (log-parity-info @~a{@complete-msg : ok, max index @index})
       (done 'ok)]
      [(result index (list typed-mutated untyped-mutated))
       (log-parity-info @~a{@complete-msg : divergence found})
       (done @~a{
                 Typed/untyped disagree in @benchmark-name @;
                 module @mod-to-mutate starting at mutation index @index
                 Typed mutates:   @typed-mutated
                 Untyped mutates: @untyped-mutated

                 })]
      [(result #f #f)
       (log-parity-info @~a{@complete-msg : has no mutants})
       (done 'ok)]
      [(result index anything-else)
       (raise-user-error
        'verify-mutation-parity
        @~a{
            Unexpected result from worker: @;
            @benchmark-name module @mod-to-mutate index @index : @;
            @~v[anything-else]
            })]))
  (report-parity-results! parity-worker-result)
  current-process-q)

(define (report-parity-results! result)
  (match result
    [(done 'ok) (void)]
    [(done msg) (displayln msg)]))


(require 'parity-check)
(define (verify-mutation-parity-for a-benchmark process-q)
  (define benchmark-name (benchmark->name a-benchmark))

  (define (enq-mutant-parity-checker-for current-process-q
                                         mod-to-mutate)
    (process-Q-enq
     current-process-q
     (thunk
      (log-parity-info
       @~a{Spawning checker for @benchmark-name "@mod-to-mutate"})
      (define-values {stdout ctl}
        (process-checker-for a-benchmark
                             mod-to-mutate))
      (process-info (parity-checker-process stdout
                                            mod-to-mutate
                                            benchmark-name
                                            (current-inexact-milliseconds))
                    ctl
                    process-will:report-mutant-parity))))

  (for/fold ([current-process-q process-q])
            ([mod-path (in-list (benchmark-typed a-benchmark))])
    (define mod-to-mutate (file-name-string-from-path mod-path))

    (enq-mutant-parity-checker-for current-process-q
                                   mod-to-mutate)))

(define (benchmark->name a-benchmark)
  (match (benchmark-typed a-benchmark)
    [(list* (app explode-path/string
                 (list _ ... name "typed" _)) _)
     name]))

(define (verify-benchmarks-mutation-parity list-of-benchmarks
                                           process-q)
  (for/fold ([current-process-q process-q])
            ([a-benchmark list-of-benchmarks])
    (verify-mutation-parity-for a-benchmark current-process-q)))

(define (verify-mutation-parity-of-all-benchmarks-in dir
                                                     process-q)
  (define benchmarks
    (for/list ([a-benchmark-path (in-list (directory-list dir #:build? #t))]
               #:when (directory-exists? a-benchmark-path))
      (read-benchmark a-benchmark-path)))
  (verify-benchmarks-mutation-parity benchmarks
                                     process-q))

(main
 #:arguments {[flags (list benchmarks-dir)]
              #:once-each
              [("-n" "--processes")
               'procs
               ("Specify the number of parallel processes to use."
                "Default: 1")
               #:collect ["N" take-latest "1"]]
              #:args [benchmarks-dir]}
 (file-stream-buffer-mode (current-output-port) 'line)
 (define n-processes (string->number (hash-ref flags 'procs)))
 (define start-ms (current-inexact-milliseconds))
 (define q
   (verify-mutation-parity-of-all-benchmarks-in
    benchmarks-dir
    (make-process-Q n-processes
                    (hash))))
 (void (process-Q-wait q))
 (define end-ms (current-inexact-milliseconds))
 (define total-minutes (/ (- end-ms start-ms) 1000 60))
 (log-parity-info @~a{Analysis complete in @total-minutes min}))
