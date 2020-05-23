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
           lowest-upper-bound-binary-search
           (struct-out result)
           (struct-out try-go-lower)
           (struct-out go-lower)
           (struct-out go-higher)
           (struct-out exhausted))

  (define INDEX-SEARCH-RANGE 20000)

  (struct result (index value) #:prefab)
  (struct try-go-lower (value) #:prefab)
  ;; (struct try-go-higher (value) #:prefab)
  (struct go-lower () #:prefab)
  (struct go-higher () #:prefab)
  (struct exhausted () #:prefab)
  ;; Finds the lowest index for which `index-result` returns `try-go-lower?`
  ;; between `base-min` and `base-max`. For such an index where `index-result`
  ;; returns `(try-go-lower v)`, this function returns `(result index v)`.
  ;;
  ;; Edge cases:
  ;; If no such index can be found between `min` and `max` and `increase-max?`
  ;; is #f, returns `(result i (exhausted))`, where `i` is the highest index
  ;; for which `index-result` returns `go-higher?`.
  ;;
  ;; If `index-result` only returns `go-lower?`, the resulting index will
  ;; instead be `(sub1 min)`.
  ;;
  ;; If `increase-max?` is #t and all results of `index-result` in `[min, max]`
  ;; are `go-higher?`, then the search is repeated on `[max, max+max]`.
  ;; And again if the same happens in that range.
  ;; If `index-result` never returns anything other than `go-higher?` on
  ;; `[0, +inf.0]` then this never terminates.
  (define ((lowest-upper-bound-binary-search index-result)
           #:min [base-min 0]
           #:max [base-max INDEX-SEARCH-RANGE]
           #:default-value [default-end-value (exhausted)]
           #:increase-max? [increase-max? #f])
    (let search ([min base-min]
                 [max base-max]
                 [lowest-end-value-found (result +inf.0 default-end-value)])
      (cond [(and (> min max)
                  increase-max?
                  (zero? (modulo max base-max)))
             (search min
                     (+ max base-max)
                     lowest-end-value-found)]
            [(> min max)
             (match lowest-end-value-found
               [(result +inf.0 v)
                (result max v)]
               [else lowest-end-value-found])]
            [else
             (define index-to-try (+ min (quotient (- max min) 2)))
             (define lowest-index (result-index lowest-end-value-found))
             (match (index-result index-to-try)
               [(go-lower)
                (search min
                        (sub1 index-to-try)
                        lowest-end-value-found)]
               [(go-higher)
                (search (add1 index-to-try)
                        max
                        lowest-end-value-found)]
               [(try-go-lower value)
                (search min
                        (sub1 index-to-try)
                        (if (< index-to-try lowest-index)
                            (result index-to-try value)
                            lowest-end-value-found))]
               #;[(try-go-higher value)
                (search (add1 index-to-try)
                        max
                        (if (< index-to-try lowest-index)
                            (result index-to-try value)
                            lowest-end-value-found))])])))

  (module+ test
    (require ruinit)
    (test-begin
      #:name lowest-upper-bound-binary-search
      (ignore (define search
                (lowest-upper-bound-binary-search
                 (match-lambda
                   [(? (</c 40)) (go-higher)]
                   [(? (and/c (>=/c 40) (<=/c 60)) n) (try-go-lower (- n))]
                   [else (go-lower)]))))
      (test-match (search)
                  (result 40 -40))
      (test-match (search #:min 70)
                  (result 69 (exhausted)))
      (test-match (search #:min 70
                          #:default-value 'foobar)
                  (result 69 'foobar))
      (test-match (search #:max 30
                          #:increase-max? #f)
                  (result 30 (exhausted)))
      (test-match (search #:max 30
                          #:increase-max? #t)
                  (result 40 -40))

      ;; Simulate a program that has no mutants!
      (ignore (define search (lowest-upper-bound-binary-search
                              (match-lambda [else (go-lower)]))))
      (test-match (search)
                  (result -1 (exhausted)))

      ;; The inverse...
      (ignore (define search (lowest-upper-bound-binary-search
                              (match-lambda [else (go-higher)]))))
      (test-match (search #:max 100)
                  (result 100 (exhausted)))


      ;; Simulate a program that passes on all mutants, and has 50 mutants. Then
      ;; we want index 49, and the result should be `exhausted?`.
      (ignore (define search (lowest-upper-bound-binary-search
                              (match-lambda
                                [(? (</c 50)) (go-higher)]
                                [else (go-lower)]))))
      (test-match (search)
                  (result 49 (exhausted)))

      ;; Simulate a program that diverges on the first mutant, and only have 50
      ;; mutants. Then we want index 0.
      (ignore (define search (lowest-upper-bound-binary-search
                              (match-lambda
                                [(? (>/c 50)) (go-lower)]
                                [i (try-go-lower (~a i))]))))
      (test-match (search)
                  (result 0 "0"))

      ;; Simulate a program that diverges on the third mutant up to the tenth,
      ;; then runs out mutants. Then we want index 2.
      (ignore (define search (lowest-upper-bound-binary-search
                              (match-lambda
                                [(? (>/c 9)) (go-lower)]
                                [(? (between/c 2 9) i) (try-go-lower (~a i))]
                                [(? (</c 2)) (go-higher)]))))
      (test-match (search)
                  (result 2 "2")))))

(module parity-check rscript
  (provide (struct-out no-more-mutants)
           process-parity-check-main
           mutated-id-for-typed/untyped-agrees?)

  (require "../configurations/configure-benchmark.rkt"
           "../mutate/mutate-program.rkt"
           "../mutate/logger.rkt"
           "../runner/mutation-runner.rkt"
           "../runner/program.rkt"
           "../util/path-utils.rkt"
           (submod ".." common)
           racket/logging)

  (struct no-more-mutants () #:prefab)
  (define (extract-mutation stx index)
    (define-values {read-end write-end} (make-pipe))
    (define mutated-id
      (with-logging-to-port write-end
        (thunk
         (define-values {_ id}
           (mutate-module stx index))
         id)
        #:logger mutate-logger
        'info
        'mutate))
    (define mutation
      (match (regexp-match #px"(?m:mutate: Mutating .*)"
                           read-end)
        [(list str) str]))
    (list mutated-id
          (strip-paths mutation)))

  (define (strip-paths str)
    (define stx-path-rx @~a{#<syntax:.+\.rkt:\d+:\d+})
    (regexp-replace* (pregexp @~a{@stx-path-rx (.+)> -> @stx-path-rx (.+)>})
                     str
                     @~a{\1 -> \2}))

  (define/contract (mutated-id-for-typed/untyped-agrees? a-benchmark
                                                         a-module-to-mutate
                                                         mutation-index)
    (benchmark/c
     module-name?
     natural?
     . -> .
     (or/c no-more-mutants?
           (list/c (or/c no-more-mutants?
                         (list/c symbol? any/c))
                   (or/c no-more-mutants?
                         (list/c symbol? any/c)))
           #t))
    (define mutateds
      (for/list ([typed-or-untyped-files (in-list (list benchmark-typed
                                                        benchmark-untyped))])
        (define mod-path (pick-file-by-name (typed-or-untyped-files a-benchmark)
                                            a-module-to-mutate))
        (define mod (make-mod mod-path))
        (with-handlers ([mutation-index-exception? (const (no-more-mutants))])
          (extract-mutation (mod-stx mod) mutation-index))))
    (match mutateds
      [(list (no-more-mutants) (no-more-mutants))
       (no-more-mutants)]
      [(list (list (? symbol? a) e-a)
             (list (? symbol? b) e-b))
       #:when (and (equal? a b)
                   (equal? e-a e-b))
       #t]
      [(list a b) (list a b)]))

  (define (parity-check/binary-search the-benchmark
                                      the-module-to-mutate)
    (define (result-for-index index)
      (match (mutated-id-for-typed/untyped-agrees? the-benchmark
                                                   the-module-to-mutate
                                                   index)
        [(no-more-mutants) (go-lower)]
        [#t                (go-higher)]
        [(list a b)        (try-go-lower (list a b))]))
    (define search (lowest-upper-bound-binary-search result-for-index))
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
      [(result index (exhausted))
       (log-parity-info @~a{@complete-msg : ok, max index @index})
       (done 'ok)]
      [(result index (list typed-mutated untyped-mutated))
       (log-parity-info @~a{@complete-msg : divergence found})
       (done @~a{
                 Typed/untyped disagree in @benchmark-name @;
                 module @mod-to-mutate starting at mutation index @index
                 Typed mutates:   @(first typed-mutated)
                 Mutation:        @(second typed-mutated)
                 Untyped mutates: @(first untyped-mutated)
                 Mutation:        @(second untyped-mutated)

                 })]
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
