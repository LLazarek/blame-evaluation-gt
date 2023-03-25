#lang at-exp rscript

(provide mutated-id-for-typed/untyped-agrees?)

(require "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "../util/program.rkt"
         "../util/for.rkt"
         "../util/binary-search.rkt"
         process-queue/priority
         racket/runtime-path)

(module common rscript
  (require "../util/binary-search.rkt")
  (provide index-ranges)

  (define (time-cost-f index)
    #;(* index 6.5e-4)
    (* 0.2536138770204011 (exp (* 1.9720252428451186e-4 index))))
  (define INDEX-RANGE-DESIRED-SECS (* 10 60))
  (define index-ranges
    (let loop ([groups '()]
               [start 0])
      (cond [(> start INDEX-SEARCH-RANGE)
             (reverse (cons (list start +inf.0)
                            groups))]
            [else
             (define new-start
               (for/fold ([end start]
                          [time-sum 0]
                          #:result end)
                         ([i (in-naturals start)]
                          #:break (> time-sum INDEX-RANGE-DESIRED-SECS))
                 (define time (time-cost-f i))
                 (values i (+ time-sum time))))
             (loop (cons (list start new-start)
                         groups)
                   (add1 new-start))]))))

(module parity-check rscript
  (provide (struct-out no-more-mutants)
           process-parity-check-main/binary-search
           process-parity-check-main/exhaustive
           mutated-id-for-typed/untyped-agrees?)

  (require "../configurations/configure-benchmark.rkt"
           "../configurables/configurables.rkt"
           "../runner/mutation-runner.rkt"
           "../util/program.rkt"
           "../util/path-utils.rkt"
           "../util/for.rkt"
           "../util/binary-search.rkt"
           "../util/mutant-util.rkt"
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
           (list/c (or/c no-more-mutants?
                         (list/c symbol? any/c))
                   (or/c no-more-mutants?
                         (list/c symbol? any/c)))
           #t))

    (define typed-config (make-max-bench-config a-benchmark))
    (define untyped-config (for/hash ([{mod _} (in-hash typed-config)])
                             (values mod 'none)))
    (define typed-benchmark-configuration (configure-benchmark a-benchmark typed-config))
    (define untyped-benchmark-configuration (configure-benchmark a-benchmark untyped-config))
    (define typed-program (benchmark-configuration->program typed-benchmark-configuration))
    (define untyped-program (benchmark-configuration->program untyped-benchmark-configuration))

    (define mod-to-mutate? (compose1 (path-ends-with a-module-to-mutate)
                                     mod-path))

    (define mutateds
      (for/list ([typed-or-untyped-program (in-list (list typed-program
                                                          untyped-program))])
        (define mod (findf mod-to-mutate? (program->mods typed-or-untyped-program)))
        (with-handlers ([mutation-index-exception? (const (no-more-mutants))])
          (extract-mutation mod mutation-index typed-or-untyped-program))))
    (match mutateds
      [(list (no-more-mutants) (no-more-mutants))
       (no-more-mutants)]
      [(list (list (? symbol? a) (? string?) e-a)
             (list (? symbol? b) (? string?) e-b))
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
    (search #:increase-max? #t))

  (define (parity-check/exhaustive the-benchmark
                                   the-module-to-mutate
                                   index-range)
    (define mid-range-end
      (for/first*
       ([i (apply in-range index-range)])
       (match (mutated-id-for-typed/untyped-agrees? the-benchmark
                                                    the-module-to-mutate
                                                    i)
         [(no-more-mutants) (result (sub1 i) (exhausted))]
         [#t #f]
         [(? list? difference) (result i difference)])))
    (or mid-range-end
        (result (second index-range) (exhausted))))

  (define (process-parity-check-main/binary-search)
    (define a-benchmark (apply benchmark (read)))
    (define a-module-to-mutate (read))
    (install-configuration! (read))
    (define result
      (parity-check/binary-search a-benchmark
                                  a-module-to-mutate))
    (writeln result))

  (define (process-parity-check-main/exhaustive)
    (define a-benchmark (apply benchmark (read)))
    (define a-module-to-mutate (read))
    (install-configuration! (read))
    (define index-range (read))
    (define result
      (parity-check/exhaustive a-benchmark
                               a-module-to-mutate
                               index-range))
    (writeln result)))

(define configuration-path (make-parameter #f))

(define (process-checker-for a-benchmark
                             mod-to-mutate
                             mode
                             maybe-index-range)
  (define function-name
    (match mode
      ['quick
       'process-parity-check-main/binary-search]
      [(or 'exhaustive 'simple-exhaustive)
       'process-parity-check-main/exhaustive]))
  (match-define (list stdout stdin _ #f ctl)
    (process/ports #f #f (current-error-port)
                   @~a{
                       rt @;
                       -e '(require @;
                            (submod (file "@this-file-path") @;
                                    parity-check))' @;
                       -e '(@function-name)'
                       }))
  (writeln (match a-benchmark
             [(benchmark t ut base both)
              (list (map path->string t)
                    (map path->string ut)
                    (if base (path->string base) base)
                    (if both (path->string both) both))])
           stdin)
  (writeln mod-to-mutate stdin)
  (writeln (configuration-path) stdin)
  (match mode
    [(or 'exhaustive 'simple-exhaustive) (writeln maybe-index-range stdin)]
    ['quick      (void)])
  (close-output-port stdin)
  (values stdout ctl))




(define-logger parity)
(define-runtime-path this-file-path "verify-mutation-parity.rkt")
(struct mod-checker (benchmark-name mod-name id))
(struct parity-checker-process (stdout
                                spawn-ms
                                mode
                                checker))
(struct done (result))
(require 'common)

(define (process-will:report-mutant-parity current-process-q
                                           info)
  (define stdout
    (parity-checker-process-stdout (process-info-data info)))
  (define mutated-id-results
    #;(let ([str (port->string stdout)])
        (displayln "output:")
        (displayln str)
        (displayln "---")
        (call-with-input-string str read))
    (read stdout))
  (close-input-port stdout)
  (report-parity-results! mutated-id-results
                          info
                          current-process-q))

(define (report-parity-results! result
                                the-process-info
                                current-process-q)
  (match-define (parity-checker-process stdout
                                        spawn-ms
                                        mode
                                        (and (mod-checker benchmark-name
                                                          mod-to-mutate
                                                          id)
                                             the-mod-checker))
    (process-info-data the-process-info))
  (define time-elapsed-mins
    (/ (- (current-inexact-milliseconds)
          spawn-ms)
       1000
       60))
  (define complete-msg
    @~a{
        @benchmark-name "@mod-to-mutate" [@id] complete @;
        in @time-elapsed-mins min
        })
  (match mode
    [(or 'quick 'simple-exhaustive)
     (report-parity-result! result
                            complete-msg)
     current-process-q]
    ['exhaustive
     (define new-q
       (record-parity-result current-process-q
                             the-mod-checker
                             result))
     (maybe-report-exhaustive-parity-result! new-q
                                             the-mod-checker
                                             complete-msg)]))

#;(module+ test
  (define a-quick-process-info
    (process-info (parity-checker-process #f
                                          0
                                          'quick
                                          (mod-checker "benchmark"
                                                       "mod-to-mutate.rkt"
                                                       -1))
                  (const #f)
                  (const #f)))
  (define (make-exhaustive-process-info id)
    (process-info (parity-checker-process #f
                                          0
                                          'exhaustive
                                          (mod-checker "benchmark"
                                                       "mod-to-mutate.rkt"
                                                       id))
                  (const #f)
                  (const #f)))

  (require (submod "../experiment/mutant-factory-test.rkt" mock))
  (log-parity-info "1")
  (void (report-parity-results! (result 30 (exhausted))
                                a-quick-process-info
                                (make-mock-Q (hash))))
  (log-parity-info "2")
  (void (report-parity-results! (result 30 (list 'a 'b))
                                a-quick-process-info
                                (make-mock-Q (hash))))
  (log-parity-info "3")
  (define q2 (report-parity-results! (result 30 (exhausted))
                                     (make-exhaustive-process-info 3)
                                     (make-mock-Q (hash))))
  (process-queue-get-data q2)
  (log-parity-info "4")
  (define q3 (for/fold ([q (make-mock-Q (hash))])
                       ([rng (in-list index-ranges)]
                        [i (in-naturals)])
               (report-parity-results! (result (first rng) (exhausted))
                                       (make-exhaustive-process-info i)
                                       q)))
  (process-queue-get-data q3)
  (log-parity-info "5")
  (define q4 (for/fold ([q (make-mock-Q (hash))])
                       ([rng (in-list index-ranges)]
                        [i (in-naturals)])
               (report-parity-results!
                (result (cond [(< i 3) (second rng)]
                              [(= i 3)
                               (define result (+ (first rng) 10))
                               (displayln result)
                               result]
                              [else (first rng)])
                        (exhausted))
                (make-exhaustive-process-info i)
                q)))
  (process-queue-get-data q4))

(define (report-parity-result! the-result prefix)
  (match the-result
    [(result index (exhausted))
     (log-parity-info @~a{@prefix : ok, max index @index})]
    [(result index (list typed-mutated untyped-mutated))
     (log-parity-info @~a{@prefix : divergence found})
     (displayln @~a{
                    @prefix
                    Typed/untyped disagree starting at mutation index @index
                    Typed mutates:   @~v[typed-mutated]
                    Untyped mutates: @~v[untyped-mutated]

                    })]))

;; process-queue-data:
;; (hash/c (list/c benchmark-name? mod-name?) (hash/c id? result?))
(define (record-parity-result current-process-q
                              the-mod-checker
                              result)
  (match-define (mod-checker benchmark-name
                             mod-to-mutate
                             id)
    the-mod-checker)
  (define results-map (process-queue-get-data current-process-q))
  (define new-results-map
    (hash-update results-map
                 (list benchmark-name mod-to-mutate)
                 (λ (mod-results)
                   (hash-set mod-results id result))
                 (hash)))
  (process-queue-set-data current-process-q
                      new-results-map))
(define (maybe-report-exhaustive-parity-result! current-process-q
                                                the-mod-checker
                                                prefix)
  (match-define (mod-checker benchmark-name
                             mod-to-mutate
                             id)
    the-mod-checker)
  (define mod-results (hash-ref (process-queue-get-data current-process-q)
                                (list benchmark-name mod-to-mutate)))
  (match mod-results
    [(hash-table [ids results] ...)
     #:when (all-ids-present? ids)
     (match results
       [(list (result _ (exhausted)) ...)
        ;; ok: max index
        (define max-index-result
          (or (result-with-max-index ids results)
              (first results)))
        (report-parity-result! max-index-result
                               prefix)
        current-process-q]
       [results-with-divergences
        (define divergences (filter (match-lambda [(result _ (? list?)) #t]
                                                  [_ #f])
                                    results-with-divergences))
        (when (empty? divergences)
          (raise-user-error
           'maybe-report-exhaustive-parity-result!
           @~a{
               Results for @benchmark-name @mod-to-mutate [@id] @;
               not of expected shape. Should have at least one @;
               divergence, but apparently does not:
               @(pretty-format results-with-divergences)

               }))
        (define lowest-divergence
          (argmin result-index divergences))
        (report-parity-result! lowest-divergence
                               prefix)
        current-process-q])]
    [else current-process-q]))

(define (result-with-max-index ids results)
  (match-define (list (list sorted-ids sorted-results) ...)
    (sort (map list ids results)
          <
          #:key first))
  (for/first* ([id (in-list sorted-ids)]
               [result (in-list sorted-results)])
              (and (not (= (result-index result) (id-range-max id)))
                   result)))

(define (id-range-max id)
  (second (list-ref index-ranges id)))

(define (all-ids-present? list-of-ids)
  (equal? (sort list-of-ids <)
          (build-list (length index-ranges) values)))


(require 'parity-check)
(define (verify-mutation-parity-for a-benchmark process-q mode)
  (define benchmark-name (benchmark->name a-benchmark))

  (define (enq-mutant-parity-checker-for current-process-q
                                         mod-to-mutate)
    (define max-id (sub1 (length index-ranges)))
    (define (make-process-checker-spawner id index-range)
      (thunk
       (log-parity-info
        @~a{
            Spawning checker [@id / @max-id] @;
            for @benchmark-name "@mod-to-mutate"
            })
       (define-values {stdout ctl}
         (process-checker-for a-benchmark
                              mod-to-mutate
                              mode
                              index-range))
       (process-info (parity-checker-process stdout
                                             (current-inexact-milliseconds)
                                             mode
                                             (mod-checker benchmark-name
                                                          mod-to-mutate
                                                          id))
                     ctl
                     process-will:report-mutant-parity)))
    (match mode
      ['quick
       (process-queue-enqueue current-process-q
                              (make-process-checker-spawner -1 #f))]
      ['exhaustive
       (for/fold ([q current-process-q])
                 ([index-range (in-list index-ranges)]
                  [i (in-naturals)])
         (process-queue-enqueue q
                                (make-process-checker-spawner i index-range)))]
      ['simple-exhaustive
       (process-queue-enqueue
        current-process-q
        (make-process-checker-spawner -2 (list 0 INDEX-SEARCH-RANGE)))]))

  (for/fold ([current-process-q process-q])
            ([mod-path (in-list (benchmark-typed a-benchmark))])
    (define mod-to-mutate (file-name-string-from-path mod-path))

    (enq-mutant-parity-checker-for current-process-q
                                   mod-to-mutate)))

(define (verify-benchmarks-mutation-parity list-of-benchmarks
                                           process-q
                                           mode)
  (for/fold ([current-process-q process-q])
            ([a-benchmark list-of-benchmarks])
    (verify-mutation-parity-for a-benchmark current-process-q mode)))

(define (verify-mutation-parity-of-all-benchmarks-in dir
                                                     process-q
                                                     mode)
  (define benchmarks
    (for/list ([a-benchmark-path (in-list (directory-list dir #:build? #t))]
               #:when (directory-exists? a-benchmark-path))
      (read-benchmark a-benchmark-path)))
  (verify-benchmarks-mutation-parity benchmarks
                                     process-q
                                     mode))

(main
 #:arguments {[flags (list benchmarks-dir)]
              #:once-each
              [("-n" "--processes")
               'procs
               ("Specify the number of parallel processes to use."
                "Default: 1")
               #:collect ["N" take-latest "1"]]
              [("-e" "--exhaustive")
               'exhaustive?
               ("Perform an exhaustive parity comparison."
                "By default, performs a quick search that may be conservative.")
               #:record]
              [("-E" "--simple-exhaustive")
               'simple-exhaustive?
               ("Like --exhaustive, but use a very simple search strategy that"
                "may be slower than --exhaustive.")
               #:conflicts '(exhaustive?)
               #:record]
              [("-c" "--config")
               'config
               ("Configuration specifying the mutators to use.")
               #:collect ["path" (set-parameter configuration-path) #f]
               #:mandatory]
              #:args [benchmarks-dir]}
 (file-stream-buffer-mode (current-output-port) 'line)
 (install-configuration! (configuration-path))
 (define n-processes (string->number (hash-ref flags 'procs)))
 (define mode (cond [(hash-ref flags 'exhaustive?)
                     'exhaustive]
                    [(hash-ref flags 'simple-exhaustive?)
                     'simple-exhaustive]
                    [else 'quick]))
 (define start-ms (current-inexact-milliseconds))
 (define q
   (verify-mutation-parity-of-all-benchmarks-in
    benchmarks-dir
    (make-process-queue n-processes
                    (hash))
    mode))
 (void (process-queue-wait q))
 (define end-ms (current-inexact-milliseconds))
 (define total-minutes (/ (- end-ms start-ms) 1000 60))
 (log-parity-info @~a{Analysis complete in @total-minutes min}))

(module test racket/base)
