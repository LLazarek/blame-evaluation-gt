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
           (struct-out exhausted)
           index-ranges)

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
                  (result 2 "2"))))

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
           "../mutate/mutate-program.rkt"
           "../mutate/logger.rkt"
           "../mutate/expression-selectors.rkt"
           "../runner/mutation-runner.rkt"
           "../runner/program.rkt"
           "../util/path-utils.rkt"
           "for-first-star.rkt"
           (submod ".." common)
           racket/logging
           syntax/parse)

  (struct no-more-mutants () #:prefab)
  (define (extract-mutation stx index)
    (define mutated-expr (box #f))
    (define mutated-id
      (with-intercepted-logging
        (match-lambda
          [(vector _ _ (list before after) _)
           (set-box! mutated-expr (list before after))]
          [other (void)])
        (thunk
         (define-values {_ id}
           (mutate-module stx index))
         id)
        #:logger mutate-logger
        'info))
    (define mutated-expr/annotations-stripped
      (strip-annotations (unbox mutated-expr)))
    (list mutated-id
          mutated-expr/annotations-stripped))

  (define (strip-annotations mutated-expr)
    (define stripped-expr (first (select-exprs-as-if-untyped mutated-expr)))
    (match (syntax->list stripped-expr)
      [(list lone-subexpr)
       #:when (syntax-parse mutated-expr
                [[name:id {~datum :} T] #t]
                [else #f])
       (strip-annotations lone-subexpr)]
      [(? list? subexprs)
       (map strip-annotations subexprs)]
      [#f (syntax->datum stripped-expr)]))

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
    (define result
      (parity-check/binary-search a-benchmark
                                  a-module-to-mutate))
    (writeln result))

  (define (process-parity-check-main/exhaustive)
    (define a-benchmark (apply benchmark (read)))
    (define a-module-to-mutate (read))
    (define index-range (read))
    (define result
      (parity-check/exhaustive a-benchmark
                               a-module-to-mutate
                               index-range))
    (writeln result)))

(define (process-checker-for a-benchmark
                             mod-to-mutate
                             mode
                             maybe-index-range)
  (define function-name
    (match mode
      ['quick      'process-parity-check-main/binary-search]
      ['exhaustive 'process-parity-check-main/exhaustive]))
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
  (match mode
    ['exhaustive (writeln maybe-index-range stdin)]
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
    ['quick
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

(module+ test
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
  (process-Q-get-data q2)
  (log-parity-info "4")
  (define q3 (for/fold ([q (make-mock-Q (hash))])
                       ([rng (in-list index-ranges)]
                        [i (in-naturals)])
               (report-parity-results! (result (first rng) (exhausted))
                                       (make-exhaustive-process-info i)
                                       q)))
  (process-Q-get-data q3)
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
  (process-Q-get-data q4))

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

;; process-Q-data:
;; (hash/c (list/c benchmark-name? mod-name?) (hash/c id? result?))
(define (record-parity-result current-process-q
                              the-mod-checker
                              result)
  (match-define (mod-checker benchmark-name
                             mod-to-mutate
                             id)
    the-mod-checker)
  (define results-map (process-Q-get-data current-process-q))
  (define new-results-map
    (hash-update results-map
                 (list benchmark-name mod-to-mutate)
                 (λ (mod-results)
                   (hash-set mod-results id result))
                 (hash)))
  (process-Q-set-data current-process-q
                      new-results-map))
(define (maybe-report-exhaustive-parity-result! current-process-q
                                                the-mod-checker
                                                prefix)
  (match-define (mod-checker benchmark-name
                             mod-to-mutate
                             id)
    the-mod-checker)
  (define mod-results (hash-ref (process-Q-get-data current-process-q)
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
       (process-Q-enq current-process-q
                      (make-process-checker-spawner -1 #f))]
      ['exhaustive
       (for/fold ([q current-process-q])
                 ([index-range (in-list index-ranges)]
                  [i (in-naturals)])
         (process-Q-enq q
                        (make-process-checker-spawner i index-range)))]))

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
              #:args [benchmarks-dir]}
 (file-stream-buffer-mode (current-output-port) 'line)
 (define n-processes (string->number (hash-ref flags 'procs)))
 (define mode (if (hash-ref flags 'exhaustive?) 'exhaustive 'quick))
 (define start-ms (current-inexact-milliseconds))
 (define q
   (verify-mutation-parity-of-all-benchmarks-in
    benchmarks-dir
    (make-process-Q n-processes
                    (hash))
    mode))
 (void (process-Q-wait q))
 (define end-ms (current-inexact-milliseconds))
 (define total-minutes (/ (- end-ms start-ms) 1000 60))
 (log-parity-info @~a{Analysis complete in @total-minutes min}))
