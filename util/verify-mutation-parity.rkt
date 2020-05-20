#lang at-exp rscript

(provide mutated-id-for-typed/untyped-agrees?)

(require "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../configurations/configure-benchmark.rkt"
         "../runner/program.rkt"
         "../mutate/mutate-program.rkt"
         "../process-q/functional.rkt"
         "../process-q/interface.rkt"
         "for-first-star.rkt")

(module parity-check rscript
  (provide (struct-out no-more-mutants)
           place-parity-check-main
           benchmark->name
           mutated-id-for-typed/untyped-agrees?)

  (require "../runner/mutation-runner.rkt"
           "../util/path-utils.rkt"
           "../configurations/configure-benchmark.rkt"
           "../runner/program.rkt"
           "../mutate/mutate-program.rkt")

  (define (benchmark->name a-benchmark)
    (match (benchmark-typed a-benchmark)
      [(list* (app explode-path/string
                   (list _ ... name "typed" _)) _)
       name]))

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
      [other other]))

  (define (place-parity-check-main pch)
    (define a-benchmark (apply benchmark (place-channel-get pch)))
    (define a-module-to-mutate (place-channel-get pch))
    (define mutation-index (place-channel-get pch))
    (define result
      (mutated-id-for-typed/untyped-agrees? a-benchmark
                                            a-module-to-mutate
                                            mutation-index))
    (place-channel-put pch result)))

(require 'parity-check)

(define (verify-mutation-parity-for a-benchmark process-q)
  (define (enq-mutant-parity-checker-for current-process-q
                                         mod-to-mutate
                                         i)
    (define (will:check-mutant-parity new-process-q
                                      info)
      (define mutated-id-results
        (place-channel-get (process-info-data info)))
      (if (done-checking-mutated-id-results-for? a-benchmark
                                                 mod-to-mutate
                                                 i
                                                 mutated-id-results)
          new-process-q
          (enq-mutant-parity-checker-for new-process-q
                                         mod-to-mutate
                                         (add1 i))))
    (process-Q-enq
     current-process-q
     (thunk
      (define the-place
        (place-process-checker-for a-benchmark
                                   mod-to-mutate
                                   i))
      (process-info the-place
                    (match-lambda
                      ['status
                       (and (sync/timeout 0 (place-dead-evt the-place))
                            #t)]
                      ['wait (place-wait the-place)]
                      [other (place-kill the-place)])
                    will:check-mutant-parity))))

  (define (done-checking-mutated-id-results-for? a-benchmark
                                                 mod-to-mutate
                                                 i
                                                 mutated-id-results)
    (match mutated-id-results
      [(no-more-mutants)
       (eprintf @~a{
                    Done with @(benchmark->name a-benchmark) @;
                    @mod-to-mutate
                    })
       #t]
      [(list typed-mutated untyped-mutated)
       (displayln
        @~a{
            Typed/untyped disagree in @(benchmark->name a-benchmark) @;
            module @mod-to-mutate starting at mutation index @i
            Typed mutates:   @typed-mutated
            Untyped mutates: @untyped-mutated

            })
       #t]
      [#t #f]))

  (define (place-process-checker-for a-benchmark
                                     mod-to-mutate
                                     i)
    (define pch
      (dynamic-place '(submod "verify-mutation-parity.rkt" parity-check)
                     'place-parity-check-main))
    (place-channel-put pch
                       (match a-benchmark
                         [(benchmark t ut base both)
                          (list t ut base both)]))
    (place-channel-put pch mod-to-mutate)
    (place-channel-put pch i)
    pch)

  (for/fold ([current-process-q process-q])
            ([mod-path (in-list (benchmark-typed a-benchmark))])
    (define mod-to-mutate (file-name-string-from-path mod-path))
    (enq-mutant-parity-checker-for current-process-q
                                   mod-to-mutate
                                   0)))

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
 (define q
   (verify-mutation-parity-of-all-benchmarks-in
    benchmarks-dir
    (make-process-Q n-processes
                    #f)))
 (process-Q-wait q))
