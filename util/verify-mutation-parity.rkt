#lang at-exp rscript

(provide mutated-id-for-typed/untyped-agrees?)

(require "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../configurations/configure-benchmark.rkt"
         "../runner/program.rkt"
         "../mutate/mutate-program.rkt"
         "for-first-star.rkt")

(struct no-more-mutants ())
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

(define (verify-mutation-parity-for a-benchmark)
  (for ([mod-path (in-list (benchmark-typed a-benchmark))])
    (define mod-to-mutate (file-name-string-from-path mod-path))
    (for*/first*
     ([i (in-naturals)])

     (eprintf
      @~a{
          Checking @(benchmark->name a-benchmark) @mod-to-mutate @i @;
          @(make-string 50 #\space) @"\r"
          })
     (define mutated-id-results
       (mutated-id-for-typed/untyped-agrees? a-benchmark
                                             mod-to-mutate
                                             i))
     (match mutated-id-results
       [(no-more-mutants) #t]
       [(list typed-mutated untyped-mutated)
        (displayln
         @~a{
             Typed/untyped disagree in @(benchmark->name a-benchmark) @;
             module @mod-to-mutate starting at mutation index @i
             Typed mutates:   @typed-mutated
             Untyped mutates: @untyped-mutated

             })
        #t]
       [#t #f]))))

(define (benchmark->name a-benchmark)
  (match (benchmark-typed a-benchmark)
    [(list* (app explode-path/string
                 (list _ ... name "typed" _)) _)
     name]))

(define (verify-benchmarks-mutation-parity list-of-benchmarks)
  (for ([a-benchmark list-of-benchmarks])
    (verify-mutation-parity-for a-benchmark)))

(define (verify-mutation-parity-of-all-benchmarks-in dir)
  (define benchmarks
    (for/list ([a-benchmark-path (in-list (directory-list dir #:build? #t))]
               #:when (directory-exists? a-benchmark-path))
      (read-benchmark a-benchmark-path)))
  (verify-benchmarks-mutation-parity benchmarks))

(main
 #:arguments {[flags (list benchmarks-dir)]
              #:args [benchmarks-dir]}
 (file-stream-buffer-mode (current-output-port) 'line)
 (verify-mutation-parity-of-all-benchmarks-in benchmarks-dir))
