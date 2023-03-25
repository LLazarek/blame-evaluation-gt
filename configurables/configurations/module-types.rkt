#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "../../util/ctc-utils.rkt")
(provide serialize-config
         deserialize-config
         config-at-max-precision-for?
         increment-config-precision-for

         benchmark->mutatable-modules
         benchmark-configuration->program
         (contract-out
          [make-max-bench-config
           (->i ([bench benchmark/c])
                [result {bench}
                        (config-for-benchmark/c bench)])]
          [configure-benchmark
           (->i ([bench benchmark/c]
                 [config {bench}
                         (config-for-benchmark/c bench)])
                [result benchmark-configuration/c])]))

(require "../../configurations/configure-benchmark.rkt"
         "../../configurations/config.rkt"
         "../../util/path-utils.rkt"
         "../../util/experiment-exns.rkt"
         "../../util/program.rkt"
         "common.rkt")

(define/contract (config-for-benchmark/c b)
  (benchmark/c . -> . (any/c . -> . boolean?))

  (simple-flat-contract-with-explanation
   (and/c config/c
          (λ (config)
            (equal? (sort-file-names (map file-name-string-from-path
                                          (benchmark-typed b)))
                    (sort-file-names (hash-keys config)))))
   @~a{a config for @~v[b]}))

(define (sort-file-names names)
  (sort names string<?))

(define (config-at-max-precision-for? name config)
  (equal? (hash-ref config name) 'types))

(define (increment-config-precision-for name config
                                        #:increment-max-error?
                                        [error-if-already-types? #t])
  (match (hash-ref config name)
    ['none (hash-set config name 'types)]
    [else
     #:when error-if-already-types?
     (error 'increment-config-precision-for
            @~a{Given config with value @name already at types: @config})]
    [else config]))

(define (benchmark->module-names a-benchmark #:include-both? [include-both? #t])
  (map file-name-string-from-path
       (append (if include-both?
                   (benchmark-both->files (benchmark-both a-benchmark))
                   empty)
               (benchmark-typed a-benchmark))))

;; Produces the names of the mutatable modules in `a-benchmark`
(define (benchmark->mutatable-modules a-benchmark)
  (benchmark->module-names a-benchmark #:include-both? #t))

(define-values {level->digit digit->level}
  (1-to-1-map->converters 'types #\1
                          'none  #\0))
(define serialize-config (make-config-serializer level->digit))
(define deserialize-config
  (make-config-deserializer digit->level
                            (λ (b)
                              (benchmark->module-names b #:include-both? #f))))


(module+ test
  (require ruinit
           (submod "../../configurations/configure-benchmark.rkt" test-env))

  (test-begin
    #:name test:increment-config-precision-for
    (test-equal? (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'none
                        "bazzle.rkt" 'types))
                 (hash "baz.rkt" 'types
                       "bazzle.rkt" 'types))
    (test-exn exn:fail?
              (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'types
                        "bazzle.rkt" 'types))))

  (test-begin
    #:name config-at-max-precision-for?
    (not/test (config-at-max-precision-for?
               "baz.rkt"
               (hash "baz.rkt" 'none
                     "bazzle.rkt" 'types)))
    (config-at-max-precision-for?
     "baz.rkt"
     (hash "baz.rkt" 'types
           "bazzle.rkt" 'types)))

  (test-begin
    #:name serialize/deserialize-config
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)
    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (serialize-config #hash(("main.rkt" . types)
                                         ("a.rkt" . none)
                                         ("b.rkt" . types)))
                 11)
    (test-equal? (serialize-config #hash(("main.rkt" . none)
                                         ("a.rkt" . none)
                                         ("b.rkt" . types)))
                 10)
    (test-equal? (serialize-config #hash(("main.rkt" . none)
                                         ("a.rkt" . types)
                                         ("b.rkt" . none)))
                 100)
    (ignore (define-simple-test (test-round-trip config)
              (test-equal? (deserialize-config (serialize-config config)
                                               #:benchmark a-benchmark)
                           config)))
    (test-round-trip #hash(("main.rkt" . types) ("a.rkt" . none) ("b.rkt" . types)))
    (test-round-trip #hash(("main.rkt" . none) ("a.rkt" . none) ("b.rkt" . types)))
    (test-round-trip #hash(("main.rkt" . none) ("a.rkt" . types) ("b.rkt" . none)))))






(define (configure-benchmark bench config)
  (match-define (benchmark typed untyped base both)
    bench)
  (define configured-files
    (for/list ([(file level) (in-hash config)])
      (define configured-file
        (pick-file-by-name (match level
                             ['none untyped]
                             ['types typed])
                           file))
      (or configured-file
          (raise-experiment-user-error
           'configure-benchmark
           @~a{Unable to find module in benchmark corresponding to name in config for @file}))))
  (match-define-values {(list main) others}
                       (partition (path-ends-with "main.rkt")
                                  configured-files))
  (define adapters (benchmark-both->files both))
  (benchmark-configuration main
                           (append others
                                   adapters)
                           base
                           config))

(define (benchmark-both->files both)
  (match both
    [#f '()]
    [dir (directory-list dir #:build? #t)]))

(define (benchmark-configuration->program c-bench)
  (make-program (benchmark-configuration-main c-bench)
                (benchmark-configuration-others c-bench)))


(define (make-max-bench-config a-benchmark)
  (define mods (benchmark->module-names a-benchmark #:include-both? #f))
  (for/hash ([mod (in-list mods)])
    (values mod 'types)))

(module+ test
  (require (submod "../../configurations/configure-benchmark.rkt" test-env))
  (test-begin
    #:name configure-benchmark
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir))
            (define config-1 (hash (file-name-string-from-path main) 'none
                                   (file-name-string-from-path a) 'none
                                   (file-name-string-from-path b) 'none)))
    (test-match (configure-benchmark a-benchmark
                                     config-1)
                (benchmark-configuration (== main paths=?)
                                         (list-no-order (== a paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== config-1)))
    (ignore (define a-types-config (hash-set config-1
                                             (file-name-string-from-path a)
                                             'types)))
    (test-match (configure-benchmark a-benchmark
                                     a-types-config)
                (benchmark-configuration (== main paths=?)
                                         (list-no-order (== a/t paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== a-types-config)))
    (ignore (define a-main-types-config (hash-set a-types-config
                                                  (file-name-string-from-path main)
                                                  'types)))
    (test-match (configure-benchmark a-benchmark
                                     a-main-types-config)
                (benchmark-configuration (== main/t paths=?)
                                         (list-no-order (== a/t paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== a-main-types-config))))

  (test-begin
    #:name make-max-bench-config
    (test-equal? (make-max-bench-config (benchmark '("b/typed/a.rkt" "b/typed/b.rkt")
                                                   '("b/typed/a.rkt" "b/typed/b.rkt")
                                                   "b/base"
                                                   #f))
                 (hash "a.rkt" 'types
                       "b.rkt" 'types)))

  (test-begin
    #:name benchmark->mutatable-modules
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (list->set (benchmark->mutatable-modules a-benchmark))
                 (set "main.rkt" "a.rkt" "b.rkt" "adapter.rkt"))))
