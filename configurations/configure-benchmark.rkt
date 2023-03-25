#lang at-exp racket

(require "../util/optional-contracts.rkt"
         "../util/ctc-utils.rkt"
         "../util/experiment-exns.rkt")
(provide (contract-out
          [read-benchmark
           (path-to-existant-directory? . -> . (or/c #f benchmark/c))]
          [benchmark->name
           ((or/c benchmark/c benchmark-configuration/c) . -> . string?)]
          [benchmark->program/no-common
           (benchmark/c . -> . program/c)]
          [benchmark-configuration->program
           (benchmark-configuration/c . -> . program/c)]

          [benchmark->mutatable-modules
           (benchmark/c . -> . (listof string?))]
          [make-max-bench-config
           (->i ([bench benchmark/c])
                [result {bench}
                        config/c])]
          [configure-benchmark
           (->i ([bench benchmark/c]
                 [config {bench}
                         config/c])
                #:pre/desc {bench}
                (or (->bool (findf (path-ends-with "main.rkt")
                                   (benchmark-typed bench)))
                    "Benchmark does not have a main.rkt module.")
                [result benchmark-configuration/c])]
          [serialize-benchmark-configuration
           (benchmark-configuration/c . -> . string?)])

         (struct-out benchmark-configuration)
         (struct-out benchmark)
         benchmark-configuration/c
         benchmark/c)

(require "config.rkt"
         "../util/path-utils.rkt"
         "../util/program.rkt"
         "../configurables/configurables.rkt")

(struct benchmark (typed untyped base both)
  #:prefab)
(struct benchmark-configuration (main others base-dir config)
  #:prefab)

(define benchmark/c
  (struct/c benchmark
            (listof path-string?)
            (listof path-string?)
            (or/c #f path-string?)
            (or/c #f path-string?)))
(define benchmark-configuration/c
  (struct/c benchmark-configuration
            path-string?
            (listof path-string?)
            (or/c #f path-string?)
            config/c))

#;(define benchmark-cache
  (make-weak-hash '()))
(define (read-benchmark path)
  (cond
    #;[(hash-ref benchmark-cache path #f) => values]
    [else
     (define-values {typed untyped}
       (read-typed-untyped-dirs path))
     (define base (simple-form-path (build-path path "base")))
     (define both (simple-form-path (build-path path "both")))
     (match (and typed untyped)
       [#f #f]
       [else
        (define result
          (benchmark typed
                     untyped
                     (and (directory-exists? base) base)
                     (and (directory-exists? both) both)))
        #;(hash-set! benchmark-cache path result)
        result])]))

(define (has-.rkt-extension? path)
  (path-has-extension? path ".rkt"))

;; path-string? -> (values (listof path?) (listof path?))
(define (read-typed-untyped-dirs path)
  (apply values
         (for/list ([dir (in-list '("typed" "untyped"))])
           (define dir-path (build-path path dir))
           (cond [(directory-exists? dir-path)
                  (define all-files
                    (map simple-form-path
                         (directory-list dir-path
                                         #:build? #t)))
                  (filter has-.rkt-extension?
                          all-files)]
                 [else #f]))))

(define (benchmark->name b)
  (match b
    [(struct* benchmark
              ([typed (list* (app explode-path/string
                                  (list _ ... name "typed" _))
                             _)]))
     name]
    [(struct* benchmark-configuration
              ([main (app explode-path/string
                          (list _ ... name (or "typed" "untyped") _))]))
     name]))


(define (benchmark->program/no-common bench)
  (define mods (benchmark-typed bench))
  (define main (pick-file-by-name mods "main.rkt"))
  (unless main
    (raise-internal-experiment-argument-error
     'benchmark->program/no-common
     "a benchmark with standard structure (i.e. having a main module named `main.rkt`)"
     bench))
  (make-program main (remove main mods)))

(module test-env racket
  (provide (all-defined-out))
  (require ruinit)
  (define-test-env
    [setup! cleanup!]
    #:directories ([test-temp "./test-temp"]
                   [a-benchmark-dir "./test-temp/a-benchmark"]
                   [typed "./test-temp/a-benchmark/typed"]
                   [untyped "./test-temp/a-benchmark/untyped"]
                   [both "./test-temp/a-benchmark/both"]
                   [base "./test-temp/a-benchmark/base"]

                   [not-a-benchmark "./test-temp/not-a-benchmark"])
    #:files ([main/t  (build-path typed "main.rkt")   "#lang typed/racket main"]
             [a/t     (build-path typed "a.rkt")      "#lang typed/racket a"]
             [b/t     (build-path typed "b.rkt")      "#lang typed/racket b"]
             [garbage (build-path typed "garbage")    "some dumb garbage"]
             [main    (build-path untyped "main.rkt") "#lang racket main"]
             [a       (build-path untyped "a.rkt")    "#lang racket a"]
             [b       (build-path untyped "b.rkt")    "#lang racket b"]
             [adapter (build-path both "adapter.rkt") "#lang typed/racket adapter"])))

(module+ test
  (require ruinit
           (submod ".." test-env))
  (test-begin
    #:name read-typed-untyped-dirs
    #:before (setup!)
    #:after (cleanup!)
    (ignore (define-values {t ut} (read-typed-untyped-dirs a-benchmark-dir)))
    (test-equal? t
                 (list a/t b/t main/t))
    (test-equal? ut
                 (list a b main)))

  (test-begin
    #:name read-benchmark
    #:before (setup!)
    #:after (cleanup!)
    (test-match (read-benchmark not-a-benchmark)
                #f)
    (test-equal? (read-benchmark a-benchmark-dir)
                 (benchmark (list a/t b/t main/t)
                            (list a b main)
                            (simple-form-path base)
                            (simple-form-path both))
                #;(benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           base
                           both))
    (ignore (delete-directory/files base))
    (test-match (read-benchmark a-benchmark-dir)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           #f
                           (== (simple-form-path both))))
    (ignore (delete-directory/files both))
    (test-match (read-benchmark a-benchmark-dir)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           #f
                           #f))

    ;; read-benchmark doesn't check for equivalence of typed/untyped
    (ignore (delete-file a))
    (test-match (read-benchmark a-benchmark-dir)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== b))
                           #f
                           #f))

    ;; it does check that both dirs are there though
    (ignore (delete-directory/files typed))
    (test-match (read-benchmark a-benchmark-dir)
                #f))

  (test-begin
    #:name benchmark->program/no-common
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)
    (test-equal? (benchmark->program/no-common (benchmark (list main/t a/t b/t)
                                                          (list main a b)
                                                          #f
                                                          #f))
                 (make-program main/t
                               (list a/t b/t)))))

(define (benchmark-configuration->program c-bench)
  ((configured:benchmark-configuration->program) c-bench))
(define (benchmark->mutatable-modules a-benchmark)
  ((configured:benchmark->mutatable-modules) a-benchmark))
(define (make-max-bench-config a-benchmark)
  ((configured:make-max-bench-config) a-benchmark))
(define (configure-benchmark bench config)
  ((configured:configure-benchmark) bench config))

(define (serialize-benchmark-configuration a-benchmark-configuration)
  (match-define (benchmark-configuration main-path others-paths base-dir-path config)
    a-benchmark-configuration)
  (~s (benchmark-configuration (~a main-path)
                               (map ~a others-paths)
                               (~a base-dir-path)
                               config)))
