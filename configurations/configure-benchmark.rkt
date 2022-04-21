#lang at-exp racket

(require "../util/optional-contracts.rkt"
         "../util/experiment-exns.rkt")
(provide (contract-out
          [read-benchmark
           (path-to-existant-directory? . -> . (or/c #f benchmark/c))]
          [benchmark->mutatable-modules
           ({benchmark/c} {#:include-both? boolean?} . ->* . (listof string?))]
          [benchmark->name
           (benchmark/c . -> . string?)]
          [make-max-bench-config
           (->i ([bench benchmark/c])
                [result {bench}
                        (and/c config/c
                               (config-for-benchmark/c bench))])]
          [configure-benchmark
           (->i ([bench benchmark/c]
                 [config {bench}
                         (and/c config/c
                                (config-for-benchmark/c bench))])
                #:pre/desc {bench}
                (or (->bool (findf (path-ends-with "main.rkt")
                                   (benchmark-typed bench)))
                    "Benchmark does not have a main.rkt module.")
                [result benchmark-configuration/c])]

          [benchmark->program/no-common
           (benchmark/c . -> . program/c)]
          [benchmark-configuration->program
           (benchmark-configuration/c . -> . program/c)])

         (struct-out benchmark-configuration)
         (struct-out benchmark)
         benchmark-configuration/c
         benchmark/c)

(require "config.rkt"
         "../util/path-utils.rkt"
         "../util/ctc-utils.rkt"
         "../util/program.rkt")

(struct benchmark-configuration (main others base-dir config)
  #:transparent)
(define benchmark-configuration/c
  (struct/c benchmark-configuration
            path-string?
            (listof path-string?)
            (or/c #f path-string?)
            config/c))

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


(define (sort-file-names names)
  (sort names string<?))

(define (sorted-files-in p)
  (sort-file-names (map path->string (directory-list p))))


(struct benchmark (typed untyped base both)
  #:transparent)
(define benchmark/c (struct/c benchmark
                              (listof path-string?)
                              (listof path-string?)
                              (or/c #f path-string?)
                              (or/c #f path-string?)))

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

(define (benchmark-both->files both)
  (match both
    [#f '()]
    [dir (directory-list dir #:build? #t)]))

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

(define (path-to-benchmark-directory? path)
  (define b (read-benchmark path))
  (and b
       (equal? (sorted-files-in (benchmark-typed b))
               (sorted-files-in (benchmark-untyped b)))))

(define/contract (config-for-benchmark/c b)
  (benchmark/c . -> . (config/c . -> . boolean?))

  (simple-flat-contract-with-explanation
   (λ (config)
     (equal? (sort-file-names (map file-name-string-from-path
                                   (benchmark-typed b)))
             (sort-file-names (hash-keys config))))
   @~a{a config for @~v[b]}))

;; Produces the names of the mutatable modules in `a-benchmark`
(define (benchmark->mutatable-modules a-benchmark #:include-both? [include-both? #t])
  (map file-name-string-from-path
       (append (if include-both?
                   (benchmark-both->files (benchmark-both a-benchmark))
                   empty)
               (benchmark-typed a-benchmark))))

(define (make-max-bench-config a-benchmark)
  (define mods (benchmark->mutatable-modules a-benchmark #:include-both? #f))
  (for/hash ([mod (in-list mods)])
    (values mod 'types)))

(define (benchmark->name a-benchmark)
  (match (benchmark-typed a-benchmark)
    [(list* (app explode-path/string
                 (list _ ... name "typed" _)) _)
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

(define (benchmark-configuration->program c-bench)
  (make-program (benchmark-configuration-main c-bench)
                (benchmark-configuration-others c-bench)))

(module+ test
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
             [adapter (build-path both "adapter.rkt") "#lang typed/racket adapter"]))
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
    #:name benchmark->program/no-common
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)
    (test-equal? (benchmark->program/no-common (benchmark (list main/t a/t b/t)
                                                          (list main a b)
                                                          #f
                                                          #f))
                 (make-program main/t
                               (list a/t b/t))))

  (test-begin
    #:name benchmark->mutatable-modules
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (list->set (benchmark->mutatable-modules a-benchmark))
                 (set "main.rkt" "a.rkt" "b.rkt" "adapter.rkt"))))
