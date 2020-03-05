#lang at-exp racket

(provide (contract-out
          [configure-benchmark
           (->i ([dir path-to-benchmark-directory?]
                 [config (dir)
                         (and/c config/c
                                (config-for-benchmark? dir))])
                [result benchmark-configuration?])]))

(require "config.rkt")

(struct benchmark-configuration (main others base-dir)
  #:transparent)

(define (configure-benchmark benchmark-dir config)
  (match-define (benchmark typed untyped base both)
    (read-benchmark benchmark-dir))
  (define configured-files
    (for/list ([(file level) (in-hash config)])
      (pick-file-by-name (match level
                           ['none untyped]
                           ['types typed])
                         file)))
  (match-define-values {(list main) others}
                       (partition (path-ends-with "main.rkt")
                                  configured-files))
  (define adapters (match both
                     [#f '()]
                     [dir (directory-list dir #:build? #t)]))
  (benchmark-configuration main
                           (append others
                                   adapters)
                           base))


(define (file-name-string-from-path f)
  (path->string (file-name-from-path f)))

(define (pick-file-by-name files name)
  (for/first ([f (in-list files)]
              #:when (string=? (file-name-string-from-path f)
                               name))
    f))

(define (sort-file-names names)
  (sort names string<?))

(define (sorted-files-in p)
  (sort-file-names (map path->string (directory-list p))))

(define ((path-ends-with name) p)
  (define-values {_1 p-name _2} (split-path p))
  (string=? (path->string p-name)
            name))

(struct benchmark (typed untyped base both)
  #:transparent)

#;(define benchmark-cache
  (make-weak-hash '()))
(define/contract (read-benchmark path)
  (path-string? . -> . (or/c #f benchmark?))

  (cond
    #;[(hash-ref benchmark-cache path #f) => values]
    [(directory-exists? path)
     (define-values {typed untyped}
       (read-typed-untyped-dirs path))
     (define base (build-path path "base"))
     (define both (build-path path "both"))
     (match (and typed untyped)
       [#f #f]
       [else
        (define result
          (benchmark typed
                     untyped
                     (and (directory-exists? base) base)
                     (and (directory-exists? both) both)))
        #;(hash-set! benchmark-cache path result)
        result])]
    [else #f]))

(define (read-typed-untyped-dirs path)
  (apply values
         (for/list ([dir (in-list '("typed" "untyped"))])
           (define dir-path (build-path path dir))
           (and (directory-exists? dir-path)
                (directory-list dir-path
                                #:build? #t)))))

(define (path-to-benchmark-directory? path)
  (define b (read-benchmark path))
  (and b
       (equal? (sorted-files-in (benchmark-typed b))
               (sorted-files-in (benchmark-untyped b)))))

(define/contract ((config-for-benchmark? dir) config)
  (path-to-benchmark-directory? . -> . (config/c . -> . boolean?))

  (define b (read-benchmark dir))
  (and b
       (equal? (sorted-files-in (benchmark-typed b))
               (sort-file-names (hash-keys config)))))

(module+ test
  (require ruinit)
  (define-test-env
    [setup! cleanup!]
    #:directories ([test-temp "./test-temp"]
                   [a-benchmark "./test-temp/a-benchmark"]
                   [typed "./test-temp/a-benchmark/typed"]
                   [untyped "./test-temp/a-benchmark/untyped"]
                   [both "./test-temp/a-benchmark/both"]
                   [base "./test-temp/a-benchmark/base"]

                   [not-a-benchmark "./test-temp/not-a-benchmark"])
    #:files ([main/t  (build-path typed "main.rkt")   ""]
             [a/t     (build-path typed "a.rkt")      ""]
             [b/t     (build-path typed "b.rkt")      ""]
             [main    (build-path untyped "main.rkt") ""]
             [a       (build-path untyped "a.rkt")    ""]
             [b       (build-path untyped "b.rkt")    ""]
             [adapter (build-path both "adapter.rkt") ""]))
  (test-begin
    #:name read-benchmark
    #:before (setup!)
    #:after (cleanup!)
    (test-match (read-benchmark not-a-benchmark)
                #f)
    (test-match (read-benchmark a-benchmark)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           base
                           both))
    (ignore (delete-directory/files base))
    (test-match (read-benchmark a-benchmark)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           #f
                           both))
    (ignore (delete-directory/files both))
    (test-match (read-benchmark a-benchmark)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== a) (== b))
                           #f
                           #f))

    ;; read-benchmark doesn't check for equivalence of typed/untyped
    (ignore (delete-file a))
    (test-match (read-benchmark a-benchmark)
                (benchmark (list-no-order (== main/t) (== a/t) (== b/t))
                           (list-no-order (== main) (== b))
                           #f
                           #f))

    ;; it does check that both dirs are there though
    (ignore (delete-directory/files typed))
    (test-match (read-benchmark a-benchmark)
                #f))

  (test-begin
    #:name configure-benchmark
    #:before (setup!)
    #:after (cleanup!)

    (test-match (configure-benchmark a-benchmark
                                     (hash (file-name-string-from-path main) 'none
                                           (file-name-string-from-path a) 'none
                                           (file-name-string-from-path b) 'none))
                (benchmark-configuration (== main)
                                         (list-no-order (== a) (== b) (== adapter))
                                         (== base)))
    (test-match (configure-benchmark a-benchmark
                                     (hash (file-name-string-from-path main) 'none
                                           (file-name-string-from-path a) 'types
                                           (file-name-string-from-path b) 'none))
                (benchmark-configuration (== main)
                                         (list-no-order (== a/t) (== b) (== adapter))
                                         (== base)))
    (test-match (configure-benchmark a-benchmark
                                     (hash (file-name-string-from-path main) 'types
                                           (file-name-string-from-path a) 'types
                                           (file-name-string-from-path b) 'none))
                (benchmark-configuration (== main/t)
                                         (list-no-order (== a/t) (== b) (== adapter))
                                         (== base)))))
