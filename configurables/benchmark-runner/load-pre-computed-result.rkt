#lang at-exp racket

(require racket/format
         racket/list
         racket/match
         racket/runtime-path
         (prefix-in db: "../../db/db.rkt")
         "../../util/optional-contracts.rkt"
         "../../util/path-utils.rkt"
         "../../util/mutant-util.rkt"
         "../../util/experiment-exns.rkt")

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c]
                       [pre-computed-results-db (db-path-relative-to? configurables)]))

(define-runtime-path configurables "..")
(define default-pre-computed-results-db
  "benchmark-runner/pre-computed-results/default.rktdb")
(define pre-computed-results-db
  (make-parameter default-pre-computed-results-db))

(define (make-benchmark-runner the-program
                               module-to-mutate
                               mutation-index)
  (λ (mod-path)
    (define path (second mod-path))
    (define bench (first (benchmark-mod-relative-path-parts path)))
    (define resolved-db-path (build-path configurables
                                         (pre-computed-results-db)))
    (define db (db:get resolved-db-path))
    (match (db:read db
                    bench
                    #f)
      [(hash-table [(mutant _
                            (== module-to-mutate)
                            (== mutation-index))
                    result]
                   _ ...)
       (raise result)]
      [else
       (raise-experiment-user-error
        'benchmark-runner:load-pre-computed-result
        @~a{
            No pre-computed result found in db @(pre-computed-results-db) @;
            for @bench mutant @module-to-mutate @"@" @mutation-index
            })])))

(module+ test
  (require ruinit
           racket/runtime-path)
  (define-test-env {setup! cleanup!}
    #:directories ([test-tmp "./test-tmp"])
    #:files ())
  (test-begin
    #:name make-benchmark-runner
    #:before (setup!)
    #:after (cleanup!)
    (ignore
     (define db-path (build-path test-tmp "db.rktdb"))
     (db:new! db-path)
     (define empty-db (db:get db-path))
     (db:write! empty-db (hash "a-benchmark" (hash (mutant #f "a-module.rkt" 42) 'a-result))))
    (parameterize ([pre-computed-results-db (find-relative-path (simple-form-path "..")
                                                                (simple-form-path db-path))])
      (test-exn (λ (x) (equal? x 'a-result))
                ((make-benchmark-runner #f "a-module.rkt" 42)
                 '(file "foo/bar/a-benchmark/untyped/main.rkt")))
      (test-exn exn:fail:user?
                ((make-benchmark-runner #f "a-module.rkt" 0)
                    '(file "foo/bar/a-benchmark/untyped/main.rkt"))))))

