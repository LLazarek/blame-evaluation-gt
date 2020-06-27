#lang at-exp racket

(require racket/format
         racket/list
         racket/match
         racket/runtime-path
         (prefix-in db: "../../db/db.rkt")
         "../../util/optional-contracts.rkt"
         "../../util/path-utils.rkt"
         "../../runner/mutation-runner.rkt"
         "benchmark-runner.rkt")

(provide (contract-out [make-benchmark-runner make-benchmark-runner/c])
         pre-computed-results-db)

(define-runtime-path default-pre-computed-results-db
  "pre-computed-results/default.rktdb")
(define pre-computed-results-db
  (make-parameter default-pre-computed-results-db))

(define (make-benchmark-runner the-program
                               module-to-mutate
                               mutation-index)
  (λ (mod-path)
    (define path (second mod-path))
    (define bench (first (benchmark-mod-relative-path-parts mod-path)))
    (define db (db:get (pre-computed-results-db)))
    (match (db:read db
                    bench
                    #f)
      [(hash-table [(list (== module-to-mutate)
                          (== mutation-index))
                    result]
                   _ ...)
       (raise result)]
      [else
       (raise-user-error
        'benchmark-runner:load-pre-computed-result
        @~a{
            No pre-computed result found in db @(pre-computed-results-db) @;
            for @bench mutant @module-to-mutate @"@" @mutation-index
            })])))

