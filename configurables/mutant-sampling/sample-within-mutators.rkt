#lang at-exp racket/base

(require "../../util/optional-contracts.rkt")
(provide (contract-out [select-mutants mutant-selector/c])
         mutation-analysis-samples-db)

(require "mutant-selector.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../configurations/configure-benchmark.rkt"
         "../../util/mutant-util.rkt"
         racket/runtime-path
         racket/format)

(define-runtime-path default-samples-db "samples/default.rktdb")
(define mutation-analysis-samples-db (make-parameter default-samples-db))

(define (select-mutants module-to-mutate-name the-bench)
  (define samples (samples-for the-bench module-to-mutate-name))
  (in-list samples))

;; benchmark/c module-name? -> summary?
(define (samples-for the-bench module-to-mutate-name)
  (define (empty-if-no-mutants)
    (define max-index (max-mutation-index module-to-mutate-name
                                          the-bench))
    (if (< max-index 0)
        '()
        (error
         'sample-within-mutators
         @~a{
             No samples recorded in db @(mutation-analysis-samples-db) @;
             for module @~e[module-to-mutate-name]
             })))
  (define bench-name (benchmark->name the-bench))
  (define db (db:get (mutation-analysis-samples-db)))
  (define samples-by-module (db:read db bench-name))
  (hash-ref samples-by-module
            module-to-mutate-name
            empty-if-no-mutants))

