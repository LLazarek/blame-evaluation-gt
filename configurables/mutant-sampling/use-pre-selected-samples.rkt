#lang at-exp racket

(require "../../util/optional-contracts.rkt")
(provide (contract-out [select-mutants mutant-selector/c]
                       [pre-selected-mutant-samples-db (parameter/c (db-path-relative-to? configurables))]
                       [all-mutants-should-have-trails? boolean?]))

(require "mutant-selector.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../db/util.rkt"
         "../../configurations/configure-benchmark.rkt"
         racket/runtime-path)

(define-runtime-path configurables "..")
(define default-samples-db "mutant-sampling/samples/default.rktdb")
(define pre-selected-mutant-samples-db (make-parameter default-samples-db))

(define (select-mutants module-to-mutate-name the-bench)
  (define samples (samples-for the-bench module-to-mutate-name))
  (in-list samples))

;; benchmark/c module-name? -> summary?
(define (samples-for the-bench module-to-mutate-name)
  (define bench-name (benchmark->name the-bench))
  (define resolved-db-path (build-path configurables
                                       (pre-selected-mutant-samples-db)))
  (define db (db:get resolved-db-path))
  (define samples-by-module (db:read db bench-name))
  (hash-ref samples-by-module
            module-to-mutate-name
            '()))

(define all-mutants-should-have-trails? #t)
