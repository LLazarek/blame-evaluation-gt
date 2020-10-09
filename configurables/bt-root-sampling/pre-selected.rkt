#lang at-exp racket/base

(require "../../experiment/mutant-factory-data.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../util/optional-contracts.rkt"
         "../../configurations/configure-benchmark.rkt"
         "make-bt-root-sampler.rkt"
         racket/format
         racket/random
         racket/runtime-path)

(provide (contract-out [make-bt-root-sampler make-bt-root-sampler/c]
                       [pre-selected-bt-root-db (parameter/c path-to-db?)]))

(define-runtime-path configurables "..")
(define pre-selected-bt-root-db (make-parameter #f))

(define (make-bt-root-sampler bench-info mutant)
  (define resolved-db-path (build-path configurables
                                       (pre-selected-bt-root-db)))
  (define db (db:get resolved-db-path))
  (define pre-selected-roots-by-mutant
    (db:read db
             (benchmark->name (bench-info-benchmark bench-info))))
  (define pre-selected-roots
    (hash-ref pre-selected-roots-by-mutant mutant))
  (λ (n)
    (unless (= n (length pre-selected-roots))
      (raise-user-error
       'pre-selected:make-bt-root-sampler
       @~a{
           Got a request for @n samples but there are @(length pre-selected-roots) @;
           in the db @resolved-db-path
           Request was for mutant: @~v[mutant]
           @(if (= n 1)
                "n = 1 so this was probably a resample, meaning a pre-selected root is not valid!"
                "n != 1 so probably there's a configuration mismatch somewhere.")
           }))
    pre-selected-roots))
