#lang at-exp racket

(require "../../experiment/mutant-factory-data.rkt"
         (prefix-in db: "../../db/db.rkt")
         "../../db/util.rkt"
         "../../util/optional-contracts.rkt"
         "../../configurations/configure-benchmark.rkt"
         "../../configurations/config.rkt"
         "../../util/experiment-exns.rkt"
         racket/runtime-path
         "make-bt-root-sampler.rkt")

(provide (contract-out [make-bt-root-sampler make-bt-root-sampler/c]
                       [pre-selected-bt-root-db (parameter/c (db-path-relative-to? configurables))]
                       [root-missing-blame-response root-missing-blame-response/c]))

;; A root that terminates in erasure (for pre-selection) may timeout in TR or Transient!
;; It may also fail to blame anything in the program.
;; In such cases, the BT fails immeditely.
(define root-missing-blame-response
  (const 'bt-failed))

(define-runtime-path configurables "..")
(define pre-selected-bt-root-db (make-parameter #f))

(define (make-bt-root-sampler bench-info mutant)
  (define resolved-db-path (build-path configurables
                                       (pre-selected-bt-root-db)))
  (define db (db:get resolved-db-path))
  (define the-benchmark (bench-info-benchmark bench-info))
  (define pre-selected-roots-by-mutant
    (db:read db
             (benchmark->name the-benchmark)))
  (define serialized-pre-selected-roots
    (hash-ref pre-selected-roots-by-mutant mutant))
  (define pre-selected-roots
    (for/list ([serialized-config (in-list serialized-pre-selected-roots)])
      (deserialize-config serialized-config
                          #:benchmark the-benchmark)))
  (λ (n)
    (unless (= n (length pre-selected-roots))
      (raise-experiment-user-error
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
