#lang at-exp racket/base

(require racket/contract/base
         racket/math
         racket/set)

(provide (struct-out blame-trail)
         (struct-out mutant-process)
         (struct-out dead-mutant-process)
         (struct-out bench-info)
         (struct-out factory)
         copy-factory
         test-mutant-flag

         sample-size

         mutant-results?
         process-ctl?
         blame-labels?
         mutant/c
         result/c
         blame-trail-id?
         blame-trail/c
         mutant-process/c
         dead-mutant-process/c
         bench-info/c
         factory/c
         mutant-will/c)

(require "../util/path-utils.rkt"
         "../util/mutant-util.rkt"
         "../runner/mutation-runner.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../process-q/interface.rkt"
         syntax/parse/define)

;; see last result of `process`
(define process-ctl? procedure?)

(struct blame-trail (id parts) #:transparent)

;; will?           := (factory? dead-mutant-process? -> factory?)
;; result/c        := run-status/c (see `mutation-runner.rkt`)
;; ctc-level?      := symbol?
;; config?         := (hash path-string?
;;                         (hash (or symbol? path-string?) ctc-level?))
;; blame-trail-id? := (or natural? 'no-blame)

;; mutant:         mutant?
;; config:         config?
;; file:           path-string?
;; id:             natural?
;; blame-trail-id: blame-trail-id?
;; blame-trail:    blame-trail/c
;; revival-count:  natural?
;; increased-limits?: boolean?
(struct mutant-process (mutant
                        config
                        file
                        id
                        blame-trail
                        revival-count
                        increased-limits?)
  #:transparent)

;; result: result/c
(struct dead-mutant-process (mutant
                             config
                             result
                             id
                             ;; INVARIANT: `blame-trail` does NOT contain this
                             ;; dead process.
                             blame-trail
                             increased-limits?)
  #:transparent)

(define mutant-results? (hash/c mutant?
                                path-to-existant-file?))

;; benchmark:  benchmark/c
;; max-config: config?
(struct bench-info (benchmark max-config) #:transparent)

;; bench: bench-info?
;; results: mutant-results?
;;     The map from mutant to blame trail data file
;; mutant-samples: mutant? |-> (set config?)
;;     The set of precision config samples checked for each mutant.
;; total-mutants-spawned: natural?
;;     Count of the total number of mutants spawned by this factory.
;;     This is primarily useful to making every new mutant file unique.
(struct factory (bench
                 results
                 mutant-samples
                 total-mutants-spawned)
  #:transparent)


(define-simple-macro (copy-factory
                      a-factory:expr field-val-pair:expr ...)
  (struct-copy factory a-factory field-val-pair ...))

(define test-mutant-flag 'test)

(define blame-labels? (non-empty-listof module-name?))
(define mutant/c
  (struct/dc mutant
             [benchmark #f]
             [module module-name?]
             [index natural?]))
(define result/c run-status/c)
(define blame-trail-id? (or/c natural?
                              test-mutant-flag))
(define blame-trail/c
  (struct/dc blame-trail
             [id     blame-trail-id?]
             [parts  (listof (recursive-contract dead-mutant-process/c #:chaperone))]))
(define mutant-process/c
  (struct/dc mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [file               path-string?]
             [id                 natural?]
             [blame-trail        blame-trail/c]
             [revival-count      natural?]
             [increased-limits?  boolean?]))
(define dead-mutant-process/c
  (struct/dc dead-mutant-process
             [mutant             mutant/c]
             [config             config/c]
             [result             result/c]
             [id                 natural?]
             [blame-trail        blame-trail/c]
             [increased-limits?  boolean?]))
(define bench-info/c
  (struct/dc bench-info
             [benchmark   benchmark/c]
             [max-config  config/c]))
(define factory/c
  (struct/dc factory
             [bench                   bench-info/c]
             [results                 mutant-results?]
             [mutant-samples          (hash/c mutant/c (set/c config/c))]
             [total-mutants-spawned   natural?]))

(define mutant-will/c
  ((process-Q/c factory/c) dead-mutant-process/c . -> . (process-Q/c factory/c)))

(define sample-size (make-parameter 96))

