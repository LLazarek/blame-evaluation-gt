#lang at-exp racket

(provide strata-proportion-estimate
         estimate-proportion
         variance->margin-of-error)

(require bex/configurables/configurables
         "experiment-info.rkt"
         "read-data.rkt")

(define (stderr->variance stderr)
  (sqr stderr))
(define (sqrt* n)
  (when (negative? n) (displayln @~a{sqrt of negative: @n}))
  (sqrt n))
(define (variance->stderr variance)
  (sqrt* variance))
(define (variance->margin-of-error variance [confidence-z-value 1.96])
  (* (variance->stderr variance) confidence-z-value))

;; proportion-estimate/c :=
;; (hashof 'proportion-estimate (real-in 0 1)
;;         'variance            real?
;;         'sample-size         natural?
;;         'population-size     natural?)

;; (listof any/c) (any/c . -> . boolean?) natural? [(real-in 0 1) real?]
;; ->
;; proportion-estimate/c
;; estimate? := (real-in 0 1)
;; variance? := real?
(define (estimate-proportion sample
                             predicate
                             population-size
                             [confidence 95/100]
                             [confidence-z-value 1.96])
  (define sample-size (length sample))
  (define proportion-estimate (/ (count predicate sample) sample-size))
  ;; variance of a proportion estimate within a sample from
  ;; https://stattrek.com/survey-research/stratified-sampling-analysis.aspx
  (define variance (/ (* sample-size proportion-estimate (- 1 proportion-estimate))
                      (sub1 sample-size)))
  (hash 'proportion-estimate proportion-estimate
        'variance variance
        'sample-size sample-size
        'population-size population-size))

(require "sizes.rkt")
(define max-bt-population-sizes-by-benchmark
  ;; computed in sizes.rkt: possible-interesting-scenario-counts/by-benchmark
  possible-interesting-scenario-counts/by-benchmark)

;; (A . -> . boolean?) ; where A is typically blame-trail?
;; (benchmark? mutator . -> . (hash/c mutant? A))
;; . -> .
;; proportion-estimate/c
(define (strata-proportion-estimate predicate bts-by-mutant-for)
  (define mutators (configured:active-mutator-names))
  (define benchmark-bucket-estimates
    (for/list ([benchmark (in-list benchmarks)])
      (define mutator-bucket-estimates
        (for/list ([mutator (in-list mutators)])
          (define bts-by-mutant (bts-by-mutant-for benchmark mutator))
          (define mutant-bucket-estimates
            (for/list ([{mutant bts} (in-hash bts-by-mutant)])
              (define max-bt-population-size
                (hash-ref max-bt-population-sizes-by-benchmark
                          (mutant-benchmark mutant)))
              (estimate-proportion bts
                                   predicate
                                   (max max-bt-population-size
                                        scenario-samples-per-mutant))))
          (combine-subgroup-estimates mutant-bucket-estimates)))
      (combine-subgroup-estimates mutator-bucket-estimates)))
  (combine-subgroup-estimates benchmark-bucket-estimates))

(define (combine-subgroup-estimates estimates)
  ;; Based entirely on:
  ;; https://stattrek.com/survey-research/stratified-sampling-analysis.aspx
  (cond [(empty? estimates)
         #f]
        [else
         (define non-empty-subgroup-estimates (filter-not false? estimates))
         (define total-group-population-size
           (for/sum ([subgroup-estimate (in-list non-empty-subgroup-estimates)])
             (hash-ref subgroup-estimate
                       'population-size)))
         (define total-group-sample-size
           (for/sum ([subgroup-estimate (in-list non-empty-subgroup-estimates)])
             (hash-ref subgroup-estimate
                       'sample-size)))
         (unless (>= total-group-population-size total-group-sample-size)
           (error 'combine @~a{@total-group-population-size < @total-group-sample-size}))
         (define group-proportion-estimate
           (for/sum ([estimate (in-list non-empty-subgroup-estimates)])
             (* (/ (hash-ref estimate 'sample-size)
                   total-group-sample-size)
                (hash-ref estimate 'proportion-estimate))))
         (define group-estimate-stderr
           (* (/ 1 total-group-population-size)
              (sqrt* (for/sum ([estimate (in-list non-empty-subgroup-estimates)])
                       (* (sqr (hash-ref estimate 'population-size))
                          (- 1 (/ (hash-ref estimate 'sample-size)
                                  (hash-ref estimate 'population-size)))
                          (/ (hash-ref estimate 'variance)
                             (hash-ref estimate 'sample-size)))))))
         (define group-variance (stderr->variance group-estimate-stderr))
         (hash 'sample-size (length non-empty-subgroup-estimates)
               'proportion-estimate group-proportion-estimate
               'variance group-variance
               'population-size total-group-population-size)]))
