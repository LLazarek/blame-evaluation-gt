#lang at-exp racket

(provide configure!
         install-configuration!
         current-configuration-path
         call-with-configuration)

(require "configurable-implementation.rkt")

(define-configurable mutation
  #:provides [mutate-benchmark active-mutator-names]

  (define-implementation type-mistakes-in-code
    #:module "mutation/mutate-benchmark.rkt")

  (define-implementation type-annotation-mistakes
    #:module "mutation/mutate-types.rkt"))

(define-configurable mutant-sampling
  #:provides [select-mutants all-mutants-should-have-trails?]

  (define-implementation none
    #:module "mutant-sampling/no-sampling.rkt")

  (define-implementation use-pre-selected-samples
    #:module "mutant-sampling/use-pre-selected-samples.rkt"
    #:parameters [pre-selected-mutant-samples-db]))

(define-configurable blame-following
  #:provides [make-extract-blamed]

  (define-implementation null
    #:module "blame-following/null.rkt")

  (define-implementation natural-blame
    #:module "blame-following/natural-blame.rkt")

  (define-implementation transient-oldest
    #:module "blame-following/transient-oldest.rkt")
  (define-implementation transient-all
    #:module "blame-following/transient-all.rkt")

  (define-implementation stack
    #:module "blame-following/stack.rkt"))

(define-configurable stack-location-selection
  #:provides [make-extract-runtime-error-location]

  (define-implementation top
    #:module "../runner/error-extractors/extract-runtime-error-location.rkt"
    #:fixed-parameters ([pick-locations (λ (all) (take all 1))]))
  (define-implementation all
    #:module "../runner/error-extractors/extract-runtime-error-location.rkt"
    #:fixed-parameters ([pick-locations values])))

(define-configurable module-instrumentation
  #:provides [instrument-module]

  (define-implementation none
    #:module "module-instrumentation/none.rkt")

  (define-implementation transient-types
    #:module "module-instrumentation/type-with-transient.rkt"
    #:parameters [transient-special-cases-db]))

(define-configurable benchmark-runner
  #:provides [make-benchmark-runner]

  (define-implementation run-it
    #:module "benchmark-runner/run-it.rkt")

  (define-implementation none
    #:module "benchmark-runner/nothing.rkt")

  (define-implementation load-pre-computed-result
    #:module "benchmark-runner/load-pre-computed-result.rkt"
    #:parameters [pre-computed-results-db]))

(define-configurable trail-completion
  #:provides [blame-trail-ended?]

  (define-implementation any-type-error
    #:module "trail-completion/any-type-error.rkt")

  (define-implementation mutated-type-error/blamed-at-max
    #:module "trail-completion/mutated-type-error-or-blamed-at-max.rkt"))

(define-configurable bt-root-sampling
  #:provides [make-bt-root-sampler]

  (define-implementation random-with-replacement
    #:module "bt-root-sampling/random-with-replacement.rkt")

  (define-implementation pre-selected
    #:module "bt-root-sampling/pre-selected.rkt"
    #:parameters [pre-selected-bt-root-db]))
