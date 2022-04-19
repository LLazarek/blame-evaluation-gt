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
    #:module "mutation/mutate-types.rkt")

  (define-implementation type-interface-mistakes
    #:module "mutation/mutate-type-interface.rkt"))

(define-configurable mutant-sampling
  #:provides [select-mutants all-mutants-should-have-trails?]

  (define-implementation none
    #:module "mutant-sampling/no-sampling.rkt")

  (define-implementation use-pre-selected-samples
    #:module "mutant-sampling/use-pre-selected-samples.rkt"
    #:parameters [pre-selected-mutant-samples-db]))

(define-configurable mutant-filtering
  #:provides [should-sample-mutant-blame-trails?]

  (define-implementation select-type-erroring-max-config-mutants
    #:module "mutant-filtering/type-error.rkt")
  (define-implementation select-type/runtime/ctc-erroring-max-config-mutants
    #:module "mutant-filtering/type-runtime-ctc-error.rkt"))

(define-configurable module-selection-for-mutation
  #:provides [select-modules-to-mutate]

  (define-implementation all-regular-modules
    #:module "module-selection-for-mutation/all-regular-modules.rkt")

  (define-implementation interface-module-only
    #:module "module-selection-for-mutation/interface-module-only.rkt"))

;; Extra module-level instrumentation beyond mutation
(define-configurable module-instrumentation
  #:provides [instrument-module]

  (define-implementation none
    #:module "module-instrumentation/none.rkt")

  (define-implementation transient-types
    #:module "module-instrumentation/type-with-transient.rkt"
    #:parameters [transient-special-cases-db]))

;; Controls how all instrumentation is applied to a program.
;; Thus also has the opportunity to transform the program as part of "instrumentation".
(define-configurable program-instrumentation
  #:provides [instrument-program]

  (define-implementation just-instrument-modules
    #:module "program-instrumentation/just-instrument-modules.rkt")

  (define-implementation instrument-modules-and-insert-interface-adapter-module
    #:module "program-instrumentation/instrument-modules-and-insert-interface-adapter-module.rkt"))

(define-configurable benchmark-runner
  #:provides [make-benchmark-runner]

  (define-implementation run-it
    #:module "benchmark-runner/run-it.rkt")

  (define-implementation none
    #:module "benchmark-runner/nothing.rkt")

  (define-implementation load-pre-computed-result
    #:module "benchmark-runner/load-pre-computed-result.rkt"
    #:parameters [pre-computed-results-db]))

(define-configurable blame-following
  #:provides [follow-blame]

  (define-implementation null
    #:module "blame-following/null.rkt")

  (define-implementation pick-some
    #:module "blame-following/pick-some.rkt"
    #:parameters [selector:runtime-error-with-blame
                  selector:runtime-error
                  selector:blame]))

(define-configurable trail-completion
  #:provides [blame-trail-ended-normally?]

  (define-implementation any-type-error/blamed-at-max
    #:module "trail-completion/any-type-error-or-blamed-at-max.rkt")

  (define-implementation mutated-type-error/blamed-at-max
    #:module "trail-completion/mutated-type-error-or-blamed-at-max.rkt"))

(define-configurable bt-root-sampling
  #:provides [make-bt-root-sampler root-missing-blame-response]

  (define-implementation random-with-replacement
    #:module "bt-root-sampling/random-with-replacement.rkt")

  (define-implementation pre-selected
    #:module "bt-root-sampling/pre-selected.rkt"
    #:parameters [pre-selected-bt-root-db])

  (define-implementation subset-random-with-replacement
    #:module "bt-root-sampling/subset-random-w-replacement.rkt"
    #:parameters [root-config-filter]))

