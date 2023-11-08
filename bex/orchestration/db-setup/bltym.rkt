#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script
 #:mutation-analysis-config "../../configurables/configs/mutation-type-error-analysis.rkt"
 #:mutation-analysis-error-type 'type-error
 #:analyze-type-mutation-categories? #t
 #:experiment-config-with-which-analyze-mutants-dynamic-errors "../../configurables/configs/dynamic-error-analysis-natural.rkt"
 #:dynamic-error-filtering-lattice-config-id 'bot
 #:dynamic-error-interestingness-filter? #f
 #:search-for-interesting-scenarios? #t
 #:mutants-to-sample-per-benchmark 'all
 #:pre-compute-config "../../configurables/configs/erasure-pre-compute-benchmark-results.rkt")

