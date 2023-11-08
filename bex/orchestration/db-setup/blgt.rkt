#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script
 #:mutation-analysis-config "../../configurables/blgt-configs/mutation-type-error-analysis.rkt"
 #:mutation-analysis-error-type 'type-error
 #:analyze-type-mutation-categories? #f
 #:experiment-config-with-which-analyze-mutants-dynamic-errors "../../configurables/blgt-configs/dynamic-error-analysis-erasure.rkt"
 #:dynamic-error-filtering-lattice-config-id 'bot
 #:dynamic-error-interestingness-filter? #t
 #:search-for-interesting-scenarios? #f
 #:mutants-to-sample-per-benchmark 80
 #:pre-compute-config "../../configurables/blgt-configs/erasure-pre-compute-benchmark-results.rkt")

