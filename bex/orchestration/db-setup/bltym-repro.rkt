#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script
 #:mutation-analysis-config "../../configurables/bltym-configs/mutation-type-error-analysis.rkt"
 #:mutation-analysis-error-type 'type-error
 #:analyze-type-mutation-categories? #t
 #:experiment-config-with-which-analyze-mutants-dynamic-errors "../../configurables/bltym-configs/dynamic-error-analysis-erasure.rkt"
 #:dynamic-error-filtering-lattice-config-id 'bot
 #:dynamic-error-interestingness-filter? #f
 #:search-for-interesting-scenarios? #t
 #:mutants-to-sample-per-benchmark 1000
 #:pre-compute-config "../../configurables/bltym-configs/erasure-pre-compute-benchmark-results.rkt")

