#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script
 #:mutation-analysis-config "../../configurables/blutil-configs/mutation-type-error-analysis.rkt"
 #:mutation-analysis-error-type 'any-error
 #:analyze-type-mutation-categories? #f
 #:experiment-config-with-which-analyze-mutants-dynamic-errors "../../configurables/blutil-configs/dynamic-error-analysis.rkt"
 #:dynamic-error-filtering-lattice-config-id 'bot
 #:dynamic-error-interestingness-filter? #t
 #:search-for-interesting-scenarios? #f
 #:mutants-to-sample-per-benchmark 80
 #:no-erasure-mode)

