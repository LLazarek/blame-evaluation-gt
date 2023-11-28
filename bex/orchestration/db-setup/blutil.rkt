#lang at-exp racket

(require "db-setup.rkt")

(db-setup-script
 #:mutation-analysis-config "../../configurables/blutil-configs/mutation-code-mistake-analysis.rkt"
 #:mutation-analysis-error-type 'any-error
 #:analyze-type-mutation-categories? #f
 #:experiment-config-with-which-analyze-mutants-dynamic-errors "../../configurables/blutil-configs/mutation-code-mistake-analysis.rkt"
 #:dynamic-error-filtering-lattice-config-id 'bot
 #:dynamic-error-interestingness-filter? #f
 #:search-for-interesting-scenarios? #f
 #:mutants-to-sample-per-benchmark 80
 #:no-erasure-mode)

