#lang at-exp racket

(provide (all-defined-out)
         (struct-out orchestration-config))

(require racket/runtime-path)

(struct orchestration-config (dbs-dir dbs-dir-name download-dir setup-config))

(define-runtime-path dbs:icfp
  "../../../experiment-data/dbs/code-mutations-icfp")
(define-runtime-path dbs:natural-biased
  "../../../experiment-data/dbs/code-mutations-natural-biased")
(define-runtime-path dbs:erasure-biased-2
  "../../../experiment-data/dbs/code-mutations-erasure-biased-2")
(define-runtime-path dbs:blgt-erasure-biased-thesis
  "../../../experiment-data/dbs/code-mutations-erasure-biased-thesis")
(define-runtime-path dbs:type-api-mutations
  "../../../experiment-data/dbs/type-api-mutations")
(define-runtime-path dbs:blutil
  "../../../experiment-data/dbs/blutil")

(define-runtime-path data:natural-biased
  "../../../experiment-data/results/code-mutations-natural-biased")
(define-runtime-path data:erasure-biased-2
  "../../../experiment-data/results/code-mutations-erasure-biased-2")
(define-runtime-path data:type-api-mistakes
  "../../../experiment-data/results/type-api-mutations")
(define-runtime-path data:blgt-erasure-biased-thesis
  "../../../experiment-data/results/code-mutations-erasure-biased-thesis")
(define-runtime-path data:blutil
  "../../../experiment-data/results/blutil")

(define setup:bltym "bltym-setup-config.rkt")
(define setup:blgt "blgt-setup-config.rkt")
(define setup:blutil "blutil-setup-config.rkt")

;; this needs to match up with whatever the experiment configs look for!
(define current-remote-host-db-installation-directory-name (make-parameter #f))

(define type-mistakes
  (orchestration-config dbs:type-api-mutations
                        "type-api-mutations"
                        data:type-api-mistakes
                        setup:bltym))
(define code-mistakes
  (orchestration-config dbs:blgt-erasure-biased-thesis
                        "code-mutations"
                        data:blgt-erasure-biased-thesis
                        setup:blgt))

(define blutil
  (orchestration-config dbs:blutil
                        "blutil"
                        data:blutil
                        setup:blutil))

(define-runtime-path benchmarks-dir "../../../gtp-benchmarks/benchmarks")

(define scenario-samples-per-mutant 100)

(define experiment-benchmarks
  '("acquire"
    "gregor"
    "kcfa"
    "quadT"
    "quadU"
    ;; "sieve"
    "snake"
    "suffixtree"
    "synth"
    "take5"
    "tetris"))

(define experiment-modes
  '("TR-null"
    "TR"
    "TR-stack-first"
    "transient-newest"
    "transient-oldest"
    #;"transient-all"
    "transient-stack-first"
    "erasure-stack-first"))
