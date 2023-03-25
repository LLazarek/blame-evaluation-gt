#lang at-exp racket

(provide (all-defined-out))

(require racket/runtime-path)

(define-runtime-path dbs:icfp
  "../../../experiment-data/dbs/code-mutations-icfp")
(define-runtime-path dbs:natural-biased
  "../../../experiment-data/dbs/code-mutations-natural-biased")
(define-runtime-path dbs:erasure-biased-2
  "../../../experiment-data/dbs/code-mutations-erasure-biased-2")
(define-runtime-path dbs:type-api-mutations
  "../../../experiment-data/dbs/type-api-mutations")

(define-runtime-path data:natural-biased
  "../../../experiment-data/results/code-mutations-natural-biased")
(define-runtime-path data:erasure-biased-2
  "../../../experiment-data/results/code-mutations-erasure-biased-2")
(define-runtime-path data:type-api-mistakes
  "../../../experiment-data/results/type-api-mutations")

(define remote-host-db-installation-directory-name "type-api-mutations")

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
