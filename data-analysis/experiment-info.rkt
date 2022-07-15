#lang racket/base

(provide modes
         benchmarks
         scenario-samples-per-mutant
         configured:active-mutator-names
         benchmark-name->benchmark)

(require "../configurables/configurables.rkt"
         "../configurations/configure-benchmark.rkt"
         racket/runtime-path)

(define modes
  '("null" "TR" "TR-stack-first" "transient-newest" "transient-oldest" #;"transient-all" "transient-stack-first" "erasure-stack-first")
  #;'("TR" "TR-stack-first" "transient-newest" "transient-stack-first" "erasure-stack-first"))
(define benchmarks
  '("acquire"
    "gregor"
    "kcfa"
    "quadT"
    "quadU"
    "snake"
    "suffixtree"
    "synth"
    "take5"
    "tetris")
  #;'("acquire"
    "kcfa"
    "take5"))

(define-runtime-path benchmarks-dir "../../gtp-benchmarks/benchmarks")
(define (benchmark-name->benchmark name)
  (read-benchmark (build-path benchmarks-dir name)))

(define scenario-samples-per-mutant 100)

