#lang racket/base

(provide modes
         benchmarks
         configured:active-mutator-names)

(require "../configurables/configurables.rkt")

(define modes
  '("null" "TR" "TR-stack-first" "transient-newest" "transient-oldest" #;"transient-all" "transient-stack-first" "erasure-stack-first"))
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
    "tetris"))

