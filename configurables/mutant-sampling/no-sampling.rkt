#lang at-exp racket/base

(provide select-mutants)

(require "../../util/mutant-util.rkt")

(define (select-mutants module-to-mutate-name bench)
  (in-mutation-indices module-to-mutate-name
                       bench))
