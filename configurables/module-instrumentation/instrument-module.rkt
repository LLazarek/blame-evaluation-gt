#lang racket

(provide module-instrumenter/c)

(require "../../runner/program.rkt")

(define module-instrumenter/c
  (mod/c . -> . mod/c))
