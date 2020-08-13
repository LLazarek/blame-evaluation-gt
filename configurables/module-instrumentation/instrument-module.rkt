#lang racket

(provide module-instrumenter/c)

(require "../../util/program.rkt")

(define module-instrumenter/c
  (mod/c . -> . mod/c))
