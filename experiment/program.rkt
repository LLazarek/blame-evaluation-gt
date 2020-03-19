#lang at-exp racket

(require "../runner/instrumented-runner.rkt"
         "../util/read-module.rkt")

(provide (contract-out
          [make-mod
           (path-string? . -> . mod/c)]
          [make-program
           (path-string? (listof path-string?) . -> . program/c)]))

(define (make-mod path-str)
  (define path (simple-form-path path-str))
  (mod path
       (read-module path)))

(define (make-program main others)
  (program (make-mod main)
           (map make-mod others)))
