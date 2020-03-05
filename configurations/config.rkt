#lang at-exp racket

(provide config/c)

(define config/c (hash/c string? any/c))
