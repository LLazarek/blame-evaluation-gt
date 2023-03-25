#lang at-exp racket

(provide blame-follower/c
         selector/c)

(require "../../runner/mutation-runner-data.rkt"
         "../../configurations/config.rkt"
         "../../util/path-utils.rkt")

(define blame-follower/c
  (run-status?
   config/c
   . -> .
   (listof module-name?)))

(define selector/c
  (config/c
   (or/c (listof module-name?) #f) ;; blamed
   (listof module-name?)           ;; errortrace stack
   (listof module-name?)           ;; context stack
   . -> .
   (listof module-name?)))
