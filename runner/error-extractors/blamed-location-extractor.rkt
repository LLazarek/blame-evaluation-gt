#lang at-exp racket/base

(provide blamed-location-extractor/c
         blamed-modules/c)

(require racket/contract/base
         racket/contract/combinator
         "../../util/path-utils.rkt"
         "../program.rkt")

(define blamed-modules/c (listof (or/c module-name?
                                       library-path?)))
(define blamed-location-extractor/c
  (program/c
   (-> string?)
   . -> .
   (exn:fail:contract:blame? . -> . blamed-modules/c)))
