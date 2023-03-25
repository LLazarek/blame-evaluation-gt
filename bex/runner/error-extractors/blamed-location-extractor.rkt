#lang at-exp racket/base

(provide blamed-location-extractor/c-for
         blamed-modules/c)

(require racket/contract/base
         racket/contract/combinator
         "../../util/path-utils.rkt"
         "../../util/program.rkt"
         "../../configurations/config.rkt")

(define blamed-modules/c (listof (or/c module-name?
                                       library-path?)))
(define (blamed-location-extractor/c-for exn-predicate)
  (program/c
   config/c
   (-> string?)
   . -> .
   (exn-predicate . -> . blamed-modules/c)))
