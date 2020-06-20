#lang at-exp racket

;; Natural stack-based blame following strategy:
;; Ignore blame and follow the top component on the stack trace.

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-blamed
           (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

(require "../../util/path-utils.rkt"
         "../../runner/error-extractors/blamed-location-extractor.rkt"
         "../../runner/error-extractors/extract-runtime-error-location.rkt")

(define (make-extract-blamed the-program
                             format-mutant-info-for-error)
  (make-extract-runtime-error-location the-program
                                       format-mutant-info-for-error))
