#lang at-exp racket/base

;; Null hypothesis blame strategy: just like null stack inspection.
;; Just pick a random next component to type, regardless of the error info.

(require "../../util/optional-contracts.rkt"
         "../../runner/error-extractors/null.rkt")
(provide (contract-out
          [rename make-extract-runtime-error-location
                  make-extract-blamed
                  (blamed-location-extractor/c-for exn:fail?)]))

