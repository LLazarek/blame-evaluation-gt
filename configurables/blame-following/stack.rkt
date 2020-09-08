#lang at-exp racket

;; Stack-based blame following strategy:
;; Ignore blame and follow some component on the stack trace (configured via `pick-locations`).

(require "../../util/optional-contracts.rkt"
         "../../runner/error-extractors/extract-runtime-error-location.rkt")
(provide (contract-out
          [rename make-extract-runtime-error-location
                  make-extract-blamed
                  (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

