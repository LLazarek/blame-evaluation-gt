#lang at-exp racket

;; Natural blame following strategy:
;; Get blame from normal Racket contracts, which only ever blame a single
;; component. Follow blame to that component.

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-blamed
           (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

(require "../../util/path-utils.rkt"
         "../../runner/error-extractors/blamed-location-extractor.rkt")

(define (make-extract-blamed the-program
                             format-mutant-info-for-error)
  (λ (e)
    (define blame-obj (exn:fail:contract:blame-object e))
    (define blamed
      (match (blame-positive blame-obj)
        [`(function ,id) id]
        [`(definition ,id) id]
        [(or 'cast 'typed-world)
         (srcloc-source (blame-source blame-obj))]
        [other other]))
    (define blamed-mod-name
      (match blamed
        ;; NOTE: This depends on a modification to Typed Racket;
        ;; Specifically `require/contract` must be modified to change
        ;; the positive party, by extending the list with
        ;; ```from #,(syntax->datum #'lib)```
        [`(interface for ,_ from ,mod-name)
         mod-name]
        [(? path-string?)
         (file-name-string-from-path blamed)]
        [else
         (error 'extract-blamed
                @~a{
                    Unexpected blamed party shape: @~v[blamed]
                    Mutant info:
                    @(format-mutant-info-for-error)

                    The blame error message is:
                    @(exn-message e)
                    })]))
    (list blamed-mod-name)))
