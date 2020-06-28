#lang at-exp racket/base

;; Transient-newest blame following strategy:
;; Get blame from Transient checks, which blame a sequence of components.
;; Follow blame to the oldest component in the sequence.

(require racket/format
         racket/list
         typed-racket/utils/transient-contract-struct
         "../../util/optional-contracts.rkt"
         "../../runner/error-extractors/blamed-location-extractor.rkt"
         "extract-blamed-mod-name.rkt")

(provide (contract-out
          [make-extract-blamed
           (blamed-location-extractor/c-for exn:fail:contract:blame:transient?)]))

(define (make-extract-blamed the-program
                             program-config
                             format-mutant-info-for-error)
  (λ (e)
    (define blamed-list (transient-get-blame e))
    (define blamed
      (cond [(empty? blamed-list) empty]
            [else
             (define last-boundary (last blamed-list))
             (list (boundary-pos last-boundary)
                   (boundary-neg last-boundary))]))
    (define (error-info)
      @~a{
          Mutant info:
          @(format-mutant-info-for-error)

          The blame error message is:
          @(exn-message e)
          })
    (map (extract-blamed-mod-name error-info)
         blamed)))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name make-extract-blamed
    (ignore (define blame-exn
              (exn:fail:contract:blame:transient
               "hello"
               (current-continuation-marks)
               (list (boundary "/tmp/foo/untyped/a-pos.rkt"
                               "/tmp/foo/untyped/a-neg.rkt"
                               'Any)
                     (boundary "/tmp/foo/untyped/b-pos.rkt"
                               "/tmp/foo/untyped/b-neg.rkt"
                               'Any)
                     (boundary "/tmp/foo/untyped/c-pos.rkt"
                               "/tmp/foo/untyped/c-neg.rkt"
                               'Any)))))
    (test-equal? ((make-extract-blamed #f #f (const ""))
                  blame-exn)
                 '("c-pos.rkt" "c-neg.rkt"))

    (ignore (define empty-blame-exn
              (exn:fail:contract:blame:transient
               "hello"
               (current-continuation-marks)
               empty)))
    (test-equal? ((make-extract-blamed #f #f (const ""))
                  empty-blame-exn)
                 empty)))
