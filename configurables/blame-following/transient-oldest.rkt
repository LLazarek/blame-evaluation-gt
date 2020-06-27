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
    (when (empty? blamed-list)
      (raise-user-error 'blame-following:transient-oldest
                        @~a{
                            Transient blame error has empty blamed list:
                            @~e[e]
                            }))
    (define blamed (boundary-pos (last blamed-list)))
    (define (error-info)
      @~a{
          Mutant info:
          @(format-mutant-info-for-error)

          The blame error message is:
          @(exn-message e)
          })
    (define blamed-mod-name
      ((extract-blamed-mod-name error-info) blamed))
    (list blamed-mod-name)))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name make-extract-blamed
    (ignore (define blame-exn
              (exn:fail:contract:blame:transient
               "hello"
               (current-continuation-marks)
               (list (boundary "/tmp/foo/untyped/a.rkt" "/" 'Any)
                     (boundary "/tmp/foo/untyped/b.rkt" "/" 'Any)
                     (boundary "/tmp/foo/untyped/c.rkt" "/" 'Any)))))
    (test-equal? ((make-extract-blamed #f #f (const ""))
                  blame-exn)
                 '("c.rkt"))))
