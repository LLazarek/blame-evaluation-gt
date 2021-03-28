#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "blamed-location-extractor.rkt")
(provide (contract-out
          [make-extract-blamed-locations
           (blamed-location-extractor/c-for exn:fail:contract:blame?)]))

(require typed-racket/utils/transient-contract-struct
         "../../util/path-utils.rkt")

(define (make-extract-blamed-locations the-program
                                       program-config
                                       format-mutant-info-for-error)
  (λ (e)
    (cond [(exn:fail:contract:blame:transient? e)
           (extract-transient-blamed-location e format-mutant-info-for-error)]
          [(exn:fail:contract:blame? e)
           (extract-contract-blamed-location e format-mutant-info-for-error)]
          [else (raise-user-error
                 'make-extract-blamed-locations
                 @~a{
                     Got an exn that is not `exn:fail:contract:blame?`:
                     @~s[e]

                     Mutant info:
                     @(format-mutant-info-for-error)
                     })])))

(define (extract-contract-blamed-location e format-mutant-info-for-error)
  (define blame-obj (exn:fail:contract:blame-object e))
  (define blamed
    (match (blame-positive blame-obj)
      [`(function ,id) id]
      [`(definition ,id) id]
      [(or 'cast 'typed-world)
       (srcloc-source (blame-source blame-obj))]
      [other other]))
  (define (error-info)
    @~a{
        Mutant info:
        @(format-mutant-info-for-error)

        The blame error message is:
        @(exn-message e)
        })
  (define blamed-mod-name
    ((extract-blamed-mod-name error-info)
     blamed))
  (list blamed-mod-name))

(define (extract-transient-blamed-location e format-mutant-info-for-error)
  (define blamed-list (transient-get-blame e))
  (define blamed
    (cond [(empty? blamed-list) empty]
          [else
           (flatten
            (map (λ (boundary)
                   (list (boundary-pos boundary)
                         (boundary-neg boundary)))
                 blamed-list))]))
  (define (error-info)
    @~a{
        Mutant info:
        @(format-mutant-info-for-error)

        The blame error message is:
        @(exn-message e)
        })
  (map (extract-blamed-mod-name error-info)
       blamed))

(define ((extract-blamed-mod-name [extra-error-info (const "")])
         blamed)
  (match blamed
    ;; NOTE: This depends on a modification to Typed Racket;
    ;; Specifically `require/contract` must be modified to change
    ;; the positive party, by extending the list with
    ;; ```from #,(syntax->datum #'lib)```
    ;; util/setup.rkt makes this modification automatically.
    [`(interface for ,_ from ,mod-name)
     mod-name]
    [(? path-string?)
     (file-name-string-from-path blamed)]
    [else
     (error 'extract-blamed-mod-name
            @~a{
                Unexpected blamed party shape: @~v[blamed]
                @(extra-error-info)
                })]))

(module+ test
  (require ruinit
           racket)
  (test-begin
    #:name extract-transient-blamed-location
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
    (test-equal? ((make-extract-blamed-locations #f #f (const ""))
                  blame-exn)
                 '("a-pos.rkt"
                   "a-neg.rkt"
                   "b-pos.rkt"
                   "b-neg.rkt"
                   "c-pos.rkt"
                   "c-neg.rkt"))

    (ignore (define empty-blame-exn
              (exn:fail:contract:blame:transient
               "hello"
               (current-continuation-marks)
               empty)))
    (test-equal? ((make-extract-blamed-locations #f #f (const ""))
                  empty-blame-exn)
                 empty)))
