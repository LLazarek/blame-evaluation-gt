#lang at-exp racket

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-type-error-source
           (blamed-location-extractor/c-for exn:fail:syntax?)]))

(require "../../util/path-utils.rkt"
         "../../util/program.rkt"
         "blamed-location-extractor.rkt"
         syntax/location)

(define (make-extract-type-error-source the-program
                                        program-config
                                        format-mutant-info-for-error)
  (λ (e)
    (define failing-stxs (exn:fail:syntax-exprs e))
    (define module-name
      (match failing-stxs
        [(list* (app syntax-source-file-name #f)
                ...
                (app syntax-source-file-name (? path? file-name-path))
                _)
         (path->string file-name-path)]
        [else
         (error 'extract-type-error-source
                @~a{
                    Couldn't find mod name in type error stxs.
                    Program:
                    @(format-mutant-info-for-error)

                    Stxs:
                    @~v[failing-stxs]

                    The type error message is:
                    @(exn-message e)
                    })]))
    (list module-name)))
