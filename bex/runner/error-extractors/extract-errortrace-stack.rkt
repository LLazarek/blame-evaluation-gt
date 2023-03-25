#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "blamed-location-extractor.rkt")
(provide (contract-out
          [make-extract-errortrace-stack
           (blamed-location-extractor/c-for exn:fail?)]))

(require errortrace/errortrace-key
         "../../util/path-utils.rkt")

(define (make-extract-errortrace-stack the-program
                                          program-config
                                          format-mutant-info-for-error)
  (λ (e)
    (define errortrace-locations
      (map errortrace-mark->location
           (continuation-mark-set->list (exn-continuation-marks e)
                                        errortrace-key)))
    (define (mutatable-mod-in-program? name)
      (hash-has-key? program-config name))
    (define errortrace-locations-in-program
      (filter mutatable-mod-in-program? errortrace-locations))
    errortrace-locations-in-program))

(define (errortrace-mark->location mark)
  (file-name-string-from-path (second mark)))
