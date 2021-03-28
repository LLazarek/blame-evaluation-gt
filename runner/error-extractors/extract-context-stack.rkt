#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "blamed-location-extractor.rkt")
(provide (contract-out
          [make-extract-context-stack
           (blamed-location-extractor/c-for exn:fail?)]))

(require "../../util/path-utils.rkt")

(define (make-extract-context-stack the-program
                                    program-config
                                    format-mutant-info-for-error)
  (λ (e)
    (define ctx (continuation-mark-set->context
                 (exn-continuation-marks e)))
    (define (mutatable-mod-in-program? name)
      (hash-has-key? program-config name))
    (define stacktrace-locations
      (for*/list ([ctx-item (in-list ctx)]
                  [ctx-mod-path
                   (in-value
                    (match ctx-item
                      [(cons _ (srcloc (? path? path) _ _ _ _))
                       (path->string path)]
                      [(cons _ (srcloc (? string? path-str) _ _ _ _))
                       (string-trim path-str "\"")]
                      [(cons (? symbol?
                                (app symbol->string
                                     (regexp @regexp{^body of "(.+)"$}
                                             (list _ path-str))))
                             _)
                       path-str]
                      [(cons (? symbol?
                                (app symbol->string
                                     (regexp @regexp{^body of '(.+)$}
                                             (list _ mod-name-sym))))
                             _)
                       (~a mod-name-sym ".rkt")]
                      [else #f]))]
                  [ctx-mod-name
                   (in-value (and ctx-mod-path
                                  (file-name-string-from-path ctx-mod-path)))]
                  #:when (and ctx-mod-name
                              (mutatable-mod-in-program? ctx-mod-name)))
        ctx-mod-name))
    stacktrace-locations))

