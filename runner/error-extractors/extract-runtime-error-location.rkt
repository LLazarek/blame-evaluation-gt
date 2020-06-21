#lang at-exp racket

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-runtime-error-location
           (blamed-location-extractor/c-for exn:fail?)]))

(require "../../util/path-utils.rkt"
         "../program.rkt"
         "blamed-location-extractor.rkt")

(define (make-extract-runtime-error-location the-program
                                             program-config
                                             format-mutant-info-for-error)
  (λ (e)
    (define ctx (continuation-mark-set->context
                 (exn-continuation-marks e)))
    (define mod-with-error
      (for*/first ([ctx-item (in-list ctx)]
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
                   #:when ctx-mod-path
                   [mod (in-list (list* (program-main the-program)
                                        (program-others the-program)))]
                   #:when (equal? (file-name-string-from-path (mod-path mod))
                                  (file-name-string-from-path ctx-mod-path)))
        (file-name-string-from-path ctx-mod-path)))
    (define mod-with-error-name
      (match mod-with-error
        [(? string? name) name]
        [#f
         (eprintf @~a{
                      Couldn't find a mod name in ctx from runtime error, @;
                      possibly because the error happened while @;
                      instantiating a module.
                      Program:
                      @(format-mutant-info-for-error)

                      Ctx:
                      @pretty-format[ctx]

                      The runtime error message is:
                      @(exn-message e)

                      Assuming that errortrace failed us and moving on.
                      })
         #f]))
    (list mod-with-error-name)))
