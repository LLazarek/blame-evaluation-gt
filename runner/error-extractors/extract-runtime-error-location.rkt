#lang at-exp racket

(require "../../util/optional-contracts.rkt")
(provide (contract-out
          [make-extract-runtime-error-location
           (blamed-location-extractor/c-for exn:fail?)]
          [pick-locations
           (parameter/c ((listof string?) . -> . (listof string?)))]))

(require errortrace/errortrace-key
         "../../util/path-utils.rkt"
         "../../util/program.rkt"
         "blamed-location-extractor.rkt")

(define pick-locations (make-parameter #f))

(define (make-extract-runtime-error-location the-program
                                             program-config
                                             format-mutant-info-for-error)
  (λ (e)
    (define ctx (continuation-mark-set->context
                 (exn-continuation-marks e)))
    (define errortrace-locations
      (map errortrace-mark->location
           (continuation-mark-set->list (exn-continuation-marks e)
                                        errortrace-key)))
    (define (mutatable-mod-in-program? name)
      (hash-has-key? program-config name))
    (define errortrace-locations-in-program
      (filter mutatable-mod-in-program? errortrace-locations))
    (define mods-with-error
      (match errortrace-locations-in-program
        [(? cons?)
         ((pick-locations) errortrace-locations-in-program)]
        [else
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
         ((pick-locations) stacktrace-locations)]))
    (match mods-with-error
      [(? list?) mods-with-error]
      [#f
       (displayln @~a{
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
                      }
                  (current-error-port))
       ;; Since error extractors return lists, we can return an empty list here.
       ;; This ends up being consistent in a way with how non-`module-name?`
       ;; elements in a list (from blaming library code) get filtered out.
       empty])))

(define (errortrace-mark->location mark)
  (file-name-string-from-path (second mark)))
