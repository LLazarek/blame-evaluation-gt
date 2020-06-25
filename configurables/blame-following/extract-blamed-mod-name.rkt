#lang at-exp racket/base

(provide extract-blamed-mod-name)

(require racket/format
         racket/function
         racket/match
         "../../util/path-utils.rkt")

(define ((extract-blamed-mod-name [extra-error-info (const "")])
         blamed)
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
     (error 'blame-following:extract-blamed-mod-name
            @~a{
                Unexpected blamed party shape: @~v[blamed]
                @(extra-error-info)
                })]))
