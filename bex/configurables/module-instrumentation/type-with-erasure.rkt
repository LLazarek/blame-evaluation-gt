#lang at-exp racket

(require syntax/parse
         syntax/parse/define
         (for-syntax syntax/parse)
         "../../util/program.rkt"
         "../../util/optional-contracts.rkt"
         "instrument-module.rkt")

(provide (contract-out
          [instrument-module module-instrumenter/c]))

(define (instrument-module a-mod)
  (define instrumented-stx
    (erase-require/typed/checks
     (swap-lang->tr/no-check
      (mod-stx a-mod))))
  (struct-copy mod a-mod
               [stx instrumented-stx]))

(define (swap-lang->tr/no-check stx)
  (syntax-parse stx
    [(module name {~and {~or* {~datum typed/racket}
                              {~datum typed/racket/base}}
                        tr}
       . body)
     #:with tr/no-check (datum->syntax this-syntax 'typed/racket/no-check this-syntax)
     (syntax/loc this-syntax
       (module name tr/no-check . body))]
    [other this-syntax]))

(define-syntax-class adapter-req
  #:description "adapter module require path"
  (pattern path:str
           #:do [(define path-str (syntax->datum #'path))]
           #:when (regexp-match? #rx".+-adapt(e|o)(r|d).rkt$" path-str)
           #:with erased (datum->syntax this-syntax
                                        (regexp-replace #rx"-adapt(e|o)(r|d).rkt$"
                                                        path-str
                                                        ".rkt")))
  (pattern "typed-data.rkt"
           #:with erased #'"data.rkt")
  (pattern ({~and e-i {~datum except-in}} r:adapter-req name ...)
           #:with erased #'(e-i r.erased name ...)))

(define-syntax-class tlf
  #:description "top level form"
  (pattern ({~datum require} {~alt "../../../utilities/require-typed-check-provide.rkt"
                                   {~datum require-typed-check}
                                   other-reqs}
                             ...)
           #:with erased (datum->syntax this-syntax
                                        `(require
                                          "../../../utilities/require-typed-check-provide-erased.rkt"
                                          . ,(attribute other-reqs))))
  (pattern ({~datum reprovide} {~alt adapter:adapter-req other-reqs} ...)
           #:with erased (datum->syntax this-syntax
                                        `(reprovide
                                          ,@(append (attribute adapter.erased)
                                                    (attribute other-reqs)))))
  (pattern something-else
           #:with erased this-syntax))

(define (erase-require/typed/checks stx)
  (syntax-parse stx
    [(module name lang
       (#%module-begin top-level-form:tlf ...))
     (syntax/loc this-syntax
       (module name lang (#%module-begin top-level-form.erased ...)))]))
