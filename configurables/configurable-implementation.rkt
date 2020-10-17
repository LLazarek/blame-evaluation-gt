#lang at-exp racket/base

(require racket/contract
         racket/format
         racket/runtime-path
         "../util/path-utils.rkt")

(provide define-configurable
         define-implementation
         configure!
         current-configuration-path
         call-with-configuration
         (contract-out
          [install-configuration!
           (path-to-existant-file? . -> . void?)]))

(require syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     racket/path
                     racket/format
                     syntax/location))

(define-simple-macro (define-configurable name
                       #:provides [provided:id ...]
                       {~and body ({~literal define-implementation} . impl-body)} ...)
  #:with [provided-param ...] (for/list ([provided-id (in-list (attribute provided))])
                                (format-id this-syntax "configured:~a" provided-id))
  #:with install-params! (datum->syntax #'name 'install-params!)
  #:with provides (datum->syntax #'name 'provides)
  (begin
    (define provided-param (make-parameter #f)) ...
    (provide provided-param ...)
    (module+ name
      (module+ provides (provide provided-param ...))
      (define (install-params! mod-path)
        (provided-param (dynamic-require `(file ,mod-path) 'provided))
        ...)
      (define-implementation name . impl-body) ...)))

(define-runtime-path configurables-dir "../configurables")

(define-simple-macro (define-implementation configurable-name name
                       #:module path-expr
                       ;; lltodo: contracts should be specified here too
                       {~alt {~optional {~seq #:parameters [parameter:id ...]}
                                        #:defaults ([(parameter 1) '()])}
                             {~optional {~seq #:fixed-parameters ([config-parameter:id config-parameter-expr] ...)}
                                        #:defaults ([(config-parameter 1) '()]
                                                    [(config-parameter-expr 1) '()])}}
                       ...)
  #:with install-params! (datum->syntax #'name 'install-params!)
  (begin
    (define path (path->string (build-path configurables-dir path-expr)))
    (define (name {~? {~@ parameter ...}})
      ((dynamic-require `(file ,path) 'config-parameter) config-parameter-expr) ...
      ((dynamic-require `(file ,path) 'parameter) parameter) ...
      (install-params! path))
    (provide name)))

(define-simple-macro (configure! configurable:id implementation:id parameter-value ...)
  #:do {(define configurables-parent-dir
          (find-relative-path (syntax-source-directory this-syntax)
                              (path-only (quote-source-file))))
        (define configurables-rel-path
          (path->string (build-path configurables-parent-dir
                                    "configurables.rkt")))}
  ;; local-require so that different configurables can have implementations with the same name
  #:with parameter-require-expr
  (datum->syntax #'configurable
                 `(local-require (submod ,configurables-rel-path ,#'configurable)))
  (begin
    (let ()
      parameter-require-expr
      (implementation parameter-value ...))))

(define current-configuration-path/private (make-parameter #f))
(define (current-configuration-path) (current-configuration-path/private))

(define (install-configuration! path)
  (define path-string (~a path))
  (current-configuration-path/private path-string)
  (dynamic-require `(file ,path-string) #f))

(define (call-with-configuration configuration-path
                                 thunk)
  (define old-config (current-configuration-path/private))
  (install-configuration! configuration-path)
  (begin0 (thunk)
    (when old-config
      (install-configuration! old-config))))
