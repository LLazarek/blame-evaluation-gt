#lang racket

(provide exn:fail:contract:blame:transient?
         exn:fail:contract:blame:transient
         transient-get-blame
         boundary
         boundary-pos
         boundary-neg

         try-dynamic-require-transient-blame-exn-predicate!)

(require syntax/parse/define)

;; Returns the predicate recognizing transient blame exns if the module is
;; available, otherwise #f
(define (try-dynamic-require-transient-blame-exn-predicate!)
  (with-handlers ([exn:fail:filesystem:missing-module?
                   ;; if we can't load the transient exn module, it's not
                   ;; installed and we definitely can't get transient blames
                   (const #f)])
    (dynamic-require-transient-id 'exn:fail:contract:blame:transient?)))

(define (dynamic-require-transient-id name)
  (dynamic-require 'typed-racket/utils/shallow-contract-struct name))

(define maybe-exn:fail:contract:blame:transient?
  (try-dynamic-require-transient-blame-exn-predicate!))

(define (exn:fail:contract:blame:transient? v)
  (and maybe-exn:fail:contract:blame:transient?
       (maybe-exn:fail:contract:blame:transient? v)))

(define-simple-macro (define-guarded-d/r-transient-ids name:id ...)
  (begin (define name
           (and maybe-exn:fail:contract:blame:transient?
                (dynamic-require-transient-id 'name)))
         ...))
(define-guarded-d/r-transient-ids
  exn:fail:contract:blame:transient
  transient-get-blame
  boundary
  boundary-pos
  boundary-neg)

