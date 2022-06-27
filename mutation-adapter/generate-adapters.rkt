#lang at-exp racket

(provide generate-adapter-module-for-mutation)

(require "mutation-adapter.rkt"
         "util.rkt"
         racket/runtime-path
         syntax/parse)

(struct mutated-type (name original-type new-type definition?) #:transparent)

;; plan: just rename the original interface module to a new name, which then
;; gets passed in here as `mod-path`, and save the resulting adapter module as
;; the original interface name.


;; syntax? syntax? mutation-type? string? -> syntax?
;; `mod-name` should be the of the interface module corresponding to `original-mod-stx`.
(define (generate-adapter-module-for-mutation original-mod-stx
                                              mutated-mod-stx
                                              mutation-type
                                              mod-name)
  (define adapter-ctcs (generate-adapter-ctcs-for-mutation original-mod-stx
                                                           mutated-mod-stx
                                                           mutation-type))
  (adapter-ctcs->module-stx adapter-ctcs
                            mod-name
                            original-mod-stx))

;; syntax? syntax? mutation-type? -> (listof (cons identifier? contract?))
(define (generate-adapter-ctcs-for-mutation original-mod-stx
                                            mutated-mod-stx
                                            mutation-type)
  (match-define (mutated-type name original-type new-type mutated-definition?)
    (find-mutated-type original-mod-stx mutated-mod-stx))
  (define mit (mutated-interface-type original-type new-type mutation-type))
  (define adapter (generate-adapter-ctc mit))
  (if mutated-definition?
      (raise-user-error 'generate-adapter-ctcs-for-mutation
                        "Mutated a type definition, which we haven't really figured out yet.")
      ;; lltodo: is this really right?
      #;(adapt-all-referencing-provides mutated-mod-stx name adapter)
      (list (cons name adapter))))

(define r/t/c/p-form?
  (syntax-parser
    [({~datum require/typed/check/provide} _ ...) #t]
    [else #f]))

(define require/provide-form?
  (syntax-parser
    [({~or {~datum require} {~datum reprovide} {~datum provide}} _ ...) #t]
    [else #f]))

(define extract-top-level-forms
  (syntax-parser
    [(module _ _
       (#%module-begin
        forms
        ...))
     (attribute forms)]))


(struct name+type (name type definition? struct?) #:transparent)
(define (r/t/p-entry->name+type entry)
  (match entry
    [(list '#:struct (or (list name _) (? symbol? name)) _ ...) (name+type name entry #f #t)]
    [(list name t)               (name+type name t #f #f)]))
;; (or/c syntax? sexp?) -> (listof name+type?)
(define (top-level-form->types form)
  (match (if (syntax? form)
             (syntax->datum form)
             form)
    [(list (or 'require/typed/provide 'require/typed/check/provide) _ entries ...)
     (map r/t/p-entry->name+type entries)]
    [(list 'define-type name t)
     (list (name+type name t #t #f))]
    [other empty]))
(define (interface-types a-mod-stx)
  (match (syntax->datum a-mod-stx)
    [(or (list 'module name lang (list '#%module-begin top-level-forms ...))
         (list 'module name lang top-level-forms ...))
     (append* (map top-level-form->types top-level-forms))]))

(module+ test
  (require ruinit)
  (test-begin
    #:name top-level-form->types
    (test-equal? (top-level-form->types '(require/typed/check/provide foobar
                                                                      [x A]
                                                                      [y B]))
                 (list (name+type 'x 'A #f #f)
                       (name+type 'y 'B #f #f)))
    (test-equal? (top-level-form->types '(require/typed/check/provide foobar
                                                                      [#:struct s ([f F])]
                                                                      [x A]
                                                                      [y B]))
                 (list (name+type 's '[#:struct s ([f F])] #f #t)
                       (name+type 'x 'A #f #f)
                       (name+type 'y 'B #f #f)))
    (test-equal? (top-level-form->types '(require/typed/check/provide foobar
                                                                      [#:struct (s blah) ([f F])]
                                                                      [x A]
                                                                      [y B]))
                 (list (name+type 's '[#:struct (s blah) ([f F])] #f #t)
                       (name+type 'x 'A #f #f)
                       (name+type 'y 'B #f #f)))))

;; syntax? syntax? -> mutated-type?
(define (find-mutated-type original-mod-stx new-mod-stx)
  (define original-interface-types (interface-types original-mod-stx))
  (define new-interface-types (interface-types new-mod-stx))
  (or (for/or ([t1 (in-list original-interface-types)]
               [t2 (in-list new-interface-types)])
        (and (not (equal? t1 t2))
             (mutated-type (name+type-name t1)
                           (name+type-type t1)
                           (name+type-type t2)
                           (or (name+type-definition? t1)
                               (name+type-struct? t1)))))
      (error 'find-mutated-type
             @~a{
                 Couldn't find mutated type between the original and new mods.
                 original:
                 @pretty-format[(syntax->datum original-mod-stx)]

                 @pretty-format[(syntax->datum new-mod-stx)]

                 })))

(module+ test
  (test-begin
    #:name find-mutated-type
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (require/typed/provide "x.rkt"
                        [f Number]))
                  #'(module A racket
                      (require/typed/provide "x.rkt"
                        [f String])))
                 (mutated-type 'f 'Number 'String #f))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (require/typed/check/provide "x.rkt"
                        [f X]
                        [f Number]
                        [f Y]))
                  #'(module A racket
                      (require/typed/check/provide "x.rkt"
                        [f X]
                        [f String]
                        [f Y])))
                 (mutated-type 'f 'Number 'String #f))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f Number]
                        [f Y]))
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f String]
                        [f Y])))
                 (mutated-type 'f 'Number 'String #f))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z Real))
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Real Number))]
                        [f Y])
                      (define-type Z Real)))
                 (mutated-type 'f
                               '(-> String (-> Number Number))
                               '(-> String (-> Real Number))
                               #f))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Number Number))))
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Real Number)))))
                 (mutated-type 'Z
                               '(-> String (-> Number Number))
                               '(-> String (-> Real Number))
                               #t))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [#:struct foobar ([a : Real]
                                          [b : Real])]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Number Number))))
                  #'(module A racket
                      (define-type Z Real)
                      (require/typed/provide "x.rkt"
                        [f X]
                        [#:struct foobar ([a : Real]
                                          [b : Number])]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Number Number)))))
                 (mutated-type 'foobar
                               '[#:struct foobar ([a : Real]
                                          [b : Real])]
                               '[#:struct foobar ([a : Real]
                                          [b : Number])]
                               #t))

    (test-equal? (find-mutated-type
                  #'(module main racket
                      (#%module-begin
                       (require/typed/check/provide
                        "something"
                        [f (-> Number Real String)])))
                  #'(module main racket
                      (#%module-begin
                       (require/typed/check/provide
                        "something"
                        [f (-> Real Number String)]))))
                 (mutated-type 'f
                               '(-> Number Real String)
                               '(-> Real Number String)
                               #f)))

  (test-begin
    #:name extract-r/t/c/p-forms
    (test-equal? (map syntax->datum
                      (filter r/t/c/p-form?
                              (extract-top-level-forms
                               #'(module type-interface typed-racket
                                   (#%module-begin
                                    (require "../../../utilities/require-typed-check-provide.rkt")
                                    (require/typed/check/provide "library.rkt"
                                                                 [x Integer]))))))
                 '((require/typed/check/provide "library.rkt" [x Integer])))))

(define (sexp-contains? s id)
  (let loop ([s s])
    (match s
      [(? symbol? sym) (equal? sym id)]
      [(list sub-exps ...) (ormap loop sub-exps)]
      [other #f])))

;; syntax? identifier? contract? -> (listof (cons identifier? contract?))
(define (adapt-all-referencing-provides module-stx name ctc)
  (define all-interface-types (interface-types module-stx))
  (define referencing-name (for/list ([n+t (in-list all-interface-types)]
                                      #:when (sexp-contains? (name+type-type n+t) name))
                             n+t))
  (define (another-name+type-definition? a-name+type)
    (and (or (name+type-definition? a-name+type)
             (name+type-struct? a-name+type))
         (not (equal? (name+type-name a-name+type)
                      name))))
  (assert (not (ormap another-name+type-definition? referencing-name))
          #:name 'adapt-all-referencing-provides
          @~a{
              Found one or more type definitions that refer to a mutated type.
              Chaining definition adapters to support this is not implemented yet.

              Mutated type name: @name
              Definitions referring to it:
              @~s[(filter another-name+type-definition? referencing-name)]
              })
  (for/list ([ref (in-list referencing-name)])
    (cons (name+type-name ref)
          (generate-delegating-adapter-ctc (name+type-type ref)
                                           name
                                           ctc))))

(module+ test
  (test-begin
    #:name adapt-all-referencing-provides
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (require/typed/provide "x.rkt"
                       [a X]
                       [#:struct foobar ([a : Real]
                                         [b : Number])]
                       [b (-> String (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> String (-> Number Number))))
                 'Foo
                 (make-base-type-adapter 'Integer 'Real))
                (list-no-order
                 (cons 'b (app (compose1 syntax->datum ->stx)
                               '(delegating->
                                 (list)
                                 (list
                                  (cons 0 (delegating->
                                           (list
                                            (cons 0 #;(make-base-type-adapter 'Integer 'Real)
                                                  (sealing-adapter)))
                                           (list)))))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (require/typed/provide "x.rkt"
                       [a (-> Number Foo)]
                       [#:struct foobar ([a : Real]
                                         [b : Number])]
                       [b (-> String (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> String (-> Number Number))))
                 'Foo
                 (make-base-type-adapter 'Integer 'Real))
                (list-no-order
                 (cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list)
                              (list
                               (cons 0 #;(make-base-type-adapter 'Integer 'Real)
                                     (sealing-adapter))))))
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list)
                              (list
                               (cons 0 (delegating->
                                        (list
                                         (cons 0 #;(make-base-type-adapter 'Integer 'Real)
                                               (sealing-adapter)))
                                        (list)))))))))
    (test-exn (λ (e) (string-contains? (exn-message e)
                                       "not implemented"))
              (adapt-all-referencing-provides
               #'(module A racket
                   (define-type Z Real)
                   (require/typed/provide "x.rkt"
                     [a (-> Number Foo)]
                     [#:struct foobar ([a : Real]
                                       [b : Number])]
                     [b (-> String (-> Foo Number))]
                     [c Y])
                   (define-type Z2 (-> Foo (-> Number Number))))
               'Foo
               any/c))

    ;; This adaptation of `something` requires adapting both arguments and results.
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (require/typed/provide "x.rkt"
                       [#:struct foobar ([a : Real]
                                         [b : Number])]
                       [something (-> Natural (-> stream) stream)]
                       [c Y])
                     (define-type Z2 (-> String (-> Number Number))))
                 'stream
                 (make-base-type-adapter 'Integer 'Index))
                (list-no-order
                 (cons 'something
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 1 (delegating->
                                        (list)
                                        (list (cons 0 #;(make-base-type-adapter 'Integer 'Index)
                                                    (sealing-adapter))))))
                              (list
                               (cons 0 #;(make-base-type-adapter 'Integer 'Index)
                                     (sealing-adapter))))))))))


(define-runtime-path type-api-mutators.rkt "mutation-adapter.rkt")
;; (dictof identifier? contract?) syntax? (listof syntax) [syntax?] -> syntax?
(define (adapter-ctcs->module-stx adapter-ctcs
                                  interface-mod-name
                                  interface-mod-stx)
  (define (munge-location+bindings stx)
    (datum->syntax interface-mod-stx
                   (syntax->datum stx)
                   interface-mod-stx))

  (define original-interface-top-level-forms (extract-top-level-forms interface-mod-stx))
  (define original-interface-r/t/c/p-forms (filter r/t/c/p-form?
                                                   original-interface-top-level-forms))
  (define original-interface-req/prov-forms (filter require/provide-form?
                                                    original-interface-top-level-forms))

  (define redirected-interface-r/t/c/p-forms
    (for/list ([form (in-list original-interface-r/t/c/p-forms)])
      ;; munge the bindings so that they line up with the import of r/t/c/p below
      (munge-location+bindings (r/t/c/p-redirect #''contracted form))))
  (define interface-typedefs
    (for*/list ([form (in-list original-interface-top-level-forms)]
                [n+t (in-list (top-level-form->types form))]
                #:when (name+type-definition? n+t))
      (datum->syntax interface-mod-stx (name+type-name n+t))))
  (munge-location+bindings
    #`(module mutation-adapter typed/racket
      (#%module-begin
       (module contracted racket
         (require (file #,(path->string type-api-mutators.rkt)))
         (require #,interface-mod-name)
         (provide (except-out (all-from-out #,interface-mod-name) #,@(dict-keys adapter-ctcs)))
         (provide (contract-out
                   #,@(for/list ([{id adapter} (in-dict adapter-ctcs)])
                        #`[#,id #,(->stx adapter)]))))
       (require "../../../utilities/require-typed-check-provide.rkt")
       #,@original-interface-req/prov-forms
       (require (only-in 'contracted #,@interface-typedefs))
       (provide #,@interface-typedefs)
       #,@redirected-interface-r/t/c/p-forms))))

;; syntax? syntax? -> syntax?
(define (r/t/c/p-redirect to stx)
  (syntax-parse stx
    [({~datum require/typed/check/provide} source . more)
     #`(require/typed/check/provide #,to . more)]))

(module+ test
  (test-begin
   #:name adapter-ctcs->module-stx
   (test-equal? (syntax->datum
                 (adapter-ctcs->module-stx empty
                                           "interface.rkt"
                                           #'(module ifce tr (mod-begin #f))))
                `(module mutation-adapter typed/racket
                   (#%module-begin
                    (module contracted racket
                      (require (file ,(path->string type-api-mutators.rkt)))
                      (require "interface.rkt")
                      (provide (except-out (all-from-out "interface.rkt")))
                      (provide (contract-out)))
                    (require "../../../utilities/require-typed-check-provide.rkt")
                    (require (only-in 'contracted))
                    (provide))))
   (test-equal?
    (syntax->datum
     (adapter-ctcs->module-stx
      `((foo . ,(make-base-type-adapter 'Integer 'Real))
        (bar . ,(delegating-> (list (cons 1 (make-base-type-adapter 'Integer 'Real))) empty)))
      "interface.rkt"
      #'(module ifce tr
          (mod-begin
           (require "../base/base-types.rkt")
           (reprovide "../base/more-types.rkt")
           (define-type FOOBAR Natural)
           (require/typed/check/provide
            "server.rkt"
            [foo Integer]
            [bar (-> Boolean Integer Void)]
            [baz (-> FOOBAR String)])))))
    `(module mutation-adapter typed/racket
       (#%module-begin
        (module contracted racket
          (require (file ,(path->string type-api-mutators.rkt)))
          (require "interface.rkt")
          (provide (except-out (all-from-out "interface.rkt")
                               foo
                               bar))
          (provide
           (contract-out
            [foo #;(make-base-type-adapter 'Integer 'Real) (sealing-adapter)]
            [bar (delegating->
                  (list (cons 1 #;(make-base-type-adapter 'Integer 'Real) (sealing-adapter)))
                  (list))])))
        (require "../../../utilities/require-typed-check-provide.rkt")
        (require "../base/base-types.rkt")
        (reprovide "../base/more-types.rkt")
        (require (only-in 'contracted FOOBAR))
        (provide FOOBAR)
        (require/typed/check/provide
         'contracted
         [foo Integer]
         [bar (-> Boolean Integer Void)]
         [baz (-> FOOBAR String)]))))))
