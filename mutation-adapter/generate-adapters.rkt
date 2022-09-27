#lang at-exp racket

(provide generate-adapter-module-for-mutation)

(require "mutation-adapter.rkt"
         "util.rkt"
         "../mutate/type-api-mutators.rkt"
         racket/runtime-path
         syntax/parse)

(struct mutated-type (name original-type new-type struct? definition?) #:transparent)

;; plan: just rename the original interface module to a new name, which then
;; gets passed in here as `mod-path`, and save the resulting adapter module as
;; the original interface name.


;; syntax? syntax? mutation-type? string? -> syntax?
;; `mod-name` should be the name of the interface module corresponding to `original-mod-stx`.
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
  (match-define (mutated-type name original-type new-type mutated-struct? mutated-definition?)
    (find-mutated-type original-mod-stx mutated-mod-stx))
  (define mit (mutated-interface-type original-type new-type mutation-type))
  (cond [mutated-definition?
         ;; lltodo: is this really right?
         #;(adapt-all-negative-referencing-provides mutated-mod-stx name adapter)
         (raise-user-error 'generate-adapter-ctcs-for-mutation
                           "Mutated a type definition, which we haven't really figured out yet.")]
        [mutated-struct?
         (adapt-all-negative-referencing-provides mutated-mod-stx
                                                  name
                                                  (generate-adapter-ctc mit))]
        [else (list (cons name (generate-adapter-ctc mit)))]))

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


(struct name+type (name type struct? definition?) #:transparent)
(define (r/t/p-entry->name+type entry)
  (match entry
    [(list '#:struct (or (list name _) (? symbol? name)) _ ...)
     ;; Would like to do this, but sieve can't apparently support prefabs. It
     ;; slows down to the point of being un-runnable.
     #;(raise-user-error 'interface-types
                       @~a{
                           Found #:struct require/typed/check clause in type interface, @;
                           which are unsupported. Use prefab structs instead.
                           })
     #f]
    [(list name t)
     (name+type name t #f #f)]))
;; (or/c syntax? sexp?) -> (listof name+type?)
(define (top-level-form->types form)
  (match (if (syntax? form)
             (syntax->datum form)
             form)
    [(list (or 'require/typed/provide 'require/typed/check/provide) _ entries ...)
     (filter-map r/t/p-entry->name+type entries)]
    [(list 'define-type name t)
     (list (name+type name t #f #t))]
    [(and struct (list (or 'struct: 'struct) (or (? symbol? name) (list name _)) _ ...))
     (list (name+type name struct #t #f))]
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
                 (list (name+type 'x 'A #f #f)
                       (name+type 'y 'B #f #f)))
    (test-equal? (top-level-form->types '(struct (s blah) ([f : F])))
                 (list (name+type 's '(struct (s blah) ([f : F])) #t #f)))
    (test-equal? (top-level-form->types '(struct s ([f : F])))
                 (list (name+type 's '(struct s ([f : F])) #t #f)))))

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
                           (name+type-struct? t1)
                           (name+type-definition? t1))))
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
                 (mutated-type 'f 'Number 'String #f #f))
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
                 (mutated-type 'f 'Number 'String #f #f))
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
                 (mutated-type 'f 'Number 'String #f #f))
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
                               #f
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
                               #f
                               #t))
    (test-equal? (find-mutated-type
                  #'(module A racket
                      (define-type Z Real)
                      (struct foobar ([a : Real]
                                      [b : Real])
                        #:prefab)
                      (provide (struct-out foobar))
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Number Number))))
                  #'(module A racket
                      (define-type Z Real)
                      (struct foobar ([a : Real]
                                      [b : Number])
                        #:prefab)
                      (provide (struct-out foobar))
                      (require/typed/provide "x.rkt"
                        [f X]
                        [f (-> String (-> Number Number))]
                        [f Y])
                      (define-type Z (-> String (-> Number Number)))))
                 (mutated-type 'foobar
                               '(struct foobar ([a : Real]
                                                [b : Real])
                                  #:prefab)
                               '(struct foobar ([a : Real]
                                                [b : Number])
                                  #:prefab)
                               #t
                               #f))

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
                               #f
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
(define (negative-reference? type id)
  (let loop ([subtype type]
             [negative? #f])
    (match subtype
      [(list '-> arg-ts ... res-t)
       (or (ormap (λ (t) (loop t (not negative?))) arg-ts)
           (loop res-t negative?))]
      [(list '->* mandatory-arg-ts optional-arg-ts res-t)
       (or (ormap (λ (t) (loop t (not negative?)))
                  (append mandatory-arg-ts optional-arg-ts))
           (loop res-t negative?))]
      [(list (or 'struct 'struct: 'define-type)
             (or (binding (? symbol?) #:with [names empty])
                 (list* _ names))
             more ...)
       (loop (append names more) #t)]
      [(? list? sub-exps) (ormap (λ (t) (loop t negative?)) sub-exps)]
      [(? symbol? sym) (and (equal? sym id) negative?)]
      [other #f])))

;; syntax? identifier? contract? -> (listof (cons identifier? contract?))
(define (adapt-all-negative-referencing-provides module-stx name ctc)
  (define all-interface-types (interface-types module-stx))
  (define referencing-name (for/list ([n+t (in-list all-interface-types)]
                                      #:when (negative-reference? (name+type-type n+t) name))
                             n+t))
  (define (another-name+type-definition? a-name+type)
    (and (or (name+type-definition? a-name+type)
             (name+type-struct? a-name+type))
         (not (equal? (name+type-name a-name+type)
                      name))))
  (assert (not (ormap another-name+type-definition? referencing-name))
          #:name 'adapt-all-negative-referencing-provides
          @~a{
              Found one or more type definitions that refer to a mutated type.
              Chaining definition adapters to support this is not implemented yet.

              Mutated type name: @name
              Definitions referring to it:
              @~s[(filter another-name+type-definition? referencing-name)]
              })
  (for/list ([ref (in-list referencing-name)])
    (cons (name+type-name ref)
          (generate-negative-delegating-adapter-ctc (name+type-type ref)
                                                    name
                                                    ctc))))

(module+ test
  (test-begin
    #:name adapt-all-negative-referencing-provides
    (test-match (adapt-all-negative-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (require/typed/provide "x.rkt"
                       [a X]
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
    (test-match (adapt-all-negative-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (provide (struct-out foobar))
                     (require/typed/provide "x.rkt"
                       [a (-> Number Foo)]
                       [b (-> String (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> String (-> Number Number))))
                 'Foo
                 (make-base-type-adapter 'Integer 'Real))
                (list-no-order
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
              (adapt-all-negative-referencing-provides
               #'(module A racket
                   (define-type Z Real)
                   (struct foobar ([a : Real]
                                   [b : Number])
                     #:prefab)
                   (provide (struct-out foobar))
                   (require/typed/provide "x.rkt"
                     [a (-> Number Foo)]
                     [b (-> String (-> Foo Number))]
                     [c Y])
                   (define-type Z2 (-> Foo (-> Number Number))))
               'Foo
               any/c))

    (test-match (adapt-all-negative-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (provide (struct-out foobar))
                     (require/typed/provide "x.rkt"
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
                              (list))))))
    (test-exn (λ (e) (string-contains? (exn-message e)
                                       "not implemented"))
              (adapt-all-negative-referencing-provides
               #'(module A racket
                   (struct YMD ([y : Natural]
                                [m : Month]
                                [d : Natural])
                     #:prefab)
                   (struct Date ([ymd : YMD]
                                 [jdn : Integer])
                     #:prefab)
                   (struct DateTime ([date : Date])
                     #:prefab)
                   (provide (struct-out YMD))
                   (provide (struct-out Date))
                   (provide (struct-out DateTime))
                   (require/typed/provide "x.rkt"
                     [a (-> Number Date)]
                     [b (-> String (-> DateTime Number))]
                     [c Y]))
               'YMD
               any/c))
    (test-match (adapt-all-negative-referencing-provides
                 #'(module A racket
                     (struct YMD ([y : Natural]
                                  [m : Month]
                                  [d : Natural])
                       #:prefab)
                     (struct Date ([ymd : YMD]
                                   [jdn : Integer])
                       #:prefab)
                     (struct DateTime ([date : Date])
                       #:prefab)
                     (struct Moment ([dt : DateTime])
                       #:prefab)
                     (provide (struct-out YMD))
                     (provide (struct-out Date))
                     (provide (struct-out DateTime))
                     (provide (struct-out Moment))
                     (require/typed/provide "x.rkt"
                       [a (-> Number Moment)]
                       [moment
                        (->*
                         (Natural)
                         (Month
                          Natural
                          Natural
                          Natural
                          Natural
                          Natural
                          #:tz
                          (U tz #f)
                          #:resolve-offset
                          (-> (U tzgap tzoverlap) DateTime (U String #f) (U #f Moment) Moment))
                         Moment)]
                       [c Y]))
                 'Moment
                 (sealing-adapter))
                (list
                 (cons 'moment
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->*
                              1
                              (list)
                              (list
                               (cons '#:resolve-offset
                                     (delegating->
                                      (list)
                                      (list (cons 0 (sealing-adapter)))))))))))))


(define-runtime-path type-api-mutators.rkt "mutation-adapter.rkt")
;; (dictof identifier? contract?) syntax? (listof syntax) -> syntax?
(define (adapter-ctcs->module-stx adapter-ctcs
                                  interface-mod-name
                                  interface-mod-stx)
  (define (munge-location+bindings stx)
    (datum->syntax interface-mod-stx
                   (syntax->datum stx)
                   interface-mod-stx))

  (define interface-top-level-forms (extract-top-level-forms interface-mod-stx))
  (define interface-r/t/c/p-forms (filter r/t/c/p-form?
                                          interface-top-level-forms))
  (define interface-req/prov-forms (filter require/provide-form?
                                           interface-top-level-forms))

  (define redirected-interface-r/t/c/p-forms
    (for/list ([form (in-list interface-r/t/c/p-forms)])
      ;; munge the bindings so that they line up with the import of r/t/c/p below
      (munge-location+bindings (r/t/c/p-redirect #''contracted form))))
  (define interface-typedefs
    (for*/list ([form (in-list interface-top-level-forms)]
                [n+t (in-list (top-level-form->types form))]
                #:when (name+type-definition? n+t))
      (datum->syntax interface-mod-stx (name+type-name n+t))))
  (define interface-structs
    (for*/list ([form (in-list interface-top-level-forms)]
                [n+t (in-list (top-level-form->types form))]
                #:when (name+type-struct? n+t))
      (datum->syntax interface-mod-stx (name+type-type n+t))))
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
        #,@interface-req/prov-forms
        (require (only-in 'contracted #,@interface-typedefs))
        (provide #,@interface-typedefs)
        #,@interface-structs
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
           (struct YMD ([y : Integer]) #:prefab)
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
        (struct YMD ([y : Integer]) #:prefab)
        (require/typed/check/provide
         'contracted
         [foo Integer]
         [bar (-> Boolean Integer Void)]
         [baz (-> FOOBAR String)]))))))

#;(define (struct-adapter-ctcs mit)
  (match-define (mutated-interface-type original-type new-type mutation-type)
    mit)
  (cond [(equal? mutation-type type:struct-field-swap)
         (list (cons ))]))

