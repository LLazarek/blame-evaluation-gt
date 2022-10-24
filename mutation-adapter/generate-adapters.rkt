#lang at-exp racket

(provide generate-adapter-module-for-mutation)

(require "mutation-adapter.rkt"
         (only-in (submod "mutation-adapter.rkt" tds) td:base?)
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
  (cond [(or mutated-definition?
             mutated-struct?)
         (adapt-all-referencing-provides mutated-mod-stx
                                         name
                                         mit)]
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


(struct name+type (name type) #:transparent)
(struct struct-type name+type (parent) #:transparent)
(struct typedef name+type () #:transparent)
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
     (name+type name t)]))
;; (or/c syntax? sexp?) -> (listof name+type?)
(define (top-level-form->types form)
  (match (if (syntax? form)
             (syntax->datum form)
             form)
    [(list (or 'require/typed/provide 'require/typed/check/provide) _ entries ...)
     (filter-map r/t/p-entry->name+type entries)]
    [(list 'define-type name t)
     (list (typedef name t))]
    [(and struct (list (or 'struct: 'struct)
                       (? symbol? name)
                       (or (? symbol? parent)
                           (binding (? list?) #:with [parent #f]))
                       _ ...))
     (list (struct-type name struct parent))]
    [other empty]))
;; syntax? -> (listof name+type?)
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
                 (list (name+type 'x 'A)
                       (name+type 'y 'B)))
    (test-equal? (top-level-form->types '(require/typed/check/provide foobar
                                                                      [#:struct s ([f F])]
                                                                      [x A]
                                                                      [y B]))
                 (list (name+type 'x 'A)
                       (name+type 'y 'B)))
    (test-equal? (top-level-form->types '(struct s blah ([f : F])))
                 (list (struct-type 's '(struct s blah ([f : F])) 'blah)))
    (test-equal? (top-level-form->types '(struct s ([f : F])))
                 (list (struct-type 's '(struct s ([f : F])) #f)))))

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
                           (struct-type? t1)
                           (typedef? t1))))
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


(define-logger adapter-generation)

(define (adapt-all-referencing-provides module-stx name mit)
  (define all-interface-types (interface-types module-stx))
  (define interface-types/except-name
    (filter (match-lambda [(name+type (== name) _) #f]
                          [else #t])
            all-interface-types))
  (define ctc (generate-adapter-ctc mit))
  (define mutation-position-in-mit (mutation-position mit))

  (adapt-types-referencing name
                           (flip mutation-position-in-mit)

                           interface-types/except-name
                           ctc))

;; mutated-interface-type? -> position?
(define (mutation-position mit)
  (match-define (mutated-interface-type original-type new-type mutation-type) mit)
  (define td (sexp->type-diff (sexp-diff original-type new-type)))
  (define pos-of-mutation-td
    (let/ec return
      (type-diff->adapter/delegate
       td
       (hash-ref mutation-type->td-leaf-predicate-hash mutation-type)
       (match-lambda** [{_ pos} (return pos)]))))
  (if (equal? mutation-type type:function-arg-swap)
      ;; The td of a function arg swap is the whole -> type, but the mutation is
      ;; really of the ->'s arguments -- ie "one more left" / swap
      (flip pos-of-mutation-td)
      pos-of-mutation-td))

(define (adapt-types-referencing name
                                 reference-positions-to-adapt

                                 all-interface-types
                                 ctc)
  (log-adapter-generation-info
   @~a{Adapting all references to @name in @reference-positions-to-adapt positions...})

  (define simple-interface-types
    (substitute-type-defs+structs* all-interface-types))
  (define references (for/list ([n+t (in-list simple-interface-types)]
                                #:when (reference? (name+type-type n+t)
                                                   name
                                                   reference-positions-to-adapt))
                       n+t))
  (for/list ([ref (in-list references)])
    (cons (name+type-name ref)
          (generate-delegating-adapter-ctc/references-in-position
           (name+type-type ref)
           name
           ctc
           reference-positions-to-adapt))))

(define (reference? type name position [overall-type-position 'pos])
  (not (set-empty? (references-in/of type (list name) position overall-type-position))))

(define (generate-delegating-adapter-ctc/references-in-position type name ctc position)
  (define type* (type-substitute (list (typedef name (gensym name))) type position))
  (define td (sexp->type-diff (sexp-diff type type*)))
  (type-diff->adapter/delegate td
                               (match-lambda** [{(? td:base?) pos} #t]
                                               [{_ _} #f])
                               (λ _ ctc)))

(define (listof-non-recursive-name+types? l)
  (define definitions (filter (disjoin struct-type? typedef?) l))
  (define defined-names (map name+type-name definitions))
  (define (collect-all-recursive-references t)
    (let loop ([ts-todo (list t)]
               [refs (set)])
      (match ts-todo
        [(cons t more-todo)
         (define t-refs (references-in/of t defined-names))
         (define new-refs (set-subtract t-refs refs))
         (loop (append more-todo (set->list new-refs))
               (set-union refs new-refs))]
        ['() refs])))
  (define defs-to-refs
    (for/hash ([def (in-list definitions)])
      (values (name+type-name def)
              (collect-all-recursive-references (name+type-type def)))))
  ;; name? -> (name? -> boolean?)
  (define (def-refers-transitively-to? target def-name)
    (let loop ([todo (list def-name)]
               [seen (set def-name)])
      (match todo
        [(cons name more-todo)
         (define def-refs (hash-ref defs-to-refs name))
         (or (set-member? def-refs target)
             (loop (append more-todo (set->list (set-subtract def-refs seen)))
                   (set-union seen def-refs)))]
        ['() #f])))
  (for/and ([def (in-list definitions)])
    (define name (name+type-name def))
    (not (def-refers-transitively-to? name name))))

(define (lookup-def name definitions)
  (for/first ([def (in-list definitions)]
              #:when (equal? (name+type-name def) name))
    (name+type-type def)))

;; (symbol? -> (or/c #f sexp?))
;; sexp?
;; [position? position?]
;; ->
;; sexp?
(define (type-substitute/lookup lookup t [position 'any] [overall-type-position 'pos])
  (let loop ([t t]
             [current-position overall-type-position])
    (match t
      [(? symbol? name)
       (or (and (position-match? current-position position)
                (lookup name))
           name)]
      [(list (and struct (or 'struct 'struct:))
             (? symbol? name)
             (and parent (or (? symbol?) (list* (or 'struct 'struct:) _))) ...
             (list (list field-names ': field-ts) ...)
             opts ...)
       (append (list struct name)
               (map (λ (sub-t) (loop sub-t current-position)) parent)
               (list (map (λ (field-name t) (list field-name ': t))
                          field-names
                          (map (λ (sub-t) (loop sub-t current-position)) field-ts))))]
      [(and (list (or 'struct 'struct:) _ ...) unparsed-struct-type)
       ;; it has bitten me enough times to warrant this case
       (error 'type-substitute/lookup
              @~a{unhandled struct type shape: @~s[unparsed-struct-type]})]
      [(list (and -> (or '-> '->*)) arg-ts ... res-t)
       (append (list ->)
               (map (λ (sub-t) (loop sub-t (flip current-position))) arg-ts)
               (list (loop res-t current-position)))]
      [(? list? sub-ts)
       (map (λ (sub-t) (loop sub-t current-position)) sub-ts)]
      [other other])))
(define/contract (type-substitute definitions t [position 'any] [overall-type-position 'pos])
  ({(listof name+type?) any/c} {position? position?} . ->* . any/c)

  (type-substitute/lookup (λ (name) (lookup-def name definitions))
                          t
                          position
                          overall-type-position))
(define ((n+t-type-substitutor definitions [position 'any] [overall-type-position 'pos]) n+t)
  (match-define (name+type name t) n+t)
  (define substituted-t (type-substitute definitions t position overall-type-position))
  (match n+t
    [(struct-type _ _ parent)
     (struct-type name substituted-t parent)]
    [(? typedef?)
     (typedef name substituted-t)]
    [else
     (name+type name substituted-t)]))

(module+ test
  (test-begin
    #:name type-substitute
    (test-equal? (type-substitute (list (typedef 'Z 'Real)
                                        (struct-type
                                         'foobar
                                         '(struct foobar ((a : Real) (b : Number)) #:prefab)
                                         #f)
                                        (typedef 'Z2 '(-> String (-> Number Number))))
                                  'X)
                 'X)
    (test-equal? (type-substitute (list (typedef 'Z 'Real)
                                        (struct-type
                                         'foobar
                                         '(struct foobar ((a : Real) (b : Number)) #:prefab)
                                         #f)
                                        (typedef 'Z2 '(-> String (-> Number Number))))
                                  '(-> String (-> Foo Number)))
                 '(-> String (-> Foo Number)))))

(define/contract (substitute-type-defs+structs* all-interface-types)
  (listof-non-recursive-name+types? . -> . (and/c listof-non-recursive-name+types?
                                                  (listof (not/c (or/c struct-type? typedef?)))))
  (define-values {definitions others}
    (partition (disjoin struct-type? typedef?) all-interface-types))
  ;; Substitute the definitions themselves iteratively to a fix point,
  ;; so that then we can just make a single pass over all the other types.
  (define fully-substituted-definitions
    (let loop ([defs definitions])
      (define new-defs (map (n+t-type-substitutor defs) defs))
      (if (equal? defs new-defs)
          defs
          (loop new-defs))))
  (map (n+t-type-substitutor fully-substituted-definitions) others))

(module+ test
  (test-begin
    #:name listof-non-recursive-name+types?
    (listof-non-recursive-name+types? empty)
    (listof-non-recursive-name+types? (list (name+type 'f
                                                       '(-> A B C))))
    (listof-non-recursive-name+types? (list (typedef 'f
                                                     '(-> A B C))))
    (not (listof-non-recursive-name+types? (list (typedef 'f
                                                          '(-> A f C)))))
    (listof-non-recursive-name+types?
     (list (struct-type 's
                        '(struct s ([a : Number]))
                        #f)))
    (not (listof-non-recursive-name+types?
          (list (struct-type 's
                             '(struct s ([a : s]))
                             #f))))
    (listof-non-recursive-name+types? (list (name+type 'f
                                                       '(-> A B C))
                                            (name+type 'g
                                                       '(-> A B C))
                                            (name+type 'h
                                                       '(-> A B C))
                                            (name+type 'i
                                                       '(-> A B C))))
    (listof-non-recursive-name+types? (list (name+type 'f
                                                       '(-> A B C))
                                            (name+type 'g
                                                       '(-> Natural String))
                                            (name+type 'c 'Number)))
    (listof-non-recursive-name+types? (list (name+type 'f
                                                       '(-> A B C))
                                            (typedef 'B
                                                     '(-> A String))
                                            (typedef 'A '(-> String String))))
    (not (listof-non-recursive-name+types? (list (name+type 'f
                                                            '(-> A B C))
                                                 (typedef 'B
                                                          '(-> A String))
                                                 (typedef 'A '(-> B String)))))
    (listof-non-recursive-name+types?
     (list (name+type 'f
                      '(-> A B C))
           (typedef 'B
                    '(-> A S))
           (typedef 'A '(-> String Number))
           (struct-type 'S
                        '(struct S ([a : Natural]
                                    [b : A]))
                        #f)))
    (not (listof-non-recursive-name+types?
          (list (name+type 'f
                           '(-> A B C))
                (typedef 'B
                         '(-> A S))
                (typedef 'A '(-> String S))
                (struct-type 'S
                             '(struct S ([a : Natural]
                                         [b : A]))
                             #f))))
    (listof-non-recursive-name+types?
     (list (typedef 'Z 'Real)
           (struct-type 'foobar '(struct foobar ((a : Real) (b : Number)) #:prefab) #f)
           (name+type 'a 'X)
           (name+type 'b '(-> String (-> Foo Number)))
           (name+type 'c 'Y)
           (typedef 'Z2 '(-> String (-> Number Number))))))
  (test-begin
    #:name reference?
    (reference? 'A 'A 'pos 'pos)
    (not (reference? 'A 'A 'neg 'pos))
    (not (reference? 'A 'A 'pos 'neg))
    (reference? '(Listof (Boxof A)) 'A 'pos 'pos)
    (not (reference? '(Listof (Boxof A)) 'A 'neg 'pos))
    (not (reference? '(Listof (Boxof A)) 'A 'pos 'neg))
    (not (reference? '(-> A B C) 'A 'pos 'pos))
    (reference? '(-> A B C) 'A 'neg 'pos)
    (not (reference? '(-> A B C) 'A 'neg 'neg))
    (reference? '(-> B C A) 'A 'pos 'pos)
    (not (reference? '(-> B C A) 'A 'pos 'neg))
    (reference? '(-> B C (-> A B)) 'A 'neg)
    (reference? '(-> B C (-> (struct s (struct s-parent ([a : A])) ([suba : NotA])) B)) 'A 'neg))
  (test-begin
    #:name substitute-type-defs+structs*
    (test-equal? (substitute-type-defs+structs* empty)
                 empty)
    (test-equal? (substitute-type-defs+structs* (list (name+type 'f
                                                                 '(-> A B C))))
                 (list (name+type 'f
                                  '(-> A B C))))
    (test-match (substitute-type-defs+structs* (list (name+type 'f
                                                                '(-> A B C))
                                                     (name+type 'g
                                                                '(-> Natural String))
                                                     (name+type 'c 'Number)))
                (list-no-order (name+type 'f
                                          '(-> A B C))
                               (name+type 'g
                                          '(-> Natural String))
                               (name+type 'c 'Number)))
    (test-match (substitute-type-defs+structs* (list (name+type 'f
                                                                '(-> A B C))
                                                     (name+type 'g
                                                                '(-> Natural String))
                                                     (typedef 'A '(-> String String))))
                (list-no-order (name+type 'f
                                          '(-> (-> String String) B C))
                               (name+type 'g
                                          '(-> Natural String))))
    (test-equal? (substitute-type-defs+structs* (list (name+type 'f
                                                                 '(-> A B C))
                                                      (typedef 'B
                                                               '(-> A String))
                                                      (typedef 'A '(-> String String))))
                 (list (name+type 'f
                                  '(-> (-> String String) (-> (-> String String) String) C))))
    (test-equal? (substitute-type-defs+structs* (list (name+type 'f
                                                                 '(-> A B C))
                                                      (typedef 'B
                                                               '(-> A S))
                                                      (typedef 'A '(-> String Number))
                                                      (struct-type 'S
                                                                   '(struct S ([a : Natural]
                                                                               [b : A]))
                                                                   #f)))
                 (list (name+type 'f
                                  '(-> (-> String Number)
                                       (-> (-> String Number)
                                           (struct S ([a : Natural]
                                                      [b : (-> String Number)])))
                                       C))))
    (test-equal? (substitute-type-defs+structs* (list (typedef 'Z 'Real)
                                                      (struct-type
                                                       'foobar
                                                       '(struct foobar ((a : Real) (b : Number)) #:prefab)
                                                       #f)
                                                      (name+type 'a 'X)
                                                      (name+type 'b '(-> String (-> Foo Number)))
                                                      (name+type 'c 'Y)
                                                      (typedef 'Z2 '(-> String (-> Number Number)))))
                 (list (name+type 'a 'X)
                       (name+type 'b '(-> String (-> Foo Number)))
                       (name+type 'c 'Y)))))

;; sexp? (listof symbol?) -> (set/c symbol?)
(define (references-in/of type names [position 'any] [overall-type-position 'pos])
  ;; I know, I know, but this is better than having two functions that walk the
  ;; type tree looking for occurrences of certain names!
  (define names* (apply set names))
  (define refs (mutable-set))
  (void (type-substitute/lookup (λ (name)
                                  (when (set-member? names* name)
                                    (set-add! refs name))
                                  #f)
                                type
                                position
                                overall-type-position))
  (for/set ([r (in-mutable-set refs)]) r))

(module+ test
  (test-begin
    #:name mutation-position
    (test-equal? (mutation-position (mutated-interface-type
                                     'Number
                                     'Any
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(Listof Number)
                                     '(Listof Any)
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(Parameterof (Listof (Boxof Number)))
                                     '(Parameterof (Listof (Boxof Any)))
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(struct s parent
                                        ([c-field : Number]))
                                     '(struct s parent
                                        ([c-field : Any]))
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> Number String)
                                     '(-> Any String)
                                     type:base-type-substitution))
                 'neg)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> Number String Void)
                                     '(-> String Number Void)
                                     type:function-arg-swap))
                 'neg)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> (Values Number String Void))
                                     '(-> (Values String Number Void))
                                     type:function-result-swap))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> (-> Number String))
                                     '(-> (-> Any String))
                                     type:base-type-substitution))
                 'neg)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> String Number)
                                     '(-> String Any)
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> (-> Number String) Void)
                                     '(-> (-> Any String) Void)
                                     type:base-type-substitution))
                 'pos)
    (test-equal? (mutation-position (mutated-interface-type
                                     '(-> (-> (Class (field [a Number] [b String])) String) Void)
                                     '(-> (-> (Class (field [a String] [b Number])) String) Void)
                                     type:class-field-swap))
                 'pos))
  (test-begin
    #:name adapt-all-referencing-provides
    (test-match (adapt-all-referencing-provides
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
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
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
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
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
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (provide (struct-out foobar))
                     (require/typed/provide "x.rkt"
                       [a (-> Z2 Foo)]
                       [b (-> String (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> Foo (-> Number Number))))
                 'Foo
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
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
                                        (list)))))))
                 ;; Nope! That's a positive reference to Foo via Z2.
                 #;(cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list (cons 0
                                          (delegating->
                                           (list (cons 0 (sealing-adapter)))
                                           (list))))
                              (list))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (provide (struct-out foobar))
                     (require/typed/provide "x.rkt"
                       [a (-> Z2 Foo)]
                       [b (-> Z (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> Z (-> Number Number))))
                 'Z
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
                (list-no-order
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (sealing-adapter)))
                              (list))))
                 ;; Nope! That's a positive reverse to Z via Z2
                 #;(cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list (cons 0
                                          (delegating->
                                           (list (cons 0 (sealing-adapter)))
                                           (list))))
                              (list))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (define-type Z Real)
                     (struct foobar ([a : Real]
                                     [b : Number])
                       #:prefab)
                     (provide (struct-out foobar))
                     (require/typed/provide "x.rkt"
                       [a (-> Foo Z2)]
                       [b (-> Z (-> Foo Number))]
                       [c Y])
                     (define-type Z2 (-> Z (-> Number Number))))
                 'Z
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
                (list-no-order
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (sealing-adapter)))
                              (list))))
                 (cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list)
                              (list (cons 0
                                          (delegating->
                                           (list (cons 0 (sealing-adapter)))
                                           (list)))))))))

    (test-match (adapt-all-referencing-provides
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
                 (mutated-interface-type 'Integer 'Real type:base-type-substitution))
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
    (test-match (adapt-all-referencing-provides
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
                       [a (-> Date Number)]
                       [b (-> String (-> DateTime Number))]
                       [c Y]))
                 'YMD
                 (mutated-interface-type '(struct YMD ([y : Natural]
                                                       [m : Month]
                                                       [d : Natural])
                                            #:prefab)
                                         '(struct YMD ([y : Any]
                                                       [m : Month]
                                                       [d : Natural])
                                            #:prefab)
                                         type:base-type-substitution))
                (list-no-order
                 (cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (delegating-struct ; Date
                                        #f
                                        2
                                        (list
                                         (cons 0 (delegating-struct ; YMD
                                                  #f
                                                  3
                                                  (list
                                                   (cons 0 (sealing-adapter)))))))))
                              (list))))
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list)
                              (list
                               (cons 0
                                     (delegating->
                                      (list
                                       (cons 0 (delegating-struct ; DateTime
                                                #f
                                                1
                                                (list
                                                 (cons 0 (delegating-struct ; Date
                                                          #f
                                                          2
                                                          (list
                                                           (cons 0 (delegating-struct ; YMD
                                                                    #f
                                                                    3
                                                                    (list
                                                                     (cons 0 (sealing-adapter))))))))))))
                                      (list)))))))))
    (test-match (adapt-all-referencing-provides
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
                 (mutated-interface-type '(struct Moment ([dt : DateTime])
                                            #:prefab)
                                         '(struct Moment ([dt : Any])
                                            #:prefab)
                                         type:complex-type->Any))
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
                                      (list (cons 0 (delegating-struct
                                                     #f
                                                     1
                                                     (list
                                                      (cons 0 (sealing-adapter))))))))))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (struct exp () #:prefab)
                     (struct app exp ([fun : exp]
                                      [arg : Integer])
                       #:prefab)
                     (struct annotated-app app ([ann : Integer])
                       #:prefab)
                     (require/typed/provide "x.rkt"
                       [a (-> app Number)]
                       [b (-> annotated-app Number)]
                       [c Y]))
                 'app
                 (mutated-interface-type '(struct app exp ([fun : exp]
                                                           [arg : Integer])
                                            #:prefab)
                                         '(struct app exp ([fun : exp]
                                                           [arg : Any])
                                            #:prefab)
                                         type:base-type-substitution))
                (list-no-order
                 (cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (delegating-struct
                                        #f
                                        2
                                        (list
                                         (cons 1 (sealing-adapter))))))
                              (list))))
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (delegating-struct
                                        (delegating-struct
                                         #f
                                         3
                                         (list
                                          (cons 1 (sealing-adapter))))
                                        1
                                        (list))))
                              (list))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (struct exp ([a : Integer]) #:prefab)
                     (struct app exp ([fun : exp]
                                      [arg : Integer])
                       #:prefab)
                     (struct annotated-app app ([ann : Integer])
                       #:prefab)
                     (require/typed/provide "x.rkt"

                       [a (-> app Number)]
                       [b (-> annotated-app Number)]
                       [c Y]))
                 'exp
                 (mutated-interface-type '(struct exp ([a : Integer])
                                            #:prefab)
                                         '(struct exp ([a : Any])
                                            #:prefab)
                                         type:base-type-substitution))
                (list-no-order
                 (cons 'a
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (delegating-struct
                                        (delegating-struct
                                         #f
                                         3
                                         (list
                                          (cons 0 (sealing-adapter))))
                                        2
                                        (list
                                         (cons 0
                                               (delegating-struct
                                                #f
                                                1
                                                (list
                                                 (cons 0 (sealing-adapter)))))))))
                              (list))))
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list
                               (cons 0 (delegating-struct
                                        (delegating-struct
                                         (delegating-struct
                                          #f
                                          4
                                          (list
                                           (cons 0 (sealing-adapter))))
                                         3
                                         (list
                                          (cons 0
                                                (delegating-struct
                                                 #f
                                                 1
                                                 (list
                                                  (cons 0 (sealing-adapter)))))))
                                        1
                                        (list))))
                              (list))))))
    (test-match (adapt-all-referencing-provides
                 #'(module A racket
                     (struct mystruct ([f : (-> Integer String)])
                       #:prefab)
                     (require/typed/provide "x.rkt"
                       [a (-> mystruct Number)]
                       [b (-> String mystruct)]
                       [c Y]))
                 'mystruct
                 (mutated-interface-type '(struct mystruct ([f : (-> Integer String)])
                                            #:prefab)
                                         '(struct mystruct ([f : (-> Any String)])
                                            #:prefab)
                                         type:base-type-substitution))
                (list
                 (cons 'b
                       (app (compose1 syntax->datum ->stx)
                            '(delegating->
                              (list)
                              (list
                               (cons 0 (delegating-struct
                                        #f
                                        1
                                        (list
                                         (cons 0
                                               (delegating->
                                                (list (cons 0 (sealing-adapter)))
                                                (list))))))))))))))


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
  (define top-level-n+ts
    (for*/list ([form (in-list interface-top-level-forms)]
                [n+t (in-list (top-level-form->types form))])
      n+t))
  (define interface-typedefs
    (for/list ([n+t (in-list top-level-n+ts)]
               #:when (typedef? n+t))
      (datum->syntax interface-mod-stx `(define-type ,(name+type-name n+t)
                                          ,(name+type-type n+t)))))
  (define interface-typedef-names
    (for/list ([n+t (in-list top-level-n+ts)]
               #:when (typedef? n+t))
      (datum->syntax interface-mod-stx (name+type-name n+t))))
  (define interface-structs
    (for/list ([n+t (in-list top-level-n+ts)]
               #:when (struct-type? n+t))
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
        #,@interface-typedefs
        (provide #,@interface-typedef-names)
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
        (define-type FOOBAR Natural)
        (provide FOOBAR)
        (struct YMD ([y : Integer]) #:prefab)
        (require/typed/check/provide
         'contracted
         [foo Integer]
         [bar (-> Boolean Integer Void)]
         [baz (-> FOOBAR String)]))))))
