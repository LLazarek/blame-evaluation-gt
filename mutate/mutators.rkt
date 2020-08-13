#lang at-exp racket/base

(require "../util/optional-contracts.rkt"
         (except-in racket/contract/base
                    contract-out))
(provide (contract-out
          [arithmetic-op-swap             mutator/c]
          [boolean-op-swap                mutator/c]
          [class-method-publicity-swap    mutator/c]
          [delete-super-new               mutator/c]
          [data-accessor-swap             mutator/c]
          [nested-list-construction-swap  mutator/c]

          [replace-constants              mutator/c]

          [delete-begin-result-expr       mutator/c]
          [negate-conditionals            mutator/c]
          [replace-class-parent           mutator/c]
          [swap-class-initializers        mutator/c]
          [rearrange-positional-exprs     mutator/c]
          [add-extra-class-method         mutator/c]

          [make-top-level-id-swap-mutator (syntax? . -> . mutator/c)]
          [make-imported-id-swap-mutator  (syntax? program/c . -> . mutator/c )]
          [make-method-call-swap-mutator  (syntax? . -> . mutator/c)]))

(require racket/class
         racket/function
         racket/list
         racket/match
         racket/set
         syntax/parse
         "logger.rkt"
         "mutate-expr.rkt"
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt"
         "../util/program.rkt"
         "../util/path-utils.rkt")

(define-id-mutator arithmetic-op-swap
  #:type "arithmetic-op-swap"
  [+ #:<-> -]
  [* #:-> /]
  [quotient #:-> /]
  [modulo #:-> /]
  [add1 #:<-> sub1])

(define-id-mutator boolean-op-swap
  #:type "boolean-op-swap"
  [and #:<-> or])

(define-id-mutator class-method-publicity-swap
  #:type "class:publicity"
  [define/public #:<-> define/private])

(define-id-mutator delete-super-new
  #:type "class:super-new"
  [super-new #:-> void])

(define-id-mutator mutate-data-structure
  #:type "data-structure-mutability"
  [make-hash #:<-> make-immutable-hash]
  [vector #:<-> vector-immutable])

(define-id-mutator data-accessor-swap
  #:type "data-accessor-swap"
  [car #:<-> cdr])

(define-id-mutator nested-list-construction-swap
  #:type "nested-list-construction-swap"
  [append #:-> cons])

(define-value-mutator replace-constants
  #:type "constant-swap"
  #:bind-value value
  ;; May mess with occurrence typing
  [(? boolean?)              #:-> (not value)]

  ;; Type generalization or subdivision swaps
  [(? number?)               #:-> (- value)]
  [(? integer?)              #:-> (exact->inexact value)]
  [(and (? number?)
        (? zero?))           #:-> 1]
  [(and (? number?)
        (? (negate zero?)))  #:-> 0]
  [(? real?)                 #:-> (* 1+0.0i value)]
  ;; this kind of narrowing probably doesn't help
  ;; [(? inexact?)          (exact-floor value)]

  ;; Blatantly ill-typed swaps
  [(? boolean?)              #:-> (if value 1 0)]
  [(? number?)               #:-> #f]
  [(? string?)               #:-> (string->bytes/utf-8 value)])

(module+ test
  (require ruinit
           racket
           "mutate-test-common.rkt")
  (define mutate-datum (compose-mutators arithmetic-op-swap
                                         boolean-op-swap
                                         class-method-publicity-swap
                                         delete-super-new
                                         mutate-data-structure
                                         data-accessor-swap
                                         replace-constants))


  (define-test (test-datum-mutation-seq orig-v new-vs)
    (define (mutate-value v index)
      (syntax->datum (mutated-stx (mutate-datum (datum->syntax #f v)
                                                index
                                                0))))
    (for/and/test
     ([new-v (in-list new-vs)]
      [i (in-naturals)])
     (extend-test-message
      #:append? #f
      (test-equal? (mutate-value orig-v i)
                   new-v)
      @~a{
          Mutation sequence [@orig-v -> @new-vs] failed on @|new-v|:

          })))
  (define-test-syntax (test-datum-mutations
                       (~or (v {~datum ->} [new-vs ...])
                            (orig {~datum ->} mutated)
                            (left {~datum <->} right)) ...)
    #'(and/test/message
       [(test-programs-equal? (mutated-stx (mutate-datum #'orig 0 0))
                              #'mutated)
        @~a{@'orig -> @'mutated failed:}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'left 0 0))
                              #'right)
        @~a{@'left -> @'right failed:}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'right 0 0))
                              #'left)
        @~a{@'right -> @'left failed:}] ...

       [(test-datum-mutation-seq v (list new-vs ...))
        ""] ...))
  (test-begin
    #:name mutate-datum
    (test-datum-mutations
     ;; Arithmetic operators
     [+ <-> -]
     [* -> /]
     [quotient -> /]
     [modulo -> /]
     [add1 <-> sub1]

     ;; Boolean operators
     [and <-> or]

     ;; Class operators
     [define/public <-> define/private]
     [super-new -> void]

     ;; Data structures
     [make-hash <-> make-immutable-hash]
     [vector <-> vector-immutable]

     ;; Other builtins
     [car <-> cdr]

     ;; Anything else is left alone
     [x -> x]

     ;; Constants
     [1 -> [-1 1.0 0 1+0.0i #f]]
     [5 -> [-5 5.0 0 5+0.0i #f]]
     [2.5 -> [-2.5 0 2.5+0.0i #f]]
     [0 -> [0.0 1 #f]]
     [0.0 -> [-0.0 1 0.0+0.0i #f]]
     [-42 -> [42 -42.0 0 -42-0.0i #f]]
     [#t -> [#f 1]]
     [#f -> [#t 0]]
     ["a" -> [#"a"]]))

  (test-begin
    #:name mutate-datum/preserve-guards
    (mtest mutation-guarded?
           (mutate-datum (mutation-guard #'(not #t))
                         0
                         1))
    (mtest (negate mutation-guarded?)
           (mutate-datum #'(not #t)
                         0
                         1))
    (mtest (compose1 mutation-guarded?
                     second
                     syntax->list)
           (mutate-datum #`(if #,(mutation-guard #'(not #t)) a b)
                         0
                         1))
    (mtest (compose1 not
                     mutation-guarded?
                     second
                     syntax->list)
           (mutate-datum #'(if (not #t) a b)
                         0
                         1))))


(define (delete-begin-result-expr stx mutation-index counter)
  (define (delete-result-expr stx mutation-index counter)
    (log-mutation-type "begin-result-deletion")
    (syntax-parse stx
      [[e ...+ result]
       (maybe-mutate stx
                     (syntax/loc stx [e ...])
                     mutation-index
                     counter)]
      [else
       (no-mutation stx mutation-index counter)]))
  (syntax-parse stx
    #:datum-literals [λ lambda begin begin0 cond]
    [({~and {~seq head ...}
            {~or* begin
                  {~seq {~or* λ lambda} formals}}}
      es ...+ e-result)
     (mdo* (def mutated-es (delete-result-expr (syntax/loc stx [es ... e-result])
                                               mutation-index
                                               counter))
           [return (quasisyntax/loc stx
                     (head ... #,@mutated-es))])]
    [(begin0 e-result es ...+)
     (mdo* (def mutated-es (delete-result-expr (quasisyntax/loc stx
                                                 [#,@(reverse (attribute es)) e-result])
                                               mutation-index
                                               counter))
           [return (quasisyntax/loc stx
                     (begin0 #,@(reverse (syntax->list mutated-es))))])]
    [(cond [test e ...] ...)
     (mdo* (def mutated-case-bodies (mutate-in-seq (syntax->list #'[[e ...] ...])
                                                   mutation-index
                                                   counter
                                                   delete-result-expr))
           [return (syntax-parse mutated-case-bodies
                     [[[mutated-e ...] ...]
                      (syntax/loc stx
                        (cond [test mutated-e ...] ...))])])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name delete-begin-result-expr
    (test-mutator* delete-begin-result-expr
                   #'(begin 1 2 3)
                   (list #'(begin 1 2)
                         #'(begin 1 2 3)))
    (test-mutator* delete-begin-result-expr
                   #'(begin0 1 2 3)
                   (list #'(begin0 2 3)
                         #'(begin0 1 2 3)))
    (test-mutator* delete-begin-result-expr
                   #'(λ _ (displayln 'hi) 42)
                   (list #'(λ _ (displayln 'hi))
                         #'(λ _ (displayln 'hi) 42)))
    (test-mutator* delete-begin-result-expr
                   #'(λ _ 42)
                   (list #'(λ _ 42)))
    (test-mutator* delete-begin-result-expr
                   #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye) 0])
                   (list #'(cond [#t 42] ['true (launch-missiles!)] [else (displayln 'bye) 0])
                         #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye)])
                         #'(cond [#t 42] ['true (launch-missiles!) -42] [else (displayln 'bye) 0])))))

(define (negate-conditionals stx mutation-index counter)
  (define (negate-condition stx mutation-index counter)
    (log-mutation-type "negate-conditional")
    (define new-stx
      (syntax-parse stx
        [{~datum else} stx]
        [condition
         (syntax/loc stx
           (not condition))]))
    (define stx-mutated
      (maybe-mutate stx
                    new-stx
                    mutation-index
                    counter))
    (mmap (if (compound-expr? stx)
              identity
              mutation-guard)
          stx-mutated))

  (syntax-parse stx
    #:datum-literals [cond if]
    [(cond [test . body] ...)
     (define test-stxs (attribute test))
     (mdo* (def mutated-test-stxs (mutate-in-seq test-stxs
                                                 mutation-index
                                                 counter
                                                 negate-condition))
           [return
            (syntax-parse mutated-test-stxs
              [[mutated-test ...]
               (syntax/loc stx
                 (cond [mutated-test . body] ...))])])]
    [(if test then-e else-e)
     (define cond-form #'(cond [test then-e] [else else-e]))
     (mdo* (def mutated-cond-form (negate-conditionals cond-form
                                                       mutation-index
                                                       counter))
           [return
            (syntax-parse mutated-cond-form
              [(cond [mutated-test _] _)
               (syntax/loc stx
                 (if mutated-test then-e else-e))])])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name negate-conditionals
    (test-mutator* negate-conditionals
                   #'(if #t (+ 2 2) 42)
                   (list #'(if (not #t) (+ 2 2) 42)
                         #'(if #t (+ 2 2) 42)))
    (test-mutator* negate-conditionals
                   #'(if (some long (thing here)) (+ 2 2) 42)
                   (list #'(if (not (some long (thing here))) (+ 2 2) 42)
                         #'(if (some long (thing here)) (+ 2 2) 42)))
    (test-mutator* negate-conditionals
                   #'(cond [first 42]
                           [(second?) => values]
                           [else 33])
                   (list #'(cond [(not first) 42]
                                 [(second?) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(not (second?)) => values]
                                 [else 33])
                         #'(cond [first 42]
                                 [(second?) => values]
                                 [else 33])))

    (for/and/test
     ([cond-expr (in-list (list #'#t #'(+ 2 2)))]
      [simple-cond? (in-list '(#t #f))]
      #:when #t
      [index (in-range 2)]
      [mutated-cond-expr (in-list (list #`(not #,cond-expr) cond-expr))])
     (define mutated-result (negate-conditionals #`(if #,cond-expr + -)
                                                 index
                                                 0))
     (define stx (mutated-stx mutated-result))
     (define mutated-cond
       (second (syntax->list stx)))
     (and/test/message
      [(test-programs-equal? stx #`(if #,mutated-cond-expr + -))
       @~a{Mutation is different than expected.}]
      [(if simple-cond?
           (mutation-guarded? mutated-cond)
           (not (mutation-guarded? mutated-cond)))
       @~a{@(if simple-cond?
                "simple"
                "complex") @;
           cond expr is @;
           @(if simple-cond?
                "not guarded when it should be"
                "guarded when it shouldn't be")}]))))

(define (replace-class-parent stx mutation-index counter)
  (log-mutation-type "class:parent-swap")
  (syntax-parse stx
    [(~or ({~and {~datum class}  class-form} superclass:expr . body)
          ({~and {~datum class*} class-form} superclass:expr
                                             interfaces:expr . body))
     (define superclass-stx (attribute superclass))
     (mdo* (def mutated-superclass (maybe-mutate superclass-stx
                                                 (datum->syntax superclass-stx
                                                                'object%
                                                                superclass-stx
                                                                superclass-stx)
                                                 mutation-index
                                                 counter))
           [return
            (quasisyntax/loc stx
              (class-form #,mutated-superclass
                          {~? interfaces}
                          . body))])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name replace-class-parent
    (test-mutator* replace-class-parent
                   #'(class a-parent
                       (field a)
                       (define/public (foo x) x))
                   (list #'(class object%
                             (field a)
                             (define/public (foo x) x))
                         #'(class a-parent
                             (field a)
                             (define/public (foo x) x))))))

(define (swap-class-initializers stx mutation-index counter)
  (log-mutation-type "class:initializer-swap")
  (syntax-parse stx
    [({~and {~or {~datum init-field}
                 {~datum field}}
            field-type}
      {~and no-init-field
            {~or _:id [_:id {~datum :} _]}}
      ...
      [field-id:id other-field-stuff ... initial-value:expr]
      ...)
     (define init-value-stxs (attribute initial-value))
     (mdo* (def rearranged-init-value-stxs
             (rearrange-in-seq init-value-stxs
                               mutation-index
                               counter))
           [return
            (syntax-parse rearranged-init-value-stxs
              [[new-init-value ...]
               (quasisyntax/loc stx
                 (field-type no-init-field ...
                             [field-id other-field-stuff ... new-init-value] ...))])])]
    [({~and {~datum new} new}
      class-e
      {~and positional-initializer {~not (_ ...)}} ...
      [field-id:id other-field-stuff ... initial-value:expr]
      ...)
     (define init-value-stxs (attribute initial-value))
     (mdo* (def rearranged-init-value-stxs
             (rearrange-in-seq init-value-stxs
                               mutation-index
                               counter))
           [return
            (syntax-parse rearranged-init-value-stxs
              [[new-init-value ...]
               (quasisyntax/loc stx
                 (new class-e
                      positional-initializer ...
                      [field-id other-field-stuff ... new-init-value] ...))])])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name swap-class-initializers
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* swap-class-initializers
                     #`(#,field-name
                        mandatory-1
                        [mandatory-2 : T2]
                        [a 1]
                        [b 2]
                        [c 3]
                        [d : T1 4]
                        [e 5])
                     (list #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 2]
                              [b 1]
                              [c 3]
                              [d : T1 4]
                              [e 5])
                           #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 1]
                              [b 2]
                              [c 4]
                              [d : T1 3]
                              [e 5])
                           #`(#,field-name
                              mandatory-1
                              [mandatory-2 : T2]
                              [a 1]
                              [b 2]
                              [c 3]
                              [d : T1 4]
                              [e 5])))
      @~a{Field: @field-name}))

    (test-mutator* swap-class-initializers
                   #'(new my-class 42 [a 5] [b "hi"])
                   (list #'(new my-class 42 [a "hi"] [b 5])
                         #'(new my-class 42 [a 5] [b "hi"])))
    (test-mutator* swap-class-initializers
                   #'(new my-class 42 33 [a 5] [b "hi"] [c 'not-this-one])
                   (list #'(new my-class 42 33 [a "hi"] [b 5] [c 'not-this-one])
                         #'(new my-class 42 33 [a 5] [b "hi"] [c 'not-this-one])))
    (test-mutator* swap-class-initializers
                   #'(new my-class 42 33 [a 5])
                   (list #'(new my-class 42 33 [a 5])))))

(define (rearrange-positional-exprs stx mutation-index counter)
  (log-mutation-type "position-swap")
  (syntax-parse stx
    [({~and head {~not _:special-form}} e ...)
     (define e-stxs (attribute e))
     (mdo* (def rearranged-e-stxs (rearrange-in-seq e-stxs
                                                    mutation-index
                                                    counter))
           [return
            (quasisyntax/loc stx
              (head #,@rearranged-e-stxs))])]
    [else
     (no-mutation stx mutation-index counter)]))
(define-syntax-class special-form
  #:description "special form"
  (pattern {~or {~datum define}
                {~datum define-values}
                {~datum define/contract}
                {~datum lambda}
                {~datum λ}
                {~datum struct}
                {~datum class}
                {~datum if}
                {~datum cond}
                {~datum when}
                {~datum unless}
                {~datum match-define}
                {~datum =>}
                {~datum ==}
                {~datum let}
                {~datum let*}
                {~datum let-values}
                {~datum let*-values}
                {~datum set!}
                {~datum define-syntax-rule}
                {~datum for}
                {~datum for*}
                {~datum for/fold}
                {~datum for*/fold}
                {~datum for/list}
                {~datum for*/list}
                {~datum for/vector}
                {~datum for*/vector}
                {~datum define/public}
                {~datum define/private}
                {~datum define/override}}))

(module+ test
  (test-begin
    #:name rearrange-positional-exprs
    (test-mutator* rearrange-positional-exprs
                   #'(a-function b c d e)
                   (list #'(a-function c b d e)
                         #'(a-function b c e d)
                         #'(a-function b c d e)))
    (test-mutator* rearrange-positional-exprs
                   #'(a-function b c d)
                   (list #'(a-function c b d)
                         #'(a-function b c d)))))

(define (add-extra-class-method stx mutation-index counter)
  (log-mutation-type "class:add-extra-method")
  (syntax-parse stx
    [(~or ({~and {~datum class}  class-form} superclass:expr . body)
          ({~and {~datum class*} class-form} superclass:expr
                                             interfaces:expr . body))
     (mdo* (def extra-method-stx
             (maybe-mutate (syntax/loc stx [])
                           (syntax/loc stx
                             [(define/public (a-nonexistant-method x) x)])
                           mutation-index
                           counter))
           [return
            (quasisyntax/loc stx
              (class-form superclass
                          {~? interfaces}
                          #,@extra-method-stx
                          . body))])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name add-extra-class-method
    (test-mutator* add-extra-class-method
                   #'(class a-parent
                       (field a))
                   (list #'(class a-parent
                             (define/public (a-nonexistant-method x) x)
                             (field a))
                         #'(class a-parent
                             (field a))))))

(require syntax/parse/lib/function-header)

(define (id-list-swap-mutators ids name)
  (for/list ([top-level-id (in-list ids)])
    (define (replace-with-top-level-id stx mutation-index counter)
      (log-mutation-type name)
      (syntax-parse stx
        [ref:id
         #:when (member #'ref ids free-identifier=?)
         (maybe-mutate (attribute ref)
                       top-level-id
                       mutation-index
                       counter)]
        [else
         (no-mutation stx mutation-index counter)]))
    replace-with-top-level-id))

(define (combined-id-list-swap-mutator ids name)
  (define mutators (id-list-swap-mutators ids name))
  (match mutators
    ['()  no-mutation]
    [else (apply compose-mutators mutators)]))

(define (make-top-level-id-swap-mutator mod-stx)
  (define all-top-level-identifiers (top-level-definitions mod-stx))
  (combined-id-list-swap-mutator all-top-level-identifiers "top-level-id-swap"))

;; Analagous to `function-header` but:
;; - matches for plain ids as well as function headers
;; - doesn't check the shape of arguments, so that it can recognize headers with
;;   type annotations too
(define-syntax-class simple-function-or-value-header
  #:attributes [name]
  (pattern {(~or header:simple-function-or-value-header name*:id) . _}
           #:attr name   #'{~? header.name name*})
  (pattern name:id))

(define top-level-definitions
  (syntax-parser
    #:datum-literals [define]
    [{{~or (define header:simple-function-or-value-header . _)
           _} ...}
     (syntax->list #'[header.name ...])]))

(module+ test
  (test-begin
    #:name top-level-definitions
    (test-equal? (map syntax->datum
                      (top-level-definitions
                       #'{(require foo x y)
                          (define v 42)
                          (+ v v)
                          (define (f x) (define y x) y)
                          (f v)}))
                 '(v f))
    (test-equal?
     (map
      syntax->datum
      (top-level-definitions
       #'{(define (word->hyphenation-points word
                                            [min-l  default-min-length]
                                            [min-ll  default-min-left-length]
                                            [min-rl  default-min-right-length])
            42)}))
     '(word->hyphenation-points))
    (test-equal?
     (map
      syntax->datum
      (top-level-definitions
       #'{(: word->hyphenation-points (->* (String) (Index Index Index) (Listof String)))
          (define (word->hyphenation-points word
                                            [min-l : Index default-min-length]
                                            [min-ll : Index default-min-left-length]
                                            [min-rl : Index default-min-right-length])
            (: add-no-hyphen-zone (-> (Listof Index) (Listof Integer)))
            42)}))
     '(word->hyphenation-points)))
  (test-begin
    #:name make-top-level-id-swap-mutator
    (ignore
     (define top-level-id-swap-mutator
       (make-top-level-id-swap-mutator
        #'{(require foobar)
           (define (f x) x)
           (+ 2 2)
           (define (g a b) (/ (f a) b))})))
    (test-mutator* top-level-id-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* top-level-id-swap-mutator
                   #'a
                   (list #'a))
    (test-mutator* top-level-id-swap-mutator
                   #'f
                   (list #'g
                         #'f))
    (test-mutator* top-level-id-swap-mutator
                   #'g
                   (list #'f
                         #'g))

    (ignore
     (define top-level-id-swap-mutator/no-ids
       (make-top-level-id-swap-mutator
        #'{(require foobar)
           (+ 2 y)})))
    (test-mutator* top-level-id-swap-mutator/no-ids
                   #'x
                   (list #'x))
    (test-mutator* top-level-id-swap-mutator/no-ids
                   #'y
                   (list #'y))))


; stx? program/c -> mutator/c
(define (make-imported-id-swap-mutator mod-top-level-forms-stx containing-program)
  (define imported-mods (syntactic-module->imported-module-names mod-top-level-forms-stx))
  (define imported-ids
    (flatten (for/list ([mod-name (in-list imported-mods)])
               (define the-mod (program-mod-with-name mod-name containing-program))
               (syntactic-module->exported-ids the-mod))))
  (combined-id-list-swap-mutator imported-ids "imported-id-swap"))

(define-syntax-class require-spec
  #:description "require spec"
  #:commit
  #:datum-literals [only-in except-in prefix-in rename-in for-syntax submod]
  #:attributes [relative-require-mod-path]
  [pattern {~or relative-require-mod-path:str _:id}]
  [pattern ({~or only-in except-in prefix-in rename-in for-syntax submod}
            inner:require-spec _ ...)
           #:with relative-require-mod-path #'inner.relative-require-mod-path])

(define (syntactic-module->imported-module-names top-level-forms)
  (syntax-parse top-level-forms
    #:datum-literals [require]
    [{{~seq {~not (require _ ...)} ...
            (require spec:require-spec ...)} ...
      {~not (require _ ...)} ...}
     (syntax->datum #'[{~? spec.relative-require-mod-path} ... ...])]))

(define (program-mod-with-name mod-name a-program)
  (define all-mods (cons (program-main a-program) (program-others a-program)))
  (findf (compose1 (path-ends-with mod-name) mod-path)
         all-mods))

(define-syntax-class provide-spec
  #:description "provide spec"
  #:commit
  #:datum-literals [rename-out]
  #:attributes [[provided-ids 1]]
  [pattern provided-id:id
           #:with [provided-ids ...] #'[provided-id]]
  [pattern (rename-out [_ provided-ids] ...)]
  [pattern _
           #:with [provided-ids ...] #'[]])

(define (syntactic-module->exported-ids a-mod)
  (syntax-parse (mod-stx a-mod)
    #:datum-literals [provide]
    [(module _ _
       (#%module-begin
        {~seq {~not (provide _ ...)} ...
              (provide spec:provide-spec ...)} ...
        {~not (provide _ ...)} ...))
     (syntax->list #'[spec.provided-ids ... ... ...])]))

(module+ test
  (test-begin
    #:name syntactic-module->imported-module-names
    (test-equal? (syntactic-module->imported-module-names
                  #'{(provide x)
                     (require "a.rkt")
                     (define z 42)
                     (require "b.rkt" (only-in "c.rkt" c))
                     (define x y)
                     (+ 2 2)})
                 '("a.rkt" "b.rkt" "c.rkt"))
    (test-equal? (syntactic-module->imported-module-names
                  #'[(provide x)
                     (require racket/match
                              (only-in "c.rkt" c)
                              syntax/parse)
                     (define z 42)
                     (main)])
                 '("c.rkt"))
    (test-equal? (syntactic-module->imported-module-names
                  #'{(+ 2 2)})
                 '()))

  (test-begin
    #:name syntactic-module->exported-ids
    (test-equal? (map
                  syntax->datum
                  (syntactic-module->exported-ids
                   (mod "a.rkt"
                        #'(module a racket
                            (#%module-begin
                             (provide x)
                             (require "a.rkt")
                             (define z 42)
                             (provide foobar)
                             (define x y)
                             (+ 2 2))))))
                 '(x foobar))
    (test-equal? (map
                  syntax->datum
                  (syntactic-module->exported-ids
                   (mod "a.rkt"
                        #'(module a racket
                            (#%module-begin
                             (provide (rename-out [z a] [x foo]))
                             (require racket/match
                                      (only-in "c.rkt" c)
                                      syntax/parse)
                             (define z 42)
                             (main))))))
                 '(a foo))
    (test-equal? (map
                  syntax->datum
                  (syntactic-module->exported-ids
                   (mod "a.rkt"
                        #'(module a racket
                            (#%module-begin
                             (+ 2 2))))))
                 '()))

  (test-begin
    #:name make-imported-id-swap-mutator
    (ignore
     (define imported-id-swap-mutator
       (make-imported-id-swap-mutator
        #'{(require foobar
                    "b.rkt"
                    racket/match
                    "c.rkt"
                    "a.rkt")
           (define (f x) (c-1 x))
           (+ 2 2)
           (define (g a b) (/ (f c-2) b-2))}
        (program (mod "main.rkt"
                      #'(module main racket
                          (#%module-begin
                           (provide main-1)
                           (main-1))))
                 (list (mod "a.rkt"
                            #'(module main racket
                                (#%module-begin
                                 42)))
                       (mod "b.rkt"
                            #'(module main racket
                                (#%module-begin
                                 (provide b-1 b-2)
                                 42)))
                       (mod "c.rkt"
                            #'(module main racket
                                (#%module-begin
                                 (provide c-1 c-2)
                                 42))))))))
    (test-mutator* imported-id-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* imported-id-swap-mutator
                   #'f
                   (list #'f))
    (test-mutator* imported-id-swap-mutator
                   #'a-1
                   (list #'a-1))
    (test-mutator* imported-id-swap-mutator
                   #'b-1
                   (list #'b-2
                         #'c-1
                         #'c-2
                         #'b-1))
    (test-mutator* imported-id-swap-mutator
                   #'c-2
                   (list #'b-1
                         #'b-2
                         #'c-1
                         #'c-2))))



(define (make-method-call-swap-mutator mod-stx)
  (define all-methods (method-names-in mod-stx))
  (combined-id-list-swap-mutator all-methods "method-name-swap"))

(define-syntax-class public-method-def
  #:attributes [name]
  #:datum-literals [define/public define/pubment define/public-final]
  (pattern ({~or define/public define/pubment define/public-final}
            (header:simple-function-or-value-header _ ...)
            _ ...)
           #:with name #'header.name))

(define method-names-in
  (syntax-parser
    #:datum-literals [class class*]
    [({~or class class*}
      {~seq {~not _:public-method-def} ...
            def:public-method-def} ...
      {~not _:public-method-def} ...)
     (syntax->list #'[def.name ...])]
    [(inner-es ...)
     (apply set-union
            (map method-names-in
                 (attribute inner-es)))]
    [else empty]))

(module+ test
  (test-begin
    #:name method-names-in
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x x)}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object% (super-new))])
                                                          (new c)
                                                          5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public y 42))])
                                                          (new c)
                                                          5))}))
                 '())
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m))
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x))
                                                                   (define/public (n x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m n))
    (test-equal? (map
                  syntax->datum
                  (method-names-in #'{(define x 5) (+ x (let ([c (class object%
                                                                   (super-new)
                                                                   (define/public (m x) (* 42 x))
                                                                   (define/private (p) (void))
                                                                   (define/public (n x) (* 42 x)))])
                                                          (send (new c) m x)))}))
                 '(m n)))

  (test-begin
    #:name make-method-call-swap-mutator
    (ignore
     (define method-call-swap-mutator
       (make-method-call-swap-mutator
        #'{(define x 5) (+ x (let ([c (class object%
                                        (super-new)
                                        (define/public (m x) (* 42 x))
                                        (define/private (p) (void))
                                        (define/public (n x) (* 42 x)))])
                               (send (new c) m x)))})))
    (test-mutator* method-call-swap-mutator
                   #'x
                   (list #'x))
    (test-mutator* method-call-swap-mutator
                   #'p
                   (list #'p))
    (test-mutator* method-call-swap-mutator
                   #'m
                   (list #'n
                         #'m))
    (test-mutator* method-call-swap-mutator
                   #'n
                   (list #'m
                         #'n))))

