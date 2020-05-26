#lang at-exp racket/base

(require "../util/optional-contracts.rkt"
         (except-in racket/contract/base
                    contract-out))
(provide (contract-out
          [arithmetic-op-swap          mutator/c]
          [boolean-op-swap             mutator/c]
          [class-method-publicity-swap mutator/c]
          [delete-super-new            mutator/c]
          [data-accessor-swap          mutator/c]

          [replace-constants           mutator/c]

          [delete-begin-result-expr    mutator/c]
          [negate-conditionals         mutator/c]
          [replace-class-parent        mutator/c]
          [swap-class-initializers     mutator/c]
          [rearrange-positional-exprs  mutator/c]
          [add-extra-class-method      mutator/c]

          [make-top-level-id-swap-mutator (syntax? . -> . mutator/c)]))

(require racket/class
         racket/function
         racket/match
         syntax/parse
         "logger.rkt"
         "mutate-expr.rkt"
         "mutate-util.rkt"
         "mutated.rkt"
         "mutator-lib.rkt")

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
  (log-mutation-type "begin-result-deletion")
  (syntax-parse stx
    #:datum-literals [begin begin0]
    [(begin es ...+ e-result)
     (maybe-mutate stx
                   (syntax/loc stx
                     (begin es ...))
                   mutation-index
                   counter)]
    [(begin0 e-result es ...+)
     (maybe-mutate stx
                   (syntax/loc stx
                     (begin0 es ...))
                   mutation-index
                   counter)]
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
                         #'(begin0 1 2 3)))))

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
      {~or [field-id:id other-field-stuff ... initial-value:expr]
           no-init-field:id}
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
                 (field-type [field-id other-field-stuff ... new-init-value] ...
                             no-init-field ...))])])]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name swap-class-initializers
    (for/and/test
     ([field-name (in-list (list #'field #'init-field))])
     (extend-test-message
      (test-mutator* swap-class-initializers
                     #`(#,field-name [a 1]
                        [b 2]
                        [c 3]
                        [d : T1 4]
                        [e 5])
                     (list #`(#,field-name [a 2]
                              [b 1]
                              [c 3]
                              [d : T1 4]
                              [e 5])
                           #`(#,field-name [a 1]
                              [b 2]
                              [c 4]
                              [d : T1 3]
                              [e 5])
                           #`(#,field-name [a 1]
                              [b 2]
                              [c 3]
                              [d : T1 4]
                              [e 5])))
      @~a{Field: @field-name}))))

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
(define (make-top-level-id-swap-mutator program-stx)
  (define all-top-level-identifiers
    (top-level-definitions program-stx))
  (define top-level-id-swap-mutators
    (for/list ([top-level-id (in-list all-top-level-identifiers)])
      (define (replace-with-top-level-id stx mutation-index counter)
        (log-mutation-type "top-level-id-swap")
        (syntax-parse stx
          [ref:id
           #:when (member #'ref all-top-level-identifiers free-identifier=?)
           (maybe-mutate (attribute ref)
                         top-level-id
                         mutation-index
                         counter)]
          [else
           (no-mutation stx mutation-index counter)]))
      replace-with-top-level-id))
  (match top-level-id-swap-mutators
    ['()  no-mutation]
    [else (apply compose-mutators
                 top-level-id-swap-mutators)]))

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
