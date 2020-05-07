#lang at-exp racket

(provide
 top-level-selector/c
 (contract-out
  [rename mutate-program/no-counter
          mutate-program
          ;; note: result may be unchanged!
          ({syntax?
            natural?}
           {#:expression-filter (syntax? . -> . boolean?)
            #:top-level-select top-level-selector/c}
           . ->* .
           mutated-program?)]
  [mutate-syntax
   ;; note: result may be unchanged!
   ({syntax?
     natural?}
    {#:expression-filter (syntax? . -> . boolean?)
     #:top-level-select top-level-selector/c}
    . ->* .
    syntax?)]

  [select-all             top-level-selector/c]
  [select-define/contract top-level-selector/c]
  [select-any-define      top-level-selector/c]
  [leftmost-identifier-in (syntax? . -> . symbol?)])
 mutation-index-exception?
 (struct-out mutated-program))

(require (for-syntax syntax/parse)
         syntax/parse
         syntax/parse/define
         racket/match
         "mutated.rkt"
         "mutator-lib.rkt"
         "logger.rkt")

#|----------------------------------------------------------------------|#
;; Data; see mutated.rkt
(define make-mutants #t)
(struct mutation-index-exception ())

#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Test setup
(module+ test
  (require ruinit
           "mutate-test-common.rkt"))


#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Mutators: perform simple syntactic mutations

(define-id-mutator mutate-arithmetic-op
  #:type "arithmetic-op-swap"
  [+ #:<-> -]
  [* #:-> /]
  [quotient #:-> /]
  [modulo #:-> /]
  [add1 #:<-> sub1])

(define-id-mutator mutate-boolean-op
  #:type "boolean-op-swap"
  [and #:<-> or])

(define-id-mutator mutate-class-publicity
  #:type "class:publicity"
  [define/public #:<-> define/private])

(define-id-mutator mutate-super-new
  #:type "class:super-new"
  [super-new #:-> void])

;; The benchmarks were written before this distinction was made in TR
;; so these mutations aren't useful
#;(define-id-mutator mutate-data-structure
  #:type "data-structure-mutability"
  [make-hash #:<-> make-immutable-hash]
  [vector #:<-> vector-immutable])

(define-id-mutator mutate-data-accessors
  #:type "data-accessor-swap"
  [car #:<-> cdr])

(define-value-mutator mutate-constant
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

(define mutate-datum (compose-mutators mutate-arithmetic-op
                                       mutate-boolean-op
                                       mutate-class-publicity
                                       mutate-super-new
                                       mutate-data-accessors
                                       mutate-constant))


(module+ test
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
     ["a" -> [#"a"]])))


;; negates
(define/contract (mutate-condition c
                                   mutation-index
                                   counter
                                   mutate-expr)
  (syntax?
   mutation-index?
   counter?
   (syntax? mutation-index? counter? . -> . mutated?)
   . -> .
   mutated?)

  (define is-complex-condition?
    (syntax-parse c
      [(e ...)        #t]
      [a-single-value #f]))
  (define (negate-cond c current-counter)
    (log-mutation-type "negate-conditional")
    (maybe-mutate c
                  (quasisyntax/loc c
                    (not #,c))
                  mutation-index
                  current-counter))
  (mdo (count-with [__ counter])
       (def negated-c (if (and (not (equal? (syntax->datum c) 'else))
                               (<= __ mutation-index))
                          (negate-cond c __)
                          (mutated c __)))
       (def mutated-c (if is-complex-condition?
                          (mutate-expr negated-c
                                       mutation-index
                                       __)
                          (mutated negated-c __)))
       [return mutated-c]))

(struct class-parts (superclass body-stxs) #:transparent)
;; lltodo: rename mutate-expr* to mutate-subexprs
;; lltodo: add `mutated/c` to specify shape of inner data
(define/contract (mutate-class the-class-parts
                               mutation-index
                               counter
                               mutate-expr)
  (class-parts?
   mutation-index?
   counter?
   (syntax? mutation-index? counter? . -> . mutated?)
   . -> .
   mutated?)

  (define (mutate-class-body-expr stx mutation-index current-counter)
    (syntax-parse stx
      [((~or (~and (~datum init-field)
                   field-type)
             (~and (~datum field)
                   field-type))
        (~or [field-id:id initial-value:expr]
             no-init-field:id) ...)
       (log-mutation-type "class-initializer-swap")
       (define init-value-stxs (syntax-e (syntax/loc stx
                                           (initial-value ...))))
       (define field-ids (syntax-e (syntax/loc stx
                                     (field-id ...))))
       (mdo [count-with (__ current-counter)]
            (def init-values/rearranged
              (rearrange-in-seq init-value-stxs
                                mutation-index
                                __))
            (def init-values/mutated
              ;; Descend into initializer expressions to apply other mutations
              (mutate-in-seq init-values/rearranged
                             mutation-index
                             __
                             mutate-expr))
            [return (quasisyntax/loc stx
                      (field-type #,@(map list
                                          field-ids
                                          init-values/mutated)
                                  no-init-field ...))])]
      [else
       ;; Descend into class body to apply other mutations
       (mutate-expr stx
                    mutation-index
                    current-counter)]))

  (mdo (count-with [__ counter])
       (def/value (class-parts superclass body-stxs) the-class-parts)
       (def superclass/mutated (maybe-mutate superclass
                                             (datum->syntax superclass
                                                            'object%
                                                            superclass
                                                            superclass)
                                             mutation-index
                                             __))
       (def body/mutated (mutate-in-seq body-stxs
                                        mutation-index
                                        __
                                        mutate-class-body-expr))
       [return (class-parts superclass/mutated
                            body/mutated)]))

;; end mutators
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Syntax traversers
;;
;; These functions traverse the program syntax finding where to apply
;; mutations.
;; Traversers can only:
;; 1. Apply structural/semantic mutations like moving expressions around
;;    or negating conditionals
;; 2. Use one of the above mutators to perform syntax substitions

(define-syntax-class method/public-or-override
  #:description "define/public or /override method"
  (pattern (~or ((~datum define/public) id+other/pub ...)
                ((~datum define/override) id+other/over ...))))
(define-syntax-class non-keyword
  #:description "non-keyword form"
  (pattern (~not (~or (~datum #%module-begin)
                      (~datum #%datum)
                      (~datum quote)
                      (~datum define)
                      (~datum define-values)
                      (~datum define/contract)
                      (~datum #%app)
                      (~datum if)
                      (~datum lambda)
                      (~datum λ)
                      (~datum struct)
                      (~datum class)
                      (~datum instantiate)
                      (~datum new)
                      (~datum super-instantiate)
                      (~datum super-new)
                      (~datum send)
                      (~datum get-field)
                      (~datum cond)
                      (~datum when)
                      (~datum unless)
                      (~datum match)
                      (~datum match*)
                      (~datum match-define)
                      (~datum =>)
                      (~datum ==)
                      (~datum begin0)
                      (~datum let)
                      (~datum let*)
                      (~datum let-values)
                      (~datum let*-values)
                      (~datum set!)
                      (~datum define-syntax-rule)
                      (~datum for)
                      (~datum for*)
                      (~datum for/fold)
                      (~datum for*/fold)
                      (~datum for/list)
                      (~datum for*/list)
                      (~datum for/vector)
                      (~datum for*/vector)
                      (~datum this)
                      (~datum init)
                      (~datum init-field)
                      (~datum inherit-field)
                      (~datum define/public)
                      (~datum define/private)
                      (~datum define/override)
                      (~datum delay)))))

(define/contract (mutate-expr stx mutation-index counter
                              #:filter [can-mutate? (const #t)])
  ({syntax? mutation-index? counter?}
   {#:filter (syntax? . -> . boolean?)}
   . ->* .
   mutated?)

  (if (and make-mutants
           (<= counter mutation-index)
           (can-mutate? stx))
      (syntax-parse stx
        ;; begin: statement deletion
        [((~datum begin) e1 e2 ...+)
         (mutate-begin-seq stx (syntax/loc stx
                                 (begin e2 ...))
                           mutation-index counter
                           #:filter can-mutate?)]
        [((~datum begin0) e1 e2 e3 ...)
         (mutate-begin-seq stx (syntax/loc stx
                                 (begin0 e1 e3 ...))
                           mutation-index counter
                           #:filter can-mutate?)]

        ;; conditionals: negate condition, or mutate result exprs
        [((~datum cond) clause ...)
         (define clauses-stxs (syntax-e (syntax/loc stx
                                          (clause ...))))
         (mdo* (def clauses (mutate-cond-in-seq clauses-stxs
                                                mutation-index
                                                counter
                                                #:filter can-mutate?))
               [return (quasisyntax/loc stx
                         (cond #,@clauses))])]
        [((~datum if) cond-e then-e else-e)
         (define clause-stx (list (quasisyntax/loc stx
                                    [#,@(rest (syntax-e stx))])))
         (mdo* (def clause (mutate-cond-in-seq clause-stx
                                               mutation-index
                                               counter
                                               #:filter can-mutate?))
               [return (quasisyntax/loc stx
                         (if #,@(first clause)))])]

        ;; classes
        [(~or ((~and (~datum class)  class-form) superclass:expr
                                                 body:expr ...)
              ((~and (~datum class*) class-form) superclass:expr interfaces:expr
                                                 body:expr ...))
         (mdo* (def (class-parts superclass/mutated body-stxs/mutated)
                 (mutate-class
                  (class-parts (syntax/loc stx
                                 superclass)
                               (syntax-e (syntax/loc stx
                                           [body ...])))
                  mutation-index
                  counter
                  (curry mutate-expr
                         #:filter can-mutate?)))
               [return (quasisyntax/loc stx
                         (class-form #,superclass/mutated
                                     {~? interfaces}
                                     #,@body-stxs/mutated))])]

        ;; Mutate function applications:
        ;; Move around arguments, or mutate argument expressions
        [(f:non-keyword arg ...)
         (log-mutation-type "argument-swap")
         (define args-stxs (syntax-e (syntax/loc stx (arg ...))))
         (mdo [count-with (__ counter)]
              (def args/rearranged (rearrange-in-seq args-stxs
                                                     mutation-index
                                                     __))
              (def f/mutated (mutate-expr (syntax/loc stx f)
                                          mutation-index
                                          __
                                          #:filter can-mutate?))
              (def args/mutated (mutate-in-seq args/rearranged
                                               mutation-index
                                               __
                                               (curry mutate-expr
                                                      #:filter can-mutate?)))
              [return (quasisyntax/loc stx
                        (#,f/mutated #,@args/mutated))])]

        ;; Mutate arbitrary sexp, including keyword forms, by just descending
        ;; into their parts (i.e. same as above clause, but don't move their
        ;; parts around)
        [(e ...)
         (mutate-expr* stx mutation-index counter
                       #:filter can-mutate?)]

        ;; anything else, just try mutate-datum
        [e
         (mutate-datum stx mutation-index counter)])

      (mutated stx counter)))

;; applies `mutate-expr` to all subexprs of `stx`
(define (mutate-expr* stx mutation-index counter
                      #:filter can-mutate-expr?)
  (mdo* (def parts (mutate-in-seq (syntax-e stx)
                                  mutation-index
                                  counter
                                  (curry mutate-expr
                                         #:filter can-mutate-expr?)))
        [return (quasisyntax/loc stx
                  (#,@parts))]))

(define/contract (rearrange-in-seq args-stxs mutation-index counter)
  ((listof syntax?)
   mutation-index?
   counter?
   . -> .
   (mutated/c (listof syntax?)))

  (define-values (pairs remainder) (pair-off args-stxs))
  (mdo* (def pairs/swapped (mutate-in-seq pairs
                                          mutation-index
                                          counter
                                          rearrange-pair))
        [return (unpair-off pairs/swapped remainder)]))

(define/contract (rearrange-pair args mutation-index counter)
  ((list/c syntax? syntax?) mutation-index? counter? . -> . (mutated/c syntax?))

  (match-define (list arg1 arg2) args)
  (define unmutated-pair (quasisyntax/loc arg1
                           (#,arg1 #,arg2)))
  (if (> counter mutation-index)
      (mutated unmutated-pair counter)
      ;; note that the swapped pair may be syntactically identical to
      ;; the original. `maybe-mutate` handles this issue
      ;; automatically.
      (maybe-mutate unmutated-pair
                    (quasisyntax/loc arg1
                      (#,arg2 #,arg1))
                    mutation-index
                    counter)))

(define (pair-off lst)
  (for/fold ([paired empty]
             [current-pair empty]
             #:result (values (reverse paired) current-pair))
            ([el (in-list lst)])
    (if (empty? current-pair)
        (values paired (list el))
        (values (cons (list (first current-pair) el)
                      paired)
                empty))))

(define (unpair-off pairs remainder)
  (append (flatten (map syntax-e pairs)) remainder))

(module+ test
  (test-begin
    #:name rearrange-in-seq
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax-e #'(a (+ 1 2)))
                         0 0)))
     #'((+ 1 2) a))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax-e #'(a (+ 1 2) b))
                         0 0)))
     #'((+ 1 2) a b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax-e #'(a (+ 1 2) b (foo 3)))
                         1 0)))
     #'(a (+ 1 2) (foo 3) b))
    (test-programs-equal?
     #`(#,@(mutated-stx (rearrange-in-seq
                         (syntax-e #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
                         2 0)))
     #'(a (+ 1 2) b (foo 3) (bar 3 4 5)))
    #| ... |#))


(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~and (~datum define/contract)
                  def/c)
            id/sig ctc body ...)))
(define-syntax-class definition
  #:description "define form"
  (pattern (def-form id/sig body ...)
           #:when (regexp-match? #rx"define"
                                 (symbol->string (syntax->datum #'def-form)))))

(define (leftmost-identifier-in stx)
  (match (flatten (list (syntax->datum stx)))
    [(list* (? symbol? s) _) s]
    [else '<no-name-found>]))

(define top-level-selector/c
  (->i ([stx syntax?])
       (values [parts-to-mutate (or/c #f (listof syntax?))]
               [mutated-id (or/c #f symbol?)]
               [reconstruct-stx (or/c #f ((listof syntax?) . -> . syntax?))])
       #:post/desc {parts-to-mutate mutated-id reconstruct-stx}
       (or (andmap false? (list parts-to-mutate
                                mutated-id
                                reconstruct-stx
                                ;; force to bool
                                #f))
           (andmap identity (list parts-to-mutate
                                  mutated-id
                                  reconstruct-stx
                                  ;; force to bool
                                  #t))
           "Either all results must be #f or all must be non-#f.")))

(define (select-all stx)
  (define name (leftmost-identifier-in stx))
  (match (syntax-e stx)
    [(? list? stx/list)
     (values stx/list
             name
             (λ (stxs/mutated)
               (datum->syntax stx stxs/mutated)))]
    [else
     (values (list stx)
             name
             (match-lambda
               [(list stx/datum/mutated)
                stx/datum/mutated]
               [a-bigger-list
                (error 'select-all
                       @~a{
                           Mutation produced multiple stxs from one stx?
                           Original: @stx
                           Mutated: @a-bigger-list
                           })]))]))
(define (select-define/contract stx)
  (syntax-parse stx
    [def:contracted-definition
     (define body-stxs (syntax-e (syntax/loc stx
                                   (def.body ...))))
     (define (reconstruct-definition body-stxs/mutated)
       (quasisyntax/loc stx
         (def.def/c def.id/sig def.ctc
           #,@body-stxs/mutated)))
     (values body-stxs
             (leftmost-identifier-in #'def.id/sig)
             reconstruct-definition)]
    [_ (values #f #f #f)]))
(define (select-any-define stx)
  (syntax-parse stx
    [def:definition
     (define body-stxs (syntax-e (syntax/loc stx
                                   (def.body ...))))
     (define (reconstruct-definition body-stxs/mutated)
       (quasisyntax/loc stx
         (def.def-form def.id/sig
           #,@body-stxs/mutated)))
     (values body-stxs
             (leftmost-identifier-in #'def.id/sig)
             reconstruct-definition)]
    [_ (values #f #f #f)]))

;; Note: distinction between mutate-program and mutate-expr is necessary because
;; mutate-program may want to descend only into certain top level forms, while
;; mutate-expr can descend into everything (mutate-program acts like its
;; gatekeeper)
(define/contract (mutate-program stx mutation-index [counter 0]
                                 #:expression-filter [mutate-expr? (const #t)]
                                 #:top-level-select [select select-all])
  ((syntax? mutation-index?)
   (counter?
    #:expression-filter (syntax? . -> . boolean?)
    #:top-level-select top-level-selector/c)
   . ->* .
   (mutated/c mutated-program?))

  (let mutate-program-rest ([stx stx]
                            [mutation-index mutation-index]
                            [counter counter])
    (if (and make-mutants
             (<= counter mutation-index))
        (syntax-parse stx
          [(top-level-e e ...)
           #:do [(define-values {body-stxs top-level-id reconstruct-e}
                   (select #'top-level-e))]
           #:when body-stxs
           (mdo [count-with (__ counter)]
                ;; try mutating this definition
                (def body-stxs/mutated
                  (mutate-in-seq body-stxs
                                 mutation-index
                                 __
                                 (curry mutate-expr
                                        #:filter mutate-expr?)))
                (def/value body-stxs-mutated?
                  (mutation-applied-already? mutation-index __))
                ;; move on to the rest of the program, if necessary
                (def (mutated-program program-rest mutated-fn-in-rest)
                  (mutate-program-rest (syntax/loc stx (e ...))
                                       mutation-index
                                       __))
                [return
                 (mutated-program
                  (quasisyntax/loc stx
                    (#,(reconstruct-e body-stxs/mutated)
                     #,@program-rest))
                  (if body-stxs-mutated?
                      top-level-id
                      mutated-fn-in-rest))])]

          ;; Ignore anything else
          [(other-e e ...)
           (mdo* (def (mutated-program rest-stxs mutated-fn-in-rest)
                   (mutate-program-rest (syntax/loc stx (e ...))
                                        mutation-index
                                        counter))
                 [return
                  (mutated-program
                   (quasisyntax/loc stx
                     (other-e #,@rest-stxs))
                   mutated-fn-in-rest)])]
          [()
           ;; signal no more mutations in this module
           (raise (mutation-index-exception))])

        (mutated (mutated-program stx #f) counter))))


;; mutate-program: syntax? natural? -> mutated-program?
(define mutate-program/no-counter
  (compose1 (match-lambda [(mutated program counter)
                           program])
            mutate-program))

;; mutate-syntax: syntax? natural? -> syntax?
(define mutate-syntax
  (compose1 (match-lambda [(mutated-program stx mutated-id)
                           stx])
            mutate-program/no-counter))

(define (mutate-syntax/define/c stx
                                 mutation-index)
  (match-define (mutated-program mutated-stx mutated-id)
    (mutate-program/define/c stx mutation-index))
  mutated-stx)

(define (mutate-program/define/c stx mutation-index)
  (mutate-program/no-counter stx mutation-index
                             #:expression-filter (const #t)
                             #:top-level-select select-define/contract))

;; end syntax traversers
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Sequence helpers
;;
;; These functions select expressions to mutate from a sequence of
;; expressions, primarily to handle the bookkeeping of the counter
;; value for such cases

(define/contract (mutate-in-seq stxs mutation-index counter
                                mutator)
  (parametric->/c
   [A]
   ((listof (and/c (or/c syntax?
                         (listof syntax?))
                   A))
    mutation-index?
    counter?
    (A mutation-index? counter? . -> . mutated?)
    . -> .
    mutated?))

  (for/fold ([mutated-so-far (mutated '() counter)]
             #:result (mmap reverse mutated-so-far))
            ([stx (in-list stxs)])
    (mdo [count-with (__ #f)]
         (def stxs-so-far mutated-so-far)
         (def element (mutator stx
                               mutation-index
                               __))
         [return (cons element stxs-so-far)])))

(module+ test
  (define-test (test-mutation/in-seq orig-seq
                                     seq-mutator
                                     mutator
                                     pretty-printer
                                     expects)
    (for ([index (in-list (map first expects))]
          [expect (in-list (map second expects))])
      (define ms (mutated-stx (seq-mutator orig-seq
                                           index
                                           0
                                           mutator)))
      (unless (andmap programs-equal?
                      (flatten ms)
                      (flatten expect))
        (fail "Result does not match expected output.
Mutation index: ~v
Expected:
~a

Actual:
~a
"
              index
              (pretty-printer expect)
              (pretty-printer ms)))))

  (test-begin
    #:name mutate-in-seq
    (test-mutation/in-seq
     (list #'#t
           #''a
           #'1
           #'(+ 1 2))
     mutate-in-seq
     mutate-datum
     (curry map syntax->datum)
     ;; only elements 0 and 2 are mutatable datums
     `(#;[1 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [0 (,#'#f
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [1 (,#'1
           ,#''a
           ,#'1
           ,#'(+ 1 2))]

       [2 (,#'#t
           ,#''a
           ,#'-1
           ,#'(+ 1 2))]
       [3 (,#'#t
           ,#''a
           ,#'1.0
           ,#'(+ 1 2))]
       [4 (,#'#t
           ,#''a
           ,#'0
           ,#'(+ 1 2))]
       [5 (,#'#t
           ,#''a
           ,#'1+0.0i
           ,#'(+ 1 2))]
       [6 (,#'#t
           ,#''a
           ,#'#f
           ,#'(+ 1 2))]
       ;; other elements aren't touched, regardless of mutation-index
       [7 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [8 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [9 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       #| ... |#))

    (test-mutation/in-seq
     (list #'#t
           #''a
           #'(test? 2))
     mutate-in-seq
     (curryr mutate-condition
             mutate-expr)
     (curry map syntax->datum)
     `(#;[0 (,#'#t
           ,#''a
           ,#'(test? 2))]
       [0 (,#'(not #t)
           ,#''a
           ,#'(test? 2))]
       [1 (,#'#t
           ,#'(not 'a)
           ,#'(test? 2))]
       [2 (,#'#t
           ,#''a
           ,#'(not (test? 2)))]
       ;; After negating conds, we descend into complex ones
       [3 (,#'#t
           ,#''a
           ,#'(test? -2))]
       #| ... |#))))



(define/contract (mutate-in-seq* stxs mutation-index counter
                                 mutator)
  ((listof (listof syntax?))
   mutation-index?
   counter?
   (syntax? mutation-index? counter? . -> . mutated?)
   . -> .
   mutated?)

  (mutate-in-seq stxs mutation-index counter
                 (curryr mutate-in-seq mutator)))

(module+ test
  (test-begin
    #:name mutate-in-seq*
    (test-mutation/in-seq
     (list (list #'#t #'#f)
           (list #''a #'1)
           (list #'(test? 2)))
     mutate-in-seq*
     mutate-datum
     (curry map
            (curry map syntax->datum))
     `(#;[0 ((,#'#t ,#'#f)
           (,#''a ,#'1)
           (,#'(test? 2)))]
       ;; first row
       [0 ((,#'#f ,#'#f)
           (,#''a ,#'1)
           (,#'(test? 2)))]
       [1 ((,#'1 ,#'#f)
           (,#''a ,#'1)
           (,#'(test? 2)))]
       [2 ((,#'#t ,#'#t)
           (,#''a ,#'1)
           (,#'(test? 2)))]
       [3 ((,#'#t ,#'0)
           (,#''a ,#'1)
           (,#'(test? 2)))]

       ;; second row
       [4 ((,#'#t ,#'#f)
           (,#''a ,#'-1)
           (,#'(test? 2)))]
       [5 ((,#'#t ,#'#f)
           (,#''a ,#'1.0)
           (,#'(test? 2)))]
       [6 ((,#'#t ,#'#f)
           (,#''a ,#'0)
           (,#'(test? 2)))]
       [7 ((,#'#t ,#'#f)
           (,#''a ,#'1+0.0i)
           (,#'(test? 2)))]
       [8 ((,#'#t ,#'#f)
           (,#''a ,#'#f)
           (,#'(test? 2)))]

       ;; third row
       [9 ((,#'#t ,#'#f)
           (,#''a ,#'1)
           ;; mutate-datum won't change applications...
           ;; [[but%20mutate-expr%20will!]]
           (,#'(test? 2)))]
       ;; mutation index exceeded size of list, no more will be mutated
       [10 ((,#'#t ,#'#f)
            (,#''a ,#'1)
            (,#'(test? 2)))]
       #| ... |#))

    (test-mutation/in-seq
     (list (list #'#t #'#f)
           (list #''a #'1)
           (list #'(test? 2)))
     mutate-in-seq*
     mutate-expr
     (curry map
            (curry map syntax->datum))
     ;; ... but mutate-expr will!
     `([9 ((,#'#t ,#'#f)
           (,#''a ,#'1)
           (,#'(test? -2)))]
       [10 ((,#'#t ,#'#f)
            (,#''a ,#'1)
            (,#'(test? 2.0)))]
       [11 ((,#'#t ,#'#f)
            (,#''a ,#'1)
            (,#'(test? 0)))]
       [12 ((,#'#t ,#'#f)
            (,#''a ,#'1)
            (,#'(test? 2+0.0i)))]
       [13 ((,#'#t ,#'#f)
            (,#''a ,#'1)
            (,#'(test? #f)))]))))

(define cond-clause? (syntax-parser
                       [[condition:expr b:expr r:expr ...]
                        #t]
                       [_ #f]))

(define/contract (mutate-cond-in-seq clauses mutation-index counter
                                     #:filter [can-mutate-expr? (const #t)])
  ({(listof cond-clause?)
    mutation-index?
    counter?}
   {#:filter (syntax? . -> . boolean?)}
   . ->* .
   mutated?)

  (define unwrapped-clauses (map syntax-e clauses))
  (define condition-stxs (map first unwrapped-clauses))
  (define/contract body-stxs* ;; each condition has a list of body-exprs
    (listof (listof syntax?))
    (map rest unwrapped-clauses))
  (mdo [count-with (__ counter)]
       (def mutated-conditions
         (mutate-in-seq condition-stxs
                        mutation-index
                        __
                        (curryr mutate-condition
                                (curry mutate-expr
                                       #:filter can-mutate-expr?))))
       (def mutated-body-stxs
         (mutate-in-seq* body-stxs*
                         mutation-index
                         __
                         (curry mutate-expr
                                #:filter can-mutate-expr?)))
       [return (map cons
                    mutated-conditions
                    mutated-body-stxs)]))

(module+ test
  (define-test (test-mutation/in-conds orig-seq
                                       expects)
    (for ([index (in-list (map first expects))]
          [expect (in-list (map second expects))])
      (define cond-lists
        (mutated-stx (mutate-cond-in-seq orig-seq
                                         index
                                         0)))
      (define cond-clauses
        (map (λ (cond-clause) #`[#,@cond-clause])
             cond-lists))
      (unless (andmap programs-equal?
                      cond-clauses
                      expect)
        (fail "Result does not match expected output.
Mutation index: ~v
Expected:
~a

Actual:
~a
"
              index
              (map syntax->datum expect)
              (map syntax->datum cond-clauses))))
    #t)

  (test-begin
    #:name mutate-in-cond-seq
    (test-mutation/in-conds
     (list #'[#t 1 2 3.0]
           #'[(bool? 5) (foo 0)]
           #'[else (bar (+ x 2) #f)])
     ;; template
     #;[ (list #'[#t 1 2 3.0]
               #'[(bool? 5) (foo 0)]
               #'[else (bar (+ x 2) #f)])]
     `([0 (,#'[(not #t) 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [1 (,#'[#t 1 2 3.0]
           ,#'[(not (bool? 5)) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [2 (,#'[#t 1 2 3.0]
           ,#'[(bool? -5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [3 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5.0) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [4 (,#'[#t 1 2 3.0]
           ,#'[(bool? 0) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [5 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5+0.0i) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [6 (,#'[#t 1 2 3.0]
           ,#'[(bool? #f) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; end conditions, enter bodies
       ;; row 1
       ;; 1,1
       [7 (,#'[#t -1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [8 (,#'[#t 1.0 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [9 (,#'[#t 0 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [10 (,#'[#t 1+0.0i 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [11 (,#'[#t #f 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; 1,2
       [12 (,#'[#t 1 -2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [13 (,#'[#t 1 2.0 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [14 (,#'[#t 1 0 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [15 (,#'[#t 1 2+0.0i 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [16 (,#'[#t 1 #f 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; 1,3
       [17 (,#'[#t 1 2 -3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [18 (,#'[#t 1 2 0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [19 (,#'[#t 1 2 3.0+0.0i]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       [20 (,#'[#t 1 2 #f]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; row 2
       [21 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0.0)]
           ,#'[else (bar (+ x 2) #f)])]
       [22 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 1)]
           ,#'[else (bar (+ x 2) #f)])]
       [23 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo #f)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; row 3
       [24 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar #f (+ x 2))])]
       [25 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ 2 x) #f)])]
       [26 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (- x 2) #f)])]
       [27 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x -2) #f)])]
       [28 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2.0) #f)])]
       [29 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 0) #f)])]
       [30 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2+0.0i) #f)])]
       [31 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x #f) #f)])]
       [32 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) #t)])]
       [33 (,#'[#t 1 2 3.0]
           ,#'[(bool? 5) (foo 0)]
           ,#'[else (bar (+ x 2) 0)])]
       ;; no more left
       [34 (,#'[#t 1 2 3.0]
            ,#'[(bool? 5) (foo 0)]
            ,#'[else (bar (+ x 2) #f)])]
       #| ... |#))))

(define (mutate-begin-seq orig-stx new-stx mutation-index counter
                          #:filter can-mutate-expr?)
  (log-mutation-type "statement-deletion")
  (mdo [count-with (__ counter)]
       (def begin-stx (maybe-mutate orig-stx
                                    new-stx
                                    mutation-index
                                    __))
       ;; applying both mutations works because of counter tracking!
       [in (mutate-expr* begin-stx
                         mutation-index
                         __
                         #:filter can-mutate-expr?)]))

;; end sequence helpers
#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Full program mutation tests

(module+ test
  (define-test (test-mutation index orig-prog new-prog
                              [mutate-syntax mutate-syntax/define/c])
    (with-handlers ([mutation-index-exception?
                     (λ _
                       (fail "Mutation index exceeded"))])
      (test-programs-equal?
       (mutate-syntax orig-prog index)
       new-prog)))
  (define-test (test-mutation/sequence orig-program expects
                                       [mutate-syntax mutate-syntax/define/c])
    (for/and/test ([expect (in-list expects)])
      (match-define (list mutation-index expected) expect)
      (define result (test-mutation mutation-index
                                    orig-program
                                    expected
                                    mutate-syntax))
      (extend-test-message result
                           " (mutation index: ~v)"
                           mutation-index)))

  (test-begin
    #:name full:constants
    (test-mutation
     0
     #'{(define/contract a any/c #t)}
     #'{(define/contract a any/c #f)})
    (test-mutation
     0
     #'{(define/contract a any/c #f)}
     #'{(define/contract a any/c #t)})

    (test-mutation
     0
     #'{(define/contract a positive? 1)}
     #'{(define/contract a positive? -1)})

    (test-mutation
     0
     #'{(define/contract a positive? -1)}
     #'{(define/contract a positive? 1)})
    (test-mutation
     0
     #'{(define/contract a positive? 5)}
     #'{(define/contract a positive? -5)})
    (test-mutation
     0
     #'{(define/contract a positive? 3)}
     #'{(define/contract a positive? -3)})
    (test-mutation
     0
     #'{(define/contract a positive? 3.5)}
     #'{(define/contract a positive? -3.5)})
    (test-exn
     mutation-index-exception?
     (mutate-syntax/define/c
      #'{(define/contract a positive? (λ (x) x))}
      0))

    (test-mutation/sequence
     #'{(define/contract a positive? 1)
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract a positive? -1)
              (define/contract b positive? 2)}]
       [5 ,#'{(define/contract a positive? 1)
              (define/contract b positive? -2)}]))

    (test-mutation
     0
     #'{(define/contract (f x)
          (-> positive? positive?)
          1)
        (define/contract b positive? 2)}
     #'{(define/contract (f x)
          (-> positive? positive?)
          -1)
        (define/contract b positive? 2)}))

  (test-begin
    #:name full:operators
    (test-mutation/sequence
     #'{(define/contract a any/c (+ 1 2))}
     `([0 ,#'{(define/contract a any/c (+ 2 1))}]
       [1 ,#'{(define/contract a any/c (- 1 2))}]
       [2 ,#'{(define/contract a any/c (+ -1 2))}]
       [7 ,#'{(define/contract a any/c (+ 1 -2))}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (+ x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (+ 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (- x 2))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (- x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (- 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (+ x 2))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (* x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (* 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (/ x 2))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (quotient x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (quotient 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (/ x 2))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (modulo x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (modulo 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (/ x 2))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (add1 x))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (sub1 x))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (car x))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (cdr x))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (and x #t))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (and #t x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (or #t x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (and x #t))
              (define/contract b positive? 2)}]))

    ;; Test choices of index
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? 2)}
     ;; Fixed this! ⇓
     `(#;[1 ,#'{(define/contract (f x)
                  any/c
                  (or x #t)) ;; tries to mutate `x` but it's a no-op
                (define/contract b positive? 2)}]
       [0 ,#'{(define/contract (f x)
                any/c
                (or #t x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (and x #t))
              (define/contract b positive? 2)}]
       [2 ,#'{(define/contract (f x)
                any/c
                (or x #f))
              (define/contract b positive? 2)}]
       [3 ,#'{(define/contract (f x)
                any/c
                (or x 1))
              (define/contract b positive? 2)}]
       [4 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? -2)}])))

  (test-begin
    #:name begin
    (test-mutation
     4
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 2))})
    (test-mutation
     4
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin0 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin0 1))}))

  (test-begin
    #:name if
    (test-mutation
     4
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract (g x)
          any/c
          (if x 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract (g x)
          any/c
          (if (not x) 1 2))}))

  (test-begin
    #:name function-application-args-swapping
    (test-mutation/sequence
     #'{(define/contract x any/c (f 1 2 3 4 5))}
     `([0 ,#'{(define/contract x any/c (f 2 1 3 4 5))}]
       [1 ,#'{(define/contract x any/c (f 1 2 4 3 5))}]
       [2 ,#'{(define/contract x any/c (f -1 2 3 4 5))}]
       #| ... |#)))


  (test-begin
    #:name complex-program
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 1 2))
        (define/contract (g x)
          any/c
          (if x 1 2))}
     `([4 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 2))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [5 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin -1 2))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [10 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 -2))
               (define/contract (g x)
                 any/c
                 (if x 1 2))}]
       [15 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 2))
               (define/contract (g x)
                 any/c
                 (if (not x) 1 2))}]
       [16 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 1 2))
              (define/contract (g x)
                any/c
                (if x -1 2))}]
       [21 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 2))
               (define/contract (g x)
                 any/c
                 (if x 1 -2))}])))

  (test-begin
    #:name out-of-mutations
    (test-exn
     mutation-index-exception?
     (mutate-program #'{(define/contract (f x)
                          any/c
                          (or x #t))
                        (define/contract b positive? (begin 1 2))
                        (define/contract (g x)
                          any/c
                          (if x 1 x))}
                     22
                     #:top-level-select select-define/contract)))


  (test-begin
    #:name classes
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class my-parent
            (define/public (f x) x)
            (define/private (g x) x)))}
     ;; superclass
     `([0 ,#'{(define/contract c
                any/c
                (class object%
                  (define/public (f x) x)
                  (define/private (g x) x)))}]
       ;; method visibility
       [1 ,#'{(define/contract c
                any/c
                (class my-parent
                  (define/private (f x) x)
                  (define/private (g x) x)))}]
       [2 ,#'{(define/contract c
                any/c
                (class my-parent
                  (define/public (f x) x)
                  (define/public (g x) x)))}]))
    ;; Initializer swapping
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class o (field [v (foo bar)]
                          w
                          [x 5]
                          [a (g 0)]
                          y
                          [b f]
                          [z #f])))}
     ;; Swap first pair of initializers
     `([1 ,#'{(define/contract c any/c (class o
                                         (field [v 5]
                                                [x (foo bar)]
                                                [a (g 0)]
                                                [b f]
                                                [z #f]
                                                w
                                                y)))}]
       ;; Swap second pair of initializers
       [2 ,#'{(define/contract c any/c (class o
                                         (field [v (foo bar)]
                                                [x 5]
                                                [a f]
                                                [b (g 0)]
                                                [z #f]
                                                w
                                                y)))}]
       ;; Descend into mutating initializer values
       ;; Note that final odd initializer is NOT swapped
       [3 ,#'{(define/contract c any/c (class o
                                         (field [v (foo bar)]
                                                [x -5]
                                                [a (g 0)]
                                                [b f]
                                                [z #f]
                                                w
                                                y)))}]
       [8 ,#'{(define/contract c any/c (class o
                                         (field [v (foo bar)]
                                                [x 5]
                                                [a (g 0.0)]
                                                [b f]
                                                [z #f]
                                                w
                                                y)))}]
       [11 ,#'{(define/contract c any/c (class o
                                          (field [v (foo bar)]
                                                 [x 5]
                                                 [a (g 0)]
                                                 [b f]
                                                 [z #t]
                                                 w
                                                 y)))}]))
    ;; same test with init-field
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class o (init-field [v (foo bar)]
                               w
                               [x 5]
                               [a (g 0)]
                               y
                               [b f]
                               [z #f])))}
     ;; Swap first pair of initializers
     `([1 ,#'{(define/contract c any/c (class o
                                         (init-field [v 5]
                                                     [x (foo bar)]
                                                     [a (g 0)]
                                                     [b f]
                                                     [z #f]
                                                     w
                                                     y)))}]))

    ;; mutation of method bodies
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class
            o
            (define/public (my-method x y)
              (- x y))))}
     `([0 ,#'{(define/contract c
                any/c
                (class
                  object% #| <- |#
                  (define/public (my-method x y)
                    (- x y))))}]
       [1 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/private #| <- |# (my-method x y)
                    (- x y))))}]
       [2 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/public (my-method y x #| <- |#)
                    (- x y))))}]
       [3 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/public (my-method x y)
                    (- y #| <-> |# x))))}]
       [4 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/public (my-method x y)
                    (+ #| <- |# x y))))}]))

    ;; super-new replacement
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class o
            (super-new)
            (define/public x 5)))}
     `([1 ,#'{(define/contract c
                any/c
                (class o
                  (void)
                  (define/public x 5)))}]
       [2 ,#'{(define/contract c
                any/c
                (class o
                  (super-new)
                  (define/private x 5)))}])))

  ;; Test that simple condition expressions are only ever considered for
  ;; mutation once: to negate them
  (test-begin
    #:name conditions-only-considered-for-negation
    (test-mutation/sequence
     #'{(displayln "B")
        (define/contract c any/c x)
        (define/contract d any/c (if #t 1 (error (quote wrong))))
        (displayln (quasiquote (c (unquote c))))
        (displayln (quasiquote (d (unquote d))))}
     `([0 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c (if (not #t) 1 (error (quote wrong))))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [1 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c (if #t -1 (error (quote wrong))))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]))

    (test-mutation/sequence
     #'{(displayln "B")
        (define/contract c any/c x)
        (define/contract d any/c
          (cond [#t 1]
                [(foobar (+ x y)) -1 5]
                [else (error (quote wrong))]))
        (displayln (quasiquote (c (unquote c))))
        (displayln (quasiquote (d (unquote d))))}
     ;; tries all conditions in sequence,
     `([0 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [(not #t) 1]
                      [(foobar (+ x y)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [1 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [#t 1]
                      [(not (foobar (+ x y))) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [2 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [#t 1]
                      [(foobar (+ y x)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [3 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [#t 1]
                      [(foobar (- x y)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       ;; then the bodies in sequence
       [4 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [#t -1]
                      [(foobar (+ x y)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [9 ,#'{(displayln "B")
              (define/contract c any/c x)
              (define/contract d any/c
                (cond [#t 1]
                      [(foobar (+ x y)) 1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [14 ,#'{(displayln "B")
               (define/contract c any/c x)
               (define/contract d any/c
                 (cond [#t 1]
                       [(foobar (+ x y)) -1 -5]
                       [else (error (quote wrong))]))
               (displayln (quasiquote (c (unquote c))))
               (displayln (quasiquote (d (unquote d))))}])))


  (test-begin
    #:name nested-exprs
    (test-mutation/sequence
     #'{(define/contract a any/c (+ (+ 1 2)
                                    (- 3 4)))}
     `(;; flip outer args
       [0 ,#'{(define/contract a any/c (+ (- 3 4)
                                          (+ 1 2)))}]
       ;; negate outer +
       [1 ,#'{(define/contract a any/c (- (+ 1 2)
                                          (- 3 4)))}]
       ;; flip inner args 1
       [2 ,#'{(define/contract a any/c (+ (+ 2 1)
                                          (- 3 4)))}]
       ;; negate inner +
       [3 ,#'{(define/contract a any/c (+ (- 1 2)
                                          (- 3 4)))}]
       ;; mutate inner + args
       [4 ,#'{(define/contract a any/c (+ (+ -1 2)
                                          (- 3 4)))}]
       ;; . . . . . . . . .
       ;; flip inner args 2
       [14 ,#'{(define/contract a any/c (+ (+ 1 2)
                                           (- 4 3)))}]
       ;; negate inner -
       [15 ,#'{(define/contract a any/c (+ (+ 1 2)
                                           (+ 3 4)))}]
       ;; mutate inner - args
       [16 ,#'{(define/contract a any/c (+ (+ 1 2)
                                           (- -3 4)))}]))
    (test-mutation
     2
     #'{(define (foo x) x)
        (define/contract a any/c (+ (foo 1) 2))}
     #'{(define (foo x) x)
        (define/contract a any/c (+ (foo -1) 2))}))

  (test-begin
    #:name higher-order
    ;; Test the mutator's ability to handle higher order expressions
    ;; 1. The function being applied is an expression
    ;; 2. Primitives that should be mutated (+, -) appear in expression ctx
    ;;    rather than application ctx
    (test-mutation/sequence
     #'{(define/contract a any/c ((if #t + -) 1 2))}
     `([0 ,#'{(define/contract a any/c ((if #t + -) 2 1))}]
       [1 ,#'{(define/contract a any/c ((if (not #t) + -) 1 2))}]
       [2 ,#'{(define/contract a any/c ((if #t - -) 1 2))}]
       [3 ,#'{(define/contract a any/c ((if #t + +) 1 2))}]
       [4 ,#'{(define/contract a any/c ((if #t + -) -1 2))}]
       [9 ,#'{(define/contract a any/c ((if #t + -) 1 -2))}])))

  (test-begin
    #:name mutated-id-reporting
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(define/contract (f x)
            any/c
            (<= x 2))
          (define/contract b positive? 2)}
       5))
     'f)
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(define/contract (f x)
            any/c
            (<= x 2))
          (define/contract b positive? 2)}
       6))
     'b)
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(displayln "B")
          (define/contract c any/c 1)
          (define/contract d any/c
            (cond [#t 1]
                  [(not (foobar (+ 1 2))) -1 5]
                  [else (error (quote wrong))]))
          (displayln (quasiquote (c (unquote c))))
          (displayln (quasiquote (d (unquote d))))}
       5))
     'd))

  (test-begin
    #:name examples-from-benchmarks
    ;; Tests on snippets from the actual benchmarks
    (test-mutation/sequence
     #'{(define/contract b positive? 2)
        (define/contract (singleton-list? x)
          (configurable-ctc)

          (and (list? x)
               (not (null? x))
               (null? (cdr x))))}
     `([5 ,#'{(define/contract b positive? 2)
              (define/contract (singleton-list? x)
                (configurable-ctc)

                (and (not (null? x))
                     (list? x)
                     (null? (cdr x))))}]
       [6 ,#'{(define/contract b positive? 2)
              (define/contract (singleton-list? x)
                (configurable-ctc)

                (or (list? x)
                    (not (null? x))
                    (null? (cdr x))))}]))

    (test-mutation/sequence
     #'{(provide
         command%
         CMD*
         )

        (require
         racket/match
         racket/class
         (only-in racket/string string-join string-split)
         (for-syntax racket/base racket/syntax syntax/parse)
         racket/contract
         "../../../ctcs/precision-config.rkt"
         (only-in racket/function curry)
         (only-in racket/list empty? first second rest)
         (only-in "../../../ctcs/common.rkt"
                  class/c*
                  or-#f/c
                  command%/c
                  command%?
                  command%?-with-exec
                  stack?
                  env?
                  list-with-min-size/c
                  equal?/c)
         )
        (require (only-in "stack.rkt"
                          stack-drop
                          stack-dup
                          stack-init
                          stack-over
                          stack-pop
                          stack-push
                          stack-swap
                          ))

        (define (assert v p)
          (unless (p v) (error 'assert))
          v)


        (define/contract command%
          command%/c
          (class object%
            (super-new)
            (init-field
             id
             descr
             exec)))

        (define ((env-with/c cmd-ids) env)
          (cond [(env? env)
                 (define env-cmd-ids
                   (for/list ([env-cmd (in-list env)])
                     (get-field id env-cmd)))
                 (for/and ([c (in-list cmd-ids)])
                   (member c env-cmd-ids))]
                [else #f]))



        ;; True if the argument is a list with one element
        (define/contract (singleton-list? x)
          (configurable-ctc
           [max (->i ([x list?])
                     [result (x) (if (empty? x)
                                     #f
                                     (empty? (rest x)))])]
           [types (list? . -> . boolean?)])

          (and (list? x)
               (not (null? x))
               (null? (cdr x))))}
     `([0 ,#'{(provide
               command%
               CMD*
               )

              (require
               racket/match
               racket/class
               (only-in racket/string string-join string-split)
               (for-syntax racket/base racket/syntax syntax/parse)
               racket/contract
               "../../../ctcs/precision-config.rkt"
               (only-in racket/function curry)
               (only-in racket/list empty? first second rest)
               (only-in "../../../ctcs/common.rkt"
                        class/c*
                        or-#f/c
                        command%/c
                        command%?
                        command%?-with-exec
                        stack?
                        env?
                        list-with-min-size/c
                        equal?/c)
               )
              (require (only-in "stack.rkt"
                                stack-drop
                                stack-dup
                                stack-init
                                stack-over
                                stack-pop
                                stack-push
                                stack-swap
                                ))

              (define (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define/contract command%
                command%/c
                (class object%
                  (void)
                  (init-field
                   id
                   descr
                   exec)))

              (define ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define/contract (singleton-list? x)
                (configurable-ctc
                 [max (->i ([x list?])
                           [result (x) (if (empty? x)
                                           #f
                                           (empty? (rest x)))])]
                 [types (list? . -> . boolean?)])

                (and (list? x)
                     (not (null? x))
                     (null? (cdr x))))}]
       [1 ,#'{(provide
               command%
               CMD*
               )

              (require
               racket/match
               racket/class
               (only-in racket/string string-join string-split)
               (for-syntax racket/base racket/syntax syntax/parse)
               racket/contract
               "../../../ctcs/precision-config.rkt"
               (only-in racket/function curry)
               (only-in racket/list empty? first second rest)
               (only-in "../../../ctcs/common.rkt"
                        class/c*
                        or-#f/c
                        command%/c
                        command%?
                        command%?-with-exec
                        stack?
                        env?
                        list-with-min-size/c
                        equal?/c)
               )
              (require (only-in "stack.rkt"
                                stack-drop
                                stack-dup
                                stack-init
                                stack-over
                                stack-pop
                                stack-push
                                stack-swap
                                ))

              (define (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define/contract command%
                command%/c
                (class object%
                  (super-new)
                  (init-field
                   id
                   descr
                   exec)))

              (define ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define/contract (singleton-list? x)
                (configurable-ctc
                 [max (->i ([x list?])
                           [result (x) (if (empty? x)
                                           #f
                                           (empty? (rest x)))])]
                 [types (list? . -> . boolean?)])

                (and (not (null? x))
                     (list? x)
                     (null? (cdr x))))}]
       [2 ,#'{(provide
               command%
               CMD*
               )

              (require
               racket/match
               racket/class
               (only-in racket/string string-join string-split)
               (for-syntax racket/base racket/syntax syntax/parse)
               racket/contract
               "../../../ctcs/precision-config.rkt"
               (only-in racket/function curry)
               (only-in racket/list empty? first second rest)
               (only-in "../../../ctcs/common.rkt"
                        class/c*
                        or-#f/c
                        command%/c
                        command%?
                        command%?-with-exec
                        stack?
                        env?
                        list-with-min-size/c
                        equal?/c)
               )
              (require (only-in "stack.rkt"
                                stack-drop
                                stack-dup
                                stack-init
                                stack-over
                                stack-pop
                                stack-push
                                stack-swap
                                ))

              (define (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define/contract command%
                command%/c
                (class object%
                  (super-new)
                  (init-field
                   id
                   descr
                   exec)))

              (define ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define/contract (singleton-list? x)
                (configurable-ctc
                 [max (->i ([x list?])
                           [result (x) (if (empty? x)
                                           #f
                                           (empty? (rest x)))])]
                 [types (list? . -> . boolean?)])

                (or (list? x)
                    (not (null? x))
                    (null? (cdr x))))}]))

    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(provide
           command%
           CMD*
           )

          (require
           racket/match
           racket/class
           (only-in racket/string string-join string-split)
           (for-syntax racket/base racket/syntax syntax/parse)
           racket/contract
           "../../../ctcs/precision-config.rkt"
           (only-in racket/function curry)
           (only-in racket/list empty? first second rest)
           (only-in "../../../ctcs/common.rkt"
                    class/c*
                    or-#f/c
                    command%/c
                    command%?
                    command%?-with-exec
                    stack?
                    env?
                    list-with-min-size/c
                    equal?/c)
           )
          (require (only-in "stack.rkt"
                            stack-drop
                            stack-dup
                            stack-init
                            stack-over
                            stack-pop
                            stack-push
                            stack-swap
                            ))

          (define (assert v p)
            (unless (p v) (error 'assert))
            v)


          (define/contract command%
            command%/c
            (class object%
              (super-new)
              (init-field
               id
               descr
               exec)))

          (define ((env-with/c cmd-ids) env)
            (cond [(env? env)
                   (define env-cmd-ids
                     (for/list ([env-cmd (in-list env)])
                       (get-field id env-cmd)))
                   (for/and ([c (in-list cmd-ids)])
                     (member c env-cmd-ids))]
                  [else #f]))



          ;; True if the argument is a list with one element
          (define/contract (singleton-list? x)
            (configurable-ctc
             [max (->i ([x list?])
                       [result (x) (if (empty? x)
                                       #f
                                       (empty? (rest x)))])]
             [types (list? . -> . boolean?)])

            (and (list? x)
                 (not (null? x))
                 (null? (cdr x))))}
       1))
     'singleton-list?))

  (define-test (test-selector selector
                              stx
                              #:parts [test-parts (λ _ #t)]
                              #:name [test-name (λ _ #t)]
                              #:reconstructor [test-reconstructor (λ _ #t)])
    (define selector/ctc (contract top-level-selector/c selector
                                'the-selector 'the-test))
    (call-with-values
     (thunk (selector/ctc stx))
     (λ (parts name reconstructor)
       (and/test (test-parts parts)
                 (test-name name)
                 (test-reconstructor reconstructor)))))
  (define (make-name-test expected)
    (λ (name) (test-equal? name expected)))
  (define (make-parts-test expected)
    (λ (parts)
      (for/and/test ([part (in-list parts)]
                     [expect (in-list expected)])
                    (test-programs-equal? part expect))))
  (define (make-reconstructor-test input expected-output)
    (λ (reconstructor)
      (test-programs-equal? (reconstructor input)
                            expected-output)))
  (test-begin
    #:name selectors
    (test-selector
     select-any-define
     #'(define (f x) (+ y y))
     #:name (make-name-test 'f)
     #:parts (make-parts-test (list #'(+ y y)))
     #:reconstructor (make-reconstructor-test (list #'foo)
                                              #'(define (f x) foo)))
    (test-selector
     select-any-define
     #'(defoobar (f x) (+ y y))
     #:name false?
     #:parts false?
     #:reconstructor false?)
    (test-selector
     select-all
     #'42
     #:name (make-name-test '<no-name-found>)
     #:parts (make-parts-test (list #'42))
     #:reconstructor (make-reconstructor-test (list #'42)
                                              #'42))
    (test-selector
     select-define/contract
     #'(define/contract (f x) ctc (+ y y))
     #:name (make-name-test 'f)
     #:parts (make-parts-test (list #'(+ y y)))
     #:reconstructor (make-reconstructor-test (list #'foo)
                                              #'(define/contract (f x) ctc foo)))
    (test-selector
     select-define/contract
     #'(define (f x) (+ y y))
     #:name false?
     #:parts false?
     #:reconstructor false?))

  (test-begin
    #:name top-level-selectors
    (test-mutation/sequence
     #'{(foobar)
        (define (f x)
          (+ y y))}
     `([0 ,#'{(foobar)
              (define (f x)
                (- y y))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:top-level-select select-any-define)))
    (test-exn mutation-index-exception?
              (mutate-program #'{(foobar)
                                 (define (f x)
                                   (+ y y))}
                              1)))

  (test-begin
    #:name expr-filtering
    (test-mutation/sequence
     #'{(: f (-> Number Number))
        (define (f x)
          (: y Number)
          (define y (+ x x))
          (+ y y))}
     `([0 ,#'{(: f (-> Number Number))
              (define (f x)
                (: y Number)
                (define y (- x x))
                (+ y y))}]
       [1 ,#'{(: f (-> Number Number))
              (define (f x)
                (: y Number)
                (define y (+ x x))
                (- y y))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:expression-filter (λ (e)
                                            (syntax-parse e
                                              [({~datum :} . _) #f]
                                              [else #t]))
                      #:top-level-select select-any-define))))

  (test-begin
    #:name data-structure-swaps
    (test-mutation
     0
     #'{(define/contract x any/c (make-hash '()))}
     #'{(define/contract x any/c (make-immutable-hash '()))})
    (test-mutation
     0
     #'{(define/contract x any/c (make-immutable-hash '()))}
     #'{(define/contract x any/c (make-hash '()))})
    (test-mutation
     0
     #'{(define/contract x any/c (vector 1))}
     #'{(define/contract x any/c (vector-immutable 1))})
    (test-mutation
     0
     #'{(define/contract x any/c (vector-immutable 1))}
     #'{(define/contract x any/c (vector 1))})))

;; Potential mutations that have been deferred:
;; - (hash a b ...) ~> (make-hash (list (cons a b) ...))
;;   Why? That form is not used in the benchmarks: only `zordoz` uses it, and
;;   only on a single line at that.
