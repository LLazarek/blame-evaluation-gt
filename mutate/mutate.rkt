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
    syntax?)])
 mutation-index-exception?
 (struct-out mutated-program))

(require (for-syntax syntax/parse)
         syntax/parse
         racket/match
         "mutated.rkt")

#|----------------------------------------------------------------------|#
;; Data; see mutated.rkt
(define make-mutants #t)
(struct mutation-index-exception ())

#|----------------------------------------------------------------------|#

#|----------------------------------------------------------------------|#
;; Test setup
(define (exprs-equal? a b)
  (equal? (syntax->datum a)
          (syntax->datum b)))
(module+ test
  (require ruinit
           ruinit/diff/diff)

  (define programs-equal? exprs-equal?)

  (define (diff-programs/string actual expected)
    (dumb-diff-lines/string (pretty-format (syntax->datum actual))
                            (pretty-format (syntax->datum expected))))

  (define-simple-test (test-programs-equal? actual expected)
    #:fail-message @~a{
                       Programs are not equal. Diff (expected <):
                       @(diff-programs/string expected actual)
                       }
    (programs-equal? actual expected)))

#|----------------------------------------------------------------------|#
;; Utilities

;; Base mutator: all mutation happens through this function
;;
;; Manages the decision of whether or not to apply a mutation based on
;; `mutation-index` and `counter`, recording the consideration of a
;; valid mutation (in terms of the counter).
;;
;; If `old-stx` is syntactically identical to `new-stx`, the mutation
;; will not be considered.
(define/contract (maybe-mutate old-stx new-stx mutation-index counter)
  (->i ([old-stx syntax?]
        [new-stx syntax?]
        [mutation-index mutation-index?]
        [counter (mutation-index)
                 (and/c counter?
                        (<=/c mutation-index))])
       [result mutated?])
  (mutated
   (if (= mutation-index counter)
       new-stx
       old-stx)
   (if (exprs-equal? old-stx new-stx)
       counter
       ;; This was a mutation that could be applied, so increment
       ;; counter, indicating that a mutatable expr has been
       ;; considered.
       (add1 counter))))

(module+ test
  (test-begin
    #:name maybe-mutate
    ;; Valid mutation, but counter is not yet high enough
    (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 0))
                 (mmap syntax->datum (mutated #'a 1)))
    ;; Valid mutation, counter is right
    (test-equal? (mmap syntax->datum (maybe-mutate #'a #'b 5 5))
                 (mmap syntax->datum (mutated #'b 6)))
    ;; Valid mutation but it is syntactically identical
    ;; This can happen when swapping argument positions
    ;; e.g. for (foo '() '())
    (test-equal? (mmap syntax->datum (maybe-mutate #'('() '()) #'('() '())
                                                   5
                                                   5))
                 (mmap syntax->datum (mutated #'('() '()) 5)))))

(define-syntax (define-mutators stx)
  (syntax-parse stx
    #:datum-literals (-> <->)
    [(_ (mutator-name stx-name mutation-index counter)
        (~or (orig -> new)
             (left <-> right)) ...
        (match-datum datum-name:id
                     [pat res] ...)
        other-mutations ...)
     #'(define (mutator-name stx-name mutation-index counter)
         (define (maybe-mutate* new-stx)
           (maybe-mutate stx-name
                         new-stx
                         mutation-index
                         counter))
         (if (and make-mutants
                  (<= counter mutation-index))
             (syntax-parse stx-name
               #:datum-literals (orig ... left ... right ...)
               [orig
                (maybe-mutate* (syntax/loc stx-name new))]
               ...
               [left
                (maybe-mutate* (syntax/loc stx-name right))]
               ...
               [right
                (maybe-mutate* (syntax/loc stx-name left))]
               ...
               [(~and datum-name
                      (~not (fn arg (... ...))))
                (match (syntax->datum #'datum-name)
                  [pat (maybe-mutate* (syntax/loc stx-name res))]
                  ...
                  [_ (mutated stx-name
                              counter)])]
               other-mutations
               ...
               [other
                (mutated stx-name
                         counter)])
             (mutated stx-name
                      counter)))]))

(define =/= (compose not =))


#|----------------------------------------------------------------------|#
;; Mutators: perform simple syntactic mutations

;; mutate-datum: stx nat nat -> mutated
(define-mutators (mutate-datum stx mutation-index counter)
  ;; operator mutation
  (< <-> <=)
  (> <-> >=)
  (= <-> =/=)
  (+ <-> -)
  (* <-> /)
  (and <-> or)
  (modulo -> *)
  (define/public -> define/private)
  ;; (define/override <-> define/augment)

  ;; constant mutation
  (match-datum value
               [(? boolean?)  (not value)]
               [1             0]
               [-1            1]
               [5             -1]
               [(? integer?)  (add1 value)]
               [(? number?)   (- -1 value)]))

(module+ test
  (define-test-syntax (test-datum-mutations (~or (orig -> mutated)
                                                 (left <-> right)) ...)
    #'(and/test/message
       [(test-programs-equal? (mutated-stx (mutate-datum #'orig 0 0))
                              #'mutated)
        @~a{@'orig -> @'mutated failed}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'left 0 0))
                              #'right)
        @~a{@'left -> @'right failed}] ...
       [(test-programs-equal? (mutated-stx (mutate-datum #'right 0 0))
                              #'left)
        @~a{@'right -> @'left failed}] ...))
  (test-begin
    #:name mutate-datum
    (test-datum-mutations
     (< <-> <=)
     (> <-> >=)
     (= <-> =/=)
     (+ <-> -)
     (* <-> /)
     (and <-> or)
     (modulo -> *)
     (define/public -> define/private)
     ;; (define/override <-> define/augment)

     (#t -> (not #t))
     (#f -> (not #f))
     (1 -> 0)
     (-1 -> 1)
     (5 -> -1)
     (7 -> (add1 7))
     (2 -> (add1 2))
     (2.2 -> (- -1 2.2))
     (5.7 -> (- -1 5.7))

     (variable -> variable)
     ('symbol -> 'symbol)
     ('a -> 'a))))


;; negates
(define/contract (mutate-condition c mutation-index counter)
  (syntax? mutation-index? counter? . -> . mutated?)

  ;; design decision: only try negating conditions
  (if (or (equal? (syntax->datum c) 'else)
          (> counter mutation-index))
      (mutated c counter)
      (maybe-mutate c
                    (quasisyntax/loc c
                      (not #,c))
                    mutation-index
                    counter)))

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
        ;; Swap init-field initializers
        [((~or (~and (~datum init-field)
                     field-type)
               (~and (~datum field)
                     field-type))
          (~or [field-id:id initial-value:expr]
               no-init-field:id) ...)
         (define init-value-stxs (syntax-e (syntax/loc stx
                                             (initial-value ...))))
         (define field-ids (syntax-e (syntax/loc stx
                                       (field-id ...))))
         (mdo [count-with (__ counter)]
              (def init-values/rearranged
                (rearrange-in-seq init-value-stxs
                                  mutation-index
                                  __))
              (def init-values/mutated (mutate-in-seq init-values/rearranged
                                                      mutation-index
                                                      __
                                                      (curry mutate-expr
                                                             #:filter can-mutate?)))
              [return (quasisyntax/loc stx
                        (field-type #,@(map list
                                            field-ids
                                            init-values/mutated)
                                    no-init-field ...))])]

        ;; mutate function applications:
        ;; Move around arguments, or mutate argument expressions
        [(f:non-keyword arg ...)
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

        ;; mutate arbitrary sexp
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
(define (select-define stx)
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
     `([0 (,#'(not #t)
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [1 (,#'#t
           ,#''a
           ,#'0
           ,#'(+ 1 2))]
       ;; other elements aren't touched, regardless of mutation-index
       [2 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [3 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       [4 (,#'#t
           ,#''a
           ,#'1
           ,#'(+ 1 2))]
       #| ... |#))

    (test-mutation/in-seq
     (list #'#t
           #''a
           #'(test? 2))
     mutate-in-seq
     mutate-condition
     (curry map syntax->datum)
     `([0 (,#'(not #t)
           ,#''a
           ,#'(test? 2))]
       [1 (,#'#t
           ,#'(not 'a)
           ,#'(test? 2))]
       [2 (,#'#t
           ,#''a
           ,#'(not (test? 2)))]
       ;; mutation index exceeded size of list, no more will be mutated
       [3 (,#'#t
           ,#''a
           ,#'(test? 2))]
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
     ;; template
     #;[ (list (list #'#t #'#f)
               (list #''a #'1)
               (list #'(test? 2)))]
     `([0 ((,#'(not #t) ,#'#f)
           (,#''a ,#'1)
           (,#'(test? 2)))]
       [1 ((,#'#t ,#'(not #f))
           (,#''a ,#'1)
           (,#'(test? 2)))]
       [2 ((,#'#t ,#'#f)
           (,#''a ,#'0)
           (,#'(test? 2)))]
       [3 ((,#'#t ,#'#f)
           (,#''a ,#'1)
           ;; mutate-datum won't change applications...
           ;; [[but%20mutate-expr%20will!]]
           (,#'(test? 2)))]
       ;; mutation index exceeded size of list, no more will be mutated
       [4 ((,#'#t ,#'#f)
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
     `([3 ((,#'#t ,#'#f)
           (,#''a ,#'1)
           ;; ... but mutate-expr will!
           (,#'(test? (add1 2))))]))))

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
       (def conditions (mutate-in-seq condition-stxs
                                      mutation-index
                                      __
                                      mutate-condition))
       (def bodies* (mutate-in-seq* body-stxs*
                                    mutation-index
                                    __
                                    (curry mutate-expr
                                           #:filter can-mutate-expr?)))
       [return (map cons conditions bodies*)]))

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
     (list #'[#t 1 2 3]
           #'[(bool? 5) (foo 7)]
           #'[else (bar (+ x 2) #f)])
     ;; template
     #;[ (list #'[#t 1 2 3]
               #'[(bool? 5) (foo 7)]
               #'[else (bar (+ x 2) #f)])]
     `([0 (,#'[(not #t) 1 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ x 2) #f)])]
       [1 (,#'[#t 1 2 3]
           ,#'[(not (bool? 5)) (foo 7)]
           ,#'[else (bar (+ x 2) #f)])]
       ;; end conditions, enter bodies
       [2 (,#'[#t 0 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ x 2) #f)])]
       [3 (,#'[#t 1 (add1 2) 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ x 2) #f)])]
       [4 (,#'[#t 1 2 (add1 3)]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ x 2) #f)])]
       [5 (,#'[#t 1 2 3]
           ,#'[(bool? 5) (foo (add1 7))]
           ,#'[else (bar (+ x 2) #f)])]
       [6 (,#'[#t 1 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar #f (+ x 2))])]
       [7 (,#'[#t 1 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ 2 x) #f)])]
       [8 (,#'[#t 1 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (- x 2) #f)])]
       [9 (,#'[#t 1 2 3]
           ,#'[(bool? 5) (foo 7)]
           ,#'[else (bar (+ x (add1 2)) #f)])]
       [10 (,#'[#t 1 2 3]
            ,#'[(bool? 5) (foo 7)]
            ,#'[else (bar (+ x 2) (not #f))])]
       ;; no more left
       [11 (,#'[#t 1 2 3]
            ,#'[(bool? 5) (foo 7)]
            ,#'[else (bar (+ x 2) #f)])]
       #| ... |#))))

(define (mutate-begin-seq orig-stx new-stx mutation-index counter
                          #:filter can-mutate-expr?)
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
     #'{(define/contract a any/c (not #t))})
    (test-mutation
     0
     #'{(define/contract a any/c #f)}
     #'{(define/contract a any/c (not #f))})

    (test-mutation
     0
     #'{(define/contract a positive? 1)}
     #'{(define/contract a positive? 0)})

    (test-mutation
     0
     #'{(define/contract a positive? -1)}
     #'{(define/contract a positive? 1)})
    (test-mutation
     0
     #'{(define/contract a positive? 5)}
     #'{(define/contract a positive? -1)})
    (test-mutation
     0
     #'{(define/contract a positive? 3)}
     #'{(define/contract a positive? (add1 3))})
    (test-mutation
     0
     #'{(define/contract a positive? 3.5)}
     #'{(define/contract a positive? (- -1 3.5))})
    (test-exn
     mutation-index-exception?
     (mutate-syntax/define/c
      #'{(define/contract a positive? (λ (x) x))}
      0))

    (test-mutation/sequence
     #'{(define/contract a positive? 1)
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract a positive? 0)
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract a positive? 1)
              (define/contract b positive? (add1 2))}]))

    (test-mutation
     0
     #'{(define/contract (f x)
          (-> positive? positive?)
          1)
        (define/contract b positive? 2)}
     #'{(define/contract (f x)
          (-> positive? positive?)
          0)
        (define/contract b positive? 2)}))

  (test-begin
    #:name full:operators
    (test-mutation/sequence
     #'{(define/contract a any/c (+ 1 2))}
     `([0 ,#'{(define/contract a any/c (+ 2 1))}]
       [1 ,#'{(define/contract a any/c (- 1 2))}]
       [2 ,#'{(define/contract a any/c (+ 0 2))}]
       [3 ,#'{(define/contract a any/c (+ 1 (add1 2)))}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (< x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (< 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (<= x 2))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (<= x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (<= 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (< x 2))
              (define/contract b positive? 2)}]))
    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (> x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (> 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (>= x 2))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (>= x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (>= 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (> x 2))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (= x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (= 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (=/= x 2))
              (define/contract b positive? 2)}]))

    (test-mutation/sequence
     #'{(define/contract (f x)
          any/c
          (=/= x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (=/= 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (= x 2))
              (define/contract b positive? 2)}]))

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
          (/ x 2))
        (define/contract b positive? 2)}
     `([0 ,#'{(define/contract (f x)
                any/c
                (/ 2 x))
              (define/contract b positive? 2)}]
       [1 ,#'{(define/contract (f x)
                any/c
                (* x 2))
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
                (or x (not #t)))
              (define/contract b positive? 2)}]
       [3 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (add1 2))}])))

  (test-begin
    #:name begin
    (test-mutation
     3
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 2))})
    (test-mutation
     3
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
     6
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 1 2))
        (define/contract (g x)
          any/c
          (if x 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin 1 2))
        (define/contract (g x)
          any/c
          (if (not x) 1 2))}))

  (test-begin
    #:name function-application-args-swapping
    (test-mutation/sequence
     #'{(define/contract x any/c (f 1 2 3 4 5))}
     `([0 ,#'{(define/contract x any/c (f 2 1 3 4 5))}]
       [1 ,#'{(define/contract x any/c (f 1 2 4 3 5))}]
       [2 ,#'{(define/contract x any/c (f 0 2 3 4 5))}]
       [3 ,#'{(define/contract x any/c (f 1 (add1 2) 3 4 5))}]
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
     `([3 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 2))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [4 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 0 2))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [5 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 1 (add1 2)))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [6 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 1 2))
              (define/contract (g x)
                any/c
                (if (not x) 1 2))}]
       [7 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 1 2))
              (define/contract (g x)
                any/c
                (if x 0 2))}]
       [8 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 1 2))
              (define/contract (g x)
                any/c
                (if x 1 (add1 2)))}])))

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
                          (if x 1 2))}
                     10
                     #:top-level-select select-define/contract)))


  (test-begin
    #:name classes
    ;; method visibility
    (test-mutation
     0
     #'{(define/contract c
          any/c
          (class (define/public (f x) x)))}
     #'{(define/contract c
          any/c
          (class (define/private (f x) x)))})
    ;; Initializer swapping
    (test-mutation/sequence
     #'{(define/contract c any/c (class (field [v (foo bar)]
                                               w
                                               [x 5]
                                               [a (g 0)]
                                               y
                                               [b f]
                                               [z #f])))}
     ;; Swap first pair of initializers
     `([0 ,#'{(define/contract c any/c (class (field [v 5]
                                                     [x (foo bar)]
                                                     [a (g 0)]
                                                     [b f]
                                                     [z #f]
                                                     w
                                                     y)))}]
       ;; Swap second pair of initializers
       [1 ,#'{(define/contract c any/c (class (field [v (foo bar)]
                                                     [x 5]
                                                     [a f]
                                                     [b (g 0)]
                                                     [z #f]
                                                     w
                                                     y)))}]
       ;; Descend into mutating initializer values
       ;; Note that final odd initializer is NOT swapped
       [2 ,#'{(define/contract c any/c (class (field [v (foo bar)]
                                                     [x -1]
                                                     [a (g 0)]
                                                     [b f]
                                                     [z #f]
                                                     w
                                                     y)))}]
       [3 ,#'{(define/contract c any/c (class (field [v (foo bar)]
                                                     [x 5]
                                                     [a (g (add1 0))]
                                                     [b f]
                                                     [z #f]
                                                     w
                                                     y)))}]
       [4 ,#'{(define/contract c any/c (class (field [v (foo bar)]
                                                     [x 5]
                                                     [a (g 0)]
                                                     [b f]
                                                     [z (not #f)]
                                                     w
                                                     y)))}]))
    ;; same test with init-field
    (test-mutation/sequence
     #'{(define/contract c any/c (class (init-field [v (foo bar)]
                                                    w
                                                    [x 5]
                                                    [a (g 0)]
                                                    y
                                                    [b f]
                                                    [z #f])))}
     ;; Swap first pair of initializers
     `([0 ,#'{(define/contract c any/c (class (init-field [v 5]
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
            (define/public (my-method x y)
              (- x y))))}
     `([0 ,#'{(define/contract c
                any/c
                (class
                  (define/private #| <- |# (my-method x y)
                    (- x y))))}]
       [1 ,#'{(define/contract c
                any/c
                (class
                  (define/public (my-method y x #| <- |#)
                    (- x y))))}]
       [2 ,#'{(define/contract c
                any/c
                (class
                  (define/public (my-method x y)
                    (- y #| <-> |# x))))}]
       [3 ,#'{(define/contract c
                any/c
                (class
                  (define/public (my-method x y)
                    (+ #| <- |# x y))))}])))

  ;; Test that condition expressions are only ever considered for
  ;; mutation once: to negate them
  (test-begin
    #:name conditions-only-considered-for-negation
    (test-mutation/sequence
     #'{(displayln "B")
        (define/contract c any/c 1)
        (define/contract d any/c (if #t 1 (error (quote wrong))))
        (displayln (quasiquote (c (unquote c))))
        (displayln (quasiquote (d (unquote d))))}
     `([1 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c (if (not #t) 1 (error (quote wrong))))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [2 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c (if #t 0 (error (quote wrong))))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]))
    (test-mutation/sequence
     #'{(displayln "B")
        (define/contract c any/c 1)
        (define/contract d any/c
          (cond [#t 1]
                [(foobar (+ 1 2)) -1 5]
                [else (error (quote wrong))]))
        (displayln (quasiquote (c (unquote c))))
        (displayln (quasiquote (d (unquote d))))}
     ;; tries all conditions in sequence,
     `([1 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c
                (cond [(not #t) 1]
                      [(foobar (+ 1 2)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [2 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c
                (cond [#t 1]
                      [(not (foobar (+ 1 2))) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       ;; then the bodies in sequence
       [3 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c
                (cond [#t 0]
                      [(foobar (+ 1 2)) -1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [4 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c
                (cond [#t 1]
                      [(foobar (+ 1 2)) 1 5]
                      [else (error (quote wrong))]))
              (displayln (quasiquote (c (unquote c))))
              (displayln (quasiquote (d (unquote d))))}]
       [5 ,#'{(displayln "B")
              (define/contract c any/c 1)
              (define/contract d any/c
                (cond [#t 1]
                      [(foobar (+ 1 2)) -1 -1]
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
       [4 ,#'{(define/contract a any/c (+ (+ 0 2)
                                          (- 3 4)))}]
       [5 ,#'{(define/contract a any/c (+ (+ 1 (add1 2))
                                          (- 3 4)))}]
       ;; flip inner args 2
       [6 ,#'{(define/contract a any/c (+ (+ 1 2)
                                          (- 4 3)))}]
       ;; negate inner -
       [7 ,#'{(define/contract a any/c (+ (+ 1 2)
                                          (+ 3 4)))}]
       ;; mutate inner - args
       [8 ,#'{(define/contract a any/c (+ (+ 1 2)
                                          (- (add1 3) 4)))}]
       [9 ,#'{(define/contract a any/c (+ (+ 1 2)
                                          (- 3 (add1 4))))}]))
    (test-mutation
     2
     #'{(define (foo x) x)
        (define/contract a any/c (+ (foo 1) 2))}
     #'{(define (foo x) x)
        (define/contract a any/c (+ (foo 0) 2))}))

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
       [4 ,#'{(define/contract a any/c ((if #t + -) 0 2))}]
       [5 ,#'{(define/contract a any/c ((if #t + -) 1 (add1 2)))}])))

  (test-begin
    #:name mutated-id-reporting
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(define/contract (f x)
            any/c
            (<= x 2))
          (define/contract b positive? 2)}
       2))
     'f)
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/define/c
       #'{(define/contract (f x)
            any/c
            (<= x 2))
          (define/contract b positive? 2)}
       3))
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
       2))
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
     `([1 ,#'{(define/contract b positive? 2)
              (define/contract (singleton-list? x)
                (configurable-ctc)

                (and (not (null? x))
                     (list? x)
                     (null? (cdr x))))}]
       [2 ,#'{(define/contract b positive? 2)
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
     select-define
     #'(define (f x) (+ y y))
     #:name (make-name-test 'f)
     #:parts (make-parts-test (list #'(+ y y)))
     #:reconstructor (make-reconstructor-test (list #'foo)
                                              #'(define (f x) foo)))
    (test-selector
     select-define
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
                      #:top-level-select select-define)))
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
                      #:top-level-select select-define)))))

;; Potential mutations that have been deferred:
;; - Moving occurrences of (super-new) around in class body
