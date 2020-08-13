#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names)

(require racket/function
         syntax/parse
         "../../mutate/mutate-expr.rkt"
         "../../mutate/mutate-program.rkt"
         "../../mutate/mutator-lib.rkt"
         "../../mutate/mutators.rkt"
         "../../mutate/top-level-selectors.rkt"
         "../../mutate/expression-selectors.rkt")

;; lltodo: this should really be the list of mutators themselves, and there
;; should be a way to get a mutator's name from the mutator.
;; Probably I want a struct with prop:procedure, and a field for the name.
;; I can have a `define-mutator` macro that takes the name and the mutator
;; function, and introduces a binding for the name.
;; For now, keep them manually in-sync.
(define active-mutator-names
  (list "arithmetic-op-swap"
        "boolean-op-swap"
        "class:publicity"
        "class:super-new"
        "nested-list-construction-swap"
        "constant-swap"
        "begin-result-deletion"
        "negate-conditional"
        "force-conditional"
        "wrap-conditional"
        "class:parent-swap"
        "class:initializer-swap"
        "position-swap"
        "class:add-extra-method"
        "top-level-id-swap"
        "imported-id-swap"))
(define mutate-atom (compose-mutators arithmetic-op-swap
                                      boolean-op-swap
                                      class-method-publicity-swap
                                      delete-super-new
                                      ; data-accessor-swap left off
                                      nested-list-construction-swap
                                      replace-constants))

(define (mutate-benchmark module-body-stxs
                          mutation-index
                          #:program [the-program #f]
                          #:top-level-select
                          [top-level-selector select-define-body]
                          #:expression-select
                          [expression-selector select-exprs-as-if-untyped])
  (define replace-ids-with-top-level-defs
    (make-top-level-id-swap-mutator module-body-stxs))
  (define swap-imported-ids
    (make-imported-id-swap-mutator module-body-stxs the-program))
  (define swap-class-method-ids
    (make-method-call-swap-mutator module-body-stxs))
  (define mutate-expr
    (make-expr-mutator
     (compose-mutators delete-begin-result-expr
                       negate-conditionals
                       force-conditionals
                       wrap-conditionals
                       replace-class-parent
                       swap-class-initializers
                       rearrange-positional-exprs
                       add-extra-class-method
                       replace-ids-with-top-level-defs
                       swap-imported-ids
                       swap-class-method-ids
                       mutate-atom)
     #:select expression-selector))
  (define mutate-program
    (make-program-mutator mutate-expr
                          top-level-selector))
  (mutate-program module-body-stxs mutation-index))

(module+ test
  (require racket
           racket/match
           ruinit
           syntax/parse
           syntax/parse/define
           "../../mutate/mutate-expr.rkt"
           "../../mutate/mutate-program.rkt"
           "../../mutate/mutate-test-common.rkt"
           "../../mutate/mutator-lib.rkt"
           "../../mutate/mutators.rkt"
           "../../mutate/top-level-selectors.rkt"
           "../../mutate/expression-selectors.rkt")

  (define (mutate-program module-body-stxs mutation-index
                          #:top-level-select [top-level-selector select-define/contract]
                          #:expression-select [select select-any-expr])
    (mutate-benchmark module-body-stxs mutation-index
                      #:top-level-select top-level-selector
                      #:expression-select select))

  (define mutate-syntax
    (syntax-only mutate-program))

  (define mutate-program/no-counter
    (without-counter mutate-program))

  (define (mutate-syntax/define/c stx
                                  mutation-index)
    (match-define (mutated-program mutated-stx mutated-id)
      (mutate-program/define/c stx mutation-index))
    mutated-stx)

  (define (mutate-program/define/c stx mutation-index)
    (mutate-program/no-counter stx mutation-index))


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
        (define/contract b positive? (begin 1))})
    (test-mutation
     4
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin0 1 2))}
     #'{(define/contract (f x)
          any/c
          (or x #t))
        (define/contract b positive? (begin0 2))}))

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
              (define/contract b positive? (begin 1))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [5 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin 2 1))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [6 ,#'{(define/contract (f x)
                any/c
                (or x #t))
              (define/contract b positive? (begin -1 2))
              (define/contract (g x)
                any/c
                (if x 1 2))}]
       [11 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 -2))
               (define/contract (g x)
                 any/c
                 (if x 1 2))}]
       [16 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 2))
               (define/contract (g x)
                 any/c
                 (if (not x) 1 2))}]
       [17 ,#'{(define/contract (f x)
                 any/c
                 (or x #t))
               (define/contract b positive? (begin 1 2))
               (define/contract (g x)
                 any/c
                 (if x -1 2))}]
       [22 ,#'{(define/contract (f x)
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
                     22)))


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
       ;; extra method
       [1 ,#'{(define/contract c
                any/c
                (class my-parent
                  (define/public (a-nonexistant-method x) x)
                  (define/public (f x) x)
                  (define/private (g x) x)))}]
       ;; method visibility
       [2 ,#'{(define/contract c
                any/c
                (class my-parent
                  (define/private (f x) x)
                  (define/private (g x) x)))}]
       [3 ,#'{(define/contract c
                any/c
                (class my-parent
                  (define/public (f x) x)
                  (define/public (g x) x)))}]))
    ;; Initializer swapping
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class o (field w
                          y
                          [v (foo bar)]
                          [x 5]
                          [a (g 0)]
                          [b f]
                          [z #f])))}
     ;; Swap first pair of initializers
     `(;; add method
       [1 ,#'{(define/contract c
                any/c
                (class o
                  (define/public (a-nonexistant-method x) x)
                  (field w
                         y
                         [v (foo bar)]
                         [x 5]
                         [a (g 0)]
                         [b f]
                         [z #f])))}]
       ;; Swap first pair of initializers
       [2 ,#'{(define/contract c any/c (class o
                                         (field w
                                                y
                                                [v 5]
                                                [x (foo bar)]
                                                [a (g 0)]
                                                [b f]
                                                [z #f])))}]
       ;; Swap second pair of initializers
       [3 ,#'{(define/contract c any/c (class o
                                         (field w
                                                y
                                                [v (foo bar)]
                                                [x 5]
                                                [a f]
                                                [b (g 0)]
                                                [z #f])))}]
       ;; swap ordering
       [4 ,#'{(define/contract c any/c (class o
                                         (field y
                                                w
                                                [v (foo bar)]
                                                [x 5]
                                                [a (g 0)]
                                                [b f]
                                                [z #f])))}]
       [5 ,#'{(define/contract c any/c (class o
                                         (field w
                                                y
                                                [x 5]
                                                [v (foo bar)]
                                                [a (g 0)]
                                                [b f]
                                                [z #f])))}]
       [6 ,#'{(define/contract c any/c (class o
                                         (field w
                                                y
                                                [v (foo bar)]
                                                [x 5]
                                                [b f]
                                                [a (g 0)]
                                                [z #f])))}]
       ;; Descend into mutating initializer values
       ;; Note that final odd initializer is NOT swapped
       [7 ,#'{(define/contract c any/c (class o
                                         (field w
                                                y
                                                [v (foo bar)]
                                                [x -5]
                                                [a (g 0)]
                                                [b f]
                                                [z #f])))}]
       [12 ,#'{(define/contract c any/c (class o
                                          (field w
                                                 y
                                                 [v (foo bar)]
                                                 [x 5]
                                                 [a (g 0.0)]
                                                 [b f]
                                                 [z #f])))}]
       [15 ,#'{(define/contract c any/c (class o
                                          (field w
                                                 y
                                                 [v (foo bar)]
                                                 [x 5]
                                                 [a (g 0)]
                                                 [b f]
                                                 [z #t])))}]))
    ;; same test with init-field
    (test-mutation/sequence
     #'{(define/contract c
          any/c
          (class o (init-field w
                               y
                               [v (foo bar)]
                               [x 5]
                               [a (g 0)]
                               [b f]
                               [z #f])))}
     ;; Swap first pair of initializers
     `([2 ,#'{(define/contract c any/c (class o
                                         (init-field w
                                                     y
                                                     [v 5]
                                                     [x (foo bar)]
                                                     [a (g 0)]
                                                     [b f]
                                                     [z #f])))}]))

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
                  (define/public (a-nonexistant-method x) x) ; <-
                  (define/public (my-method x y)
                    (- x y))))}]
       [2 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/private #| <- |# (my-method x y)
                    (- x y))))}]
       [3 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/public (my-method y x #| <- |#)
                    (- x y))))}]
       [4 ,#'{(define/contract c
                any/c
                (class
                  o
                  (define/public (my-method x y)
                    (- y #| <-> |# x))))}]
       [5 ,#'{(define/contract c
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
     `([2 ,#'{(define/contract c
                any/c
                (class o
                  (void)
                  (define/public x 5)))}]
       [3 ,#'{(define/contract c
                any/c
                (class o
                  (super-new)
                  (define/private x 5)))}])))

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
    #:name cond-guarding
    (test-mutation/sequence
     #'{(define/contract a any/c (if #t + -))}
     `([0 ,#'{(define/contract a any/c (if (not #t) + -))}]
       [1 ,#'{(define/contract a any/c (if #t - -))}]
       [2 ,#'{(define/contract a any/c (if #t + +))}])))

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
                  (define/public (a-nonexistant-method x) x)
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
                   descr
                   id
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
       [3 ,#'{(provide
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
       [4 ,#'{(provide
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
       3))
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
        (define/contract (f x) any/c
          (: y Number)
          (define y (+ x x))
          (+ y y))}
     `([0 ,#'{(: f (-> Number Number))
              (define/contract (f x) any/c
                (: y Number)
                (define y (- x x))
                (+ y y))}]
       [1 ,#'{(: f (-> Number Number))
              (define/contract (f x) any/c
                (: y Number)
                (define y (+ x x))
                (- y y))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:expression-select select-exprs-as-if-untyped))))

  (test-begin
    #:name expr-filtering+top-level-selection
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
                      #:top-level-select select-any-define
                      #:expression-select select-exprs-as-if-untyped))))

  (test-begin
    #:name top-level-id-swap
    (test-mutation/sequence
     #'{(require foobar)
        (define (f x) (add1 x))
        (define (g x) (if x f g))
        (define (main) (if #t (f x) (g x)))}
     `([0 ,#'{(require foobar)
              (define (f x) (sub1 x))
              (define (g x) (if x f g))
              (define (main) (if #t (f x) (g x)))}]
       [1 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if (not x) f g))
              (define (main) (if #t (f x) (g x)))}]
       [2 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x g g))
              (define (main) (if #t (f x) (g x)))}]
       [3 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x main g))
              (define (main) (if #t (f x) (g x)))}]
       [4 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f f))
              (define (main) (if #t (f x) (g x)))}]
       [5 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f main))
              (define (main) (if #t (f x) (g x)))}]
       [6 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if (not #t) (f x) (g x)))}]
       [7 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if #t (g x) (g x)))}]
       [8 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if #t (main x) (g x)))}]
       [9 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if #t (f x) (f x)))}]
       [10 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if #t (f x) (main x)))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:top-level-select select-any-define)))))

;; Potential mutations that have been deferred:
;; - (hash a b ...) ~> (make-hash (list (cons a b) ...))
;;   Why? That form is not used in the benchmarks: only `zordoz` uses it, and
;;   only on a single line at that.
