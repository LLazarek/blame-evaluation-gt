#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names)

(require racket/function
         syntax/parse
         mutate/define
         mutate/low-level
         mutate/traversal
         "mutators.rkt")

(define active-mutators
  (list arithmetic-op-swap
        boolean-op-swap
        class-method-publicity-swap
        delete-super-new
        nested-list-construction-swap
        replace-constants
        delete-begin-result-expr
        negate-conditionals
        force-conditionals
        replace-class-parent
        swap-class-initializers
        rearrange-positional-exprs))

(define active-dependent-mutators
  (list make-top-level-id-swap-mutator
        make-imported-id-swap-mutator
        make-method-id-swap-mutator
        make-field-id-swap-mutator))

(define active-mutator-names
  (append (map mutator-type active-mutators)
          (map dependent-mutator-type active-dependent-mutators)))

(define (mutate-benchmark module-body-stxs
                          mutation-index
                          #:program [the-program #f]
                          #:top-level-select
                          [top-level-selector select-define-body]
                          #:expression-select
                          [expression-selector select-exprs-as-if-untyped]
                          #:transformer [transform identity])
  (define instantiated-dependent-mutators
    (for/list ([make-mutator (in-list active-dependent-mutators)])
      (make-mutator module-body-stxs the-program)))
  (define mutate-expr
    (make-expr-mutator
     (apply compose-mutators
            (append active-mutators
                    instantiated-dependent-mutators))
     #:select expression-selector))
  (define mutate-program
    (transform
     (make-program-mutator mutate-expr
                           #:select top-level-selector)))
  (mutate-program module-body-stxs mutation-index))

(module+ test
  (require racket
           racket/match
           ruinit
           syntax/parse
           syntax/parse/define
           mutate/tests/testing-util
           "../../util/program.rkt")

  (define (mutate-program module-body-stxs mutation-index
                          #:program [program (program (mod "main.rkt"
                                                           #'(module main racket (#%module-begin)))
                                                      empty)]
                          #:top-level-select [top-level-selector select-define-body]
                          #:expression-select [select select-any-expr]
                          #:transformer [transform identity])
    (mutate-benchmark module-body-stxs mutation-index
                      #:top-level-select top-level-selector
                      #:expression-select select
                      #:program program
                      #:transformer transform))

  (define mutate-syntax
    (curry mutate-program #:transformer syntax-only))

  (define mutate-program/no-counter
    (curry mutate-program #:transformer without-counter))

  (define mutate-syntax/define/c
    (curry mutate-syntax
           #:top-level-select select-define/contract-body))

  (define mutate-program/define/c
    (curry mutate-program/no-counter
           #:top-level-select select-define/contract-body))


  (define-test (test-mutation index orig-prog new-prog
                              [mutate-syntax mutate-syntax])
    (define mutated (mutate-syntax orig-prog index))
    (if (equal? mutated no-more-mutations-flag)
        (fail "Mutation index exceeded")
        (test-programs-equal?
         mutated
         new-prog)))
  (define-test (test-mutation/sequence orig-program expects
                                       [mutate-syntax mutate-syntax])
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
     #'{(define a #t)}
     #'{(define a #f)})
    (test-mutation
     0
     #'{(define a #f)}
     #'{(define a #t)})

    (test-mutation
     0
     #'{(define a 1)}
     #'{(define a -1)})

    (test-mutation
     0
     #'{(define a -1)}
     #'{(define a 1)})
    (test-mutation
     0
     #'{(define a 5)}
     #'{(define a -5)})
    (test-mutation
     0
     #'{(define a 3)}
     #'{(define a -3)})
    (test-mutation
     0
     #'{(define a 3.5)}
     #'{(define a -3.5)})
    (test-equal?
     (mutate-syntax/define/c
      #'{(define a (λ (x) x))}
      0)
     no-more-mutations-flag)

    (test-mutation/sequence
     #'{(define a 1)
        (define b 2)}
     `([0 ,#'{(define a -1)
              (define b 2)}]
       [5 ,#'{(define a 1)
              (define b -2)}]))

    (test-mutation
     0
     #'{(define (f x)
          1)
        (define b 2)}
     #'{(define (f x)
          -1)
        (define b 2)}))

  (test-begin
    #:name full:operators
    (test-mutation/sequence
     #'{(define a (+ 1 2))}
     `([0 ,#'{(define a (+ 2 1))}]
       [1 ,#'{(define a (- 1 2))}]
       [2 ,#'{(define a (+ -1 2))}]
       [7 ,#'{(define a (+ 1 -2))}]))

    (test-mutation/sequence
     #'{(define (f x)
          (+ x 2))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (+ 2 x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (- x 2))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (- x 2))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (- 2 x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (+ x 2))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (* x 2))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (* 2 x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (/ x 2))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (quotient x 2))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (quotient 2 x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (/ x 2))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (modulo x 2))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (modulo 2 x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (/ x 2))
              (define b 2)}]))

    (test-mutation/sequence
     #'{(define (f x)
          (add1 x))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (sub1 x))
              (define b 2)}]))

    (test-mutation/sequence
     #'{(define (f x)
          (and x #t))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (and #t x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (or x #t))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (or x #t))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (or #t x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (and x #t))
              (define b 2)}]))
    (test-mutation/sequence
     #'{(define (f x)
          (append '() '()))
        (define b 2)}
     `([0 ,#'{(define (f x)
                (cons '() '()))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (append '() '()))
              (define b -2)}]))

    ;; Test choices of index
    (test-mutation/sequence
     #'{(define (f x)
          (or x #t))
        (define b 2)}
     ;; Fixed this! ⇓
     `(#;[1 ,#'{(define (f x)
                  (or x #t)) ;; tries to mutate `x` but it's a no-op
                (define b 2)}]
       [0 ,#'{(define (f x)
                (or #t x))
              (define b 2)}]
       [1 ,#'{(define (f x)
                (and x #t))
              (define b 2)}]
       [2 ,#'{(define (f x)
                (or x #f))
              (define b 2)}]
       [3 ,#'{(define (f x)
                (or x 1))
              (define b 2)}]
       [4 ,#'{(define (f x)
                (or x #t))
              (define b -2)}])))

  (test-begin
    #:name begin
    (test-mutation
     4
     #'{(define (f x)
          (or x #t))
        (define b (begin 1 2))}
     #'{(define (f x)
          (or x #t))
        (define b (begin 1))})
    (test-mutation
     4
     #'{(define (f x)
          (or x #t))
        (define b (begin0 1 2))}
     #'{(define (f x)
          (or x #t))
        (define b (begin0 2))}))

  (test-begin
    #:name implicit-begins
    (test-mutation/sequence
     #'{(define (f x)
          'one
          'two)
        (define b (cond [#t 'one 'two]))
        (define c (λ () 'one 'two))}
     `([0 ,#'{(define (f x)
                'one)
              (define b (cond [#t 'one 'two]))
              (define c (λ () 'one 'two))}]
       [1 ,#'{(define (f x)
                'two
                'one)
              (define b (cond [#t 'one 'two]))
              (define c (λ () 'one 'two))}]
       [2 ,#'{(define (f x)
                'one
                'two)
              (define b (cond [#t 'one]))
              (define c (λ () 'one 'two))}]
       [3 ,#'{(define (f x)
                'one
                'two)
              (define b (cond [(not #t) 'one 'two]))
              (define c (λ () 'one 'two))}]
       [4 ,#'{(define (f x)
                'one
                'two)
              (define b (cond [#t 'two 'one]))
              (define c (λ () 'one 'two))}]
       [5 ,#'{(define (f x)
                'one
                'two)
              (define b (cond [#t 'one 'two]))
              (define c (λ () 'one))}]))
    (test-mutation
     4
     #'{(define (f x)
          (or x #t))
        (define b (begin0 1 2))}
     #'{(define (f x)
          (or x #t))
        (define b (begin0 2))}))

  (test-begin
    #:name if
    (test-mutation
     4
     #'{(define (f x)
          (or x #t))
        (define (g x)
          (if x 1 2))}
     #'{(define (f x)
          (or x #t))
        (define (g x)
          (if (not x) 1 2))}))

  (test-begin
    #:name function-application-args-swapping
    (test-mutation/sequence
     #'{(define x (f 1 2 3 4 5))}
     `([0 ,#'{(define x (f 2 1 3 4 5))}]
       [1 ,#'{(define x (f 1 2 4 3 5))}]
       [2 ,#'{(define x (f -1 2 3 4 5))}]
       #| ... |#)))


  (test-begin
    #:name complex-program
    (test-mutation/sequence
     #'{(define (f x)
          (or x #t))
        (define b (begin 1 2))
        (define (g x)
          (if x 1 2))}
     `([4 ,#'{(define (f x)
                (or x #t))
              (define b (begin 1))
              (define (g x)
                (if x 1 2))}]
       [5 ,#'{(define (f x)
                (or x #t))
              (define b (begin 2 1))
              (define (g x)
                (if x 1 2))}]
       [6 ,#'{(define (f x)
                (or x #t))
              (define b (begin -1 2))
              (define (g x)
                (if x 1 2))}]
       [11 ,#'{(define (f x)
                 (or x #t))
               (define b (begin 1 -2))
               (define (g x)
                 (if x 1 2))}]
       [16 ,#'{(define (f x)
                 (or x #t))
               (define b (begin 1 2))
               (define (g x)
                 (if (not x) 1 2))}]
       [17 ,#'{(define (f x)
                 (or x #t))
               (define b (begin 1 2))
               (define (g x)
                 (if #t 1 2))}]
       [18 ,#'{(define (f x)
                 (or x #t))
               (define b (begin 1 2))
               (define (g x)
                 (if x -1 2))}]
       [23 ,#'{(define (f x)
                 (or x #t))
               (define b (begin 1 2))
               (define (g x)
                 (if x 1 -2))}])))

  (test-begin
    #:name out-of-mutations
    (test-equal?
     (mutate-program #'{(define (f x)
                          (or x #t))
                        (define b (begin 1 2))
                        (define (g x)
                          (if x 1 x))}
                     25)
     no-more-mutations-flag))


  (test-begin
    #:name classes
    (test-mutation/sequence
     #'{(define c
          (class my-parent
            (define/public (f x) x)
            (define/private (g x) x)))}
     ;; superclass
     `([0 ,#'{(define c
                (class object%
                  (define/public (f x) x)
                  (define/private (g x) x)))}]
       ;; method visibility
       [1 ,#'{(define c
                (class my-parent
                  (define/private (f x) x)
                  (define/private (g x) x)))}]
       [2 ,#'{(define c
                (class my-parent
                  (define/public (f x) x)
                  (define/public (g x) x)))}]))
    ;; Initializer swapping
    (test-mutation/sequence
     #'{(define c
          (class o (field w
                          y
                          [v (foo bar)]
                          [x 5]
                          [a (g 0)]
                          [b f]
                          [z #f])))}
     ;; Swap first pair of initializers
     `(;; Swap first pair of initializers
       [1 ,#'{(define c (class o
                          (field w
                                 y
                                 [v 5]
                                 [x (foo bar)]
                                 [a (g 0)]
                                 [b f]
                                 [z #f])))}]
       ;; Swap second pair of initializers
       [2 ,#'{(define c (class o
                          (field w
                                 y
                                 [v (foo bar)]
                                 [x 5]
                                 [a f]
                                 [b (g 0)]
                                 [z #f])))}]
       ;; swap ordering
       [3 ,#'{(define c (class o
                          (field y
                                 w
                                 [v (foo bar)]
                                 [x 5]
                                 [a (g 0)]
                                 [b f]
                                 [z #f])))}]
       [4 ,#'{(define c (class o
                          (field w
                                 y
                                 [x 5]
                                 [v (foo bar)]
                                 [a (g 0)]
                                 [b f]
                                 [z #f])))}]
       [5 ,#'{(define c (class o
                          (field w
                                 y
                                 [v (foo bar)]
                                 [x 5]
                                 [b f]
                                 [a (g 0)]
                                 [z #f])))}]
       ;; Descend into mutating initializer values
       ;; Note that final odd initializer is NOT swapped
       [6 ,#'{(define c (class o
                          (field w
                                 y
                                 [v (foo bar)]
                                 [x -5]
                                 [a (g 0)]
                                 [b f]
                                 [z #f])))}]
       [11 ,#'{(define c (class o
                           (field w
                                  y
                                  [v (foo bar)]
                                  [x 5]
                                  [a (g 0.0)]
                                  [b f]
                                  [z #f])))}]
       [14 ,#'{(define c (class o
                           (field w
                                  y
                                  [v (foo bar)]
                                  [x 5]
                                  [a (g 0)]
                                  [b f]
                                  [z #t])))}]))
    ;; same test with init-field
    (test-mutation/sequence
     #'{(define c
          (class o (init-field w
                               y
                               [v (foo bar)]
                               [x 5]
                               [a (g 0)]
                               [b f]
                               [z #f])))}
     ;; Swap first pair of initializers
     `([1 ,#'{(define c (class o
                          (init-field w
                                      y
                                      [v 5]
                                      [x (foo bar)]
                                      [a (g 0)]
                                      [b f]
                                      [z #f])))}]))

    ;; Initializer arg swapping at instantiation
    (test-mutation/sequence
     #'{(define o (new my-class [a "hi"] [b #f]))}
     `([0 ,#'{(define o (new my-class [a #f] [b "hi"]))}]))

    ;; mutation of method bodies
    (test-mutation/sequence
     #'{(define c
          (class
            o
            (define/public (my-method x y)
              (- x y))))}
     `([0 ,#'{(define c
                (class
                  object% #| <- |#
                  (define/public (my-method x y)
                    (- x y))))}]
       [1 ,#'{(define c
                (class
                  o
                  (define/private #| <- |# (my-method x y)
                    (- x y))))}]
       [2 ,#'{(define c
                (class
                  o
                  (define/public (my-method y x #| <- |#)
                    (- x y))))}]
       [3 ,#'{(define c
                (class
                  o
                  (define/public (my-method x y)
                    (- y #| <-> |# x))))}]
       [4 ,#'{(define c
                (class
                  o
                  (define/public (my-method x y)
                    (+ #| <- |# x y))))}]))

    ;; super-new replacement
    (test-mutation/sequence
     #'{(define c
          (class o
            (super-new)
            (define/public x 5)))}
     `([1 ,#'{(define c
                (class o
                  (void)
                  (define/public x 5)))}]
       [2 ,#'{(define c
                (class o
                  (super-new)
                  (define/private x 5)))}])))

  (test-begin
    #:name nested-exprs
    (test-mutation/sequence
     #'{(define a (+ (+ 1 2)
                     (- 3 4)))}
     `(;; flip outer args
       [0 ,#'{(define a (+ (- 3 4)
                           (+ 1 2)))}]
       ;; negate outer +
       [1 ,#'{(define a (- (+ 1 2)
                           (- 3 4)))}]
       ;; flip inner args 1
       [2 ,#'{(define a (+ (+ 2 1)
                           (- 3 4)))}]
       ;; negate inner +
       [3 ,#'{(define a (+ (- 1 2)
                           (- 3 4)))}]
       ;; mutate inner + args
       [4 ,#'{(define a (+ (+ -1 2)
                           (- 3 4)))}]
       ;; . . . . . . . . .
       ;; flip inner args 2
       [14 ,#'{(define a (+ (+ 1 2)
                            (- 4 3)))}]
       ;; negate inner -
       [15 ,#'{(define a (+ (+ 1 2)
                            (+ 3 4)))}]
       ;; mutate inner - args
       [16 ,#'{(define a (+ (+ 1 2)
                            (- -3 4)))}]))
    (test-mutation/sequence
     #'{(define (foo x) x)
        (define a (+ (foo 1) 2))}
     `([2 ,#'{(define (foo x) x)
              (define a (+ (a 1) 2))}]
       [3 ,#'{(define (foo x) x)
              (define a (+ (foo -1) 2))}])))

  (test-begin
    #:name cond-guarding
    (test-mutation/sequence
     #'{(define a (if #t + -))}
     `([0 ,#'{(define a (if (not #t) + -))}]
       [1 ,#'{(define a (if #t - -))}]
       [2 ,#'{(define a (if #t + +))}])))

  (test-begin
    #:name higher-order
    ;; Test the mutator's ability to handle higher order expressions
    ;; 1. The function being applied is an expression
    ;; 2. Primitives that should be mutated (+, -) appear in expression ctx
    ;;    rather than application ctx
    (test-mutation/sequence
     #'{(define a ((if #t + -) 1 2))}
     `([0 ,#'{(define a ((if #t + -) 2 1))}]
       [1 ,#'{(define a ((if (not #t) + -) 1 2))}]
       [2 ,#'{(define a ((if #t - -) 1 2))}]
       [3 ,#'{(define a ((if #t + +) 1 2))}]
       [4 ,#'{(define a ((if #t + -) -1 2))}]
       [9 ,#'{(define a ((if #t + -) 1 -2))}])))

  (test-begin
    #:name mutated-id-reporting
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/no-counter
       #'{(define (f x)
            (<= x 2))
          (define b 2)}
       5))
     'f)
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/no-counter
       #'{(define (f x)
            (<= x 2))
          (define b 2)}
       6))
     'b)
    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/no-counter
       #'{(displayln "B")
          (define c 1)
          (define d
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
     #'{(define b 2)
        (define (singleton-list? x)

          (and (list? x)
               (not (null? x))
               (null? (cdr x))))}
     `([5 ,#'{(define b 2)
              (define (singleton-list? x)

                (and (not (null? x))
                     (list? x)
                     (null? (cdr x))))}]
       [6 ,#'{(define b 2)
              (define (singleton-list? x)

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

        (define/ignore (assert v p) ; fake definer for testing
          (unless (p v) (error 'assert))
          v)


        (define command%
          (class object%
            (super-new)
            (init-field
             id
             descr
             exec)))

        (define/ignore ((env-with/c cmd-ids) env)
          (cond [(env? env)
                 (define env-cmd-ids
                   (for/list ([env-cmd (in-list env)])
                     (get-field id env-cmd)))
                 (for/and ([c (in-list cmd-ids)])
                   (member c env-cmd-ids))]
                [else #f]))



        ;; True if the argument is a list with one element
        (define (singleton-list? x)

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

              (define/ignore (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define command%
                (class object%
                  (void)
                  (init-field
                   id
                   descr
                   exec)))

              (define/ignore ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define (singleton-list? x)

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

              (define/ignore (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define command%
                (class object%
                  (super-new)
                  (init-field
                   descr
                   id
                   exec)))

              (define/ignore ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define (singleton-list? x)

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

              (define/ignore (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define command%
                (class object%
                  (super-new)
                  (init-field
                   id
                   descr
                   exec)))

              (define/ignore ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define (singleton-list? x)

                (and (not (null? x))
                     (list? x)
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

              (define/ignore (assert v p)
                (unless (p v) (error 'assert))
                v)

              (define command%
                (class object%
                  (super-new)
                  (init-field
                   id
                   descr
                   exec)))

              (define/ignore ((env-with/c cmd-ids) env)
                (cond [(env? env)
                       (define env-cmd-ids
                         (for/list ([env-cmd (in-list env)])
                           (get-field id env-cmd)))
                       (for/and ([c (in-list cmd-ids)])
                         (member c env-cmd-ids))]
                      [else #f]))



              ;; True if the argument is a list with one element
              (define (singleton-list? x)

                (or (list? x)
                    (not (null? x))
                    (null? (cdr x))))}]))

    (test-equal?
     (mutated-program-mutated-id
      (mutate-program/no-counter
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

          (define/ignore (assert v p)
            (unless (p v) (error 'assert))
            v)


          (define command%
            (class object%
              (super-new)
              (init-field
               id
               descr
               exec)))

          (define/ignore ((env-with/c cmd-ids) env)
            (cond [(env? env)
                   (define env-cmd-ids
                     (for/list ([env-cmd (in-list env)])
                       (get-field id env-cmd)))
                   (for/and ([c (in-list cmd-ids)])
                     (member c env-cmd-ids))]
                  [else #f]))



          ;; True if the argument is a list with one element
          (define (singleton-list? x)

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
    (let expand-result ([res (selector/ctc stx)])
      (match res
          [(list parts name reconstructor)
           (and/test (test-parts parts)
                     (test-name name)
                     (test-reconstructor reconstructor))]
          [#f (expand-result (list #f #f #f))])))
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
     select-define-body
     #'(define (f x) (+ y y))
     #:name (make-name-test 'f)
     #:parts (make-parts-test (list #'(begin (+ y y))))
     #:reconstructor (make-reconstructor-test (list #'(begin foo))
                                              #'(define (f x) foo)))
    (test-selector
     select-define-body
     #'(define-type T Any)
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
     select-define/contract-body
     #'(define/contract (f x) ctc (+ y y))
     #:name (make-name-test 'f)
     #:parts (make-parts-test (list #'(begin (+ y y))))
     #:reconstructor (make-reconstructor-test (list #'(begin foo))
                                              #'(define/contract (f x) ctc foo)))
    (test-selector
     select-define/contract-body
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
                      #:top-level-select select-define-body)))
    (test-equal? (mutate-program #'{(foobar)
                                    (define (f x)
                                      (+ y y))}
                                 1)
                 no-more-mutations-flag))

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
                (define y (+ x x)))}]
       [1 ,#'{(: f (-> Number Number))
              (define (f x)
                (: y Number)
                (+ y y)
                (define y (+ x x)))}]
       [2 ,#'{(: f (-> Number Number))
              (define (f x)
                (: y Number)
                (define y (- x x))
                (+ y y))}]
       [3 ,#'{(: f (-> Number Number))
              (define (f x)
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
                (define y (+ x x)))}]
       [1 ,#'{(: f (-> Number Number))
              (define (f x)
                (: y Number)
                (+ y y)
                (define y (+ x x)))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:top-level-select select-define-body
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
              (define (g x) (if #t f g))
              (define (main) (if #t (f x) (g x)))}]
       [3 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x g g))
              (define (main) (if #t (f x) (g x)))}]
       [4 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x main g))
              (define (main) (if #t (f x) (g x)))}]
       [5 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f f))
              (define (main) (if #t (f x) (g x)))}]
       [6 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f main))
              (define (main) (if #t (f x) (g x)))}]
       [7 ,#'{(require foobar)
              (define (f x) (add1 x))
              (define (g x) (if x f g))
              (define (main) (if (not #t) (f x) (g x)))}]
       [8 ,#'{(require foobar)
               (define (f x) (add1 x))
               (define (g x) (if x f g))
               (define (main) (if #t (g x) (g x)))}]
       [9 ,#'{(require foobar)
               (define (f x) (add1 x))
               (define (g x) (if x f g))
               (define (main) (if #t (main x) (g x)))}]
       [10 ,#'{(require foobar)
               (define (f x) (add1 x))
               (define (g x) (if x f g))
               (define (main) (if #t (f x) (f x)))}]
       [11 ,#'{(require foobar)
               (define (f x) (add1 x))
               (define (g x) (if x f g))
               (define (main) (if #t (f x) (main x)))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:top-level-select select-define-body))))

  ;; disabled, see above
  (test-begin
    #:name imported-id-swap
    (test-mutation/sequence
     #'{(require "a.rkt")
        (require "b.rkt")
        (define x (f (g (h 'a))))}
     `([0 ,#'{(require "a.rkt")
              (require "b.rkt")
              (define x (g (g (h 'a))))}]
       [1 ,#'{(require "a.rkt")
              (require "b.rkt")
              (define x (f (f (h 'a))))}])
     (λ (stx mi)
       (mutate-syntax stx mi
                      #:top-level-select select-define-body
                      #:program (program (mod "main.rkt" #'(module main racket (#%module-begin)))
                                         (list (mod "a.rkt"
                                                    #'(module main racket
                                                        (#%module-begin
                                                         (provide f g)
                                                         (define (f x) x)
                                                         (define (g x) x))))
                                               (mod "a.rkt"
                                                    #'(module main racket
                                                        (#%module-begin
                                                         (provide h something-else)
                                                         (define (h x) x))))))))))

  (test-begin
    #:name method-id-swap
    (test-mutation/sequence
     #'{(define c1 (class object%
                     (define/public (c1-m1 x) x)
                     (define/public (c1-m2 x) x)))
        (define another-def 'aaa)
        (define c2 (box (class object%
                          (define/public (c2-m1 y) y)
                          (define/public c2-field 'ccc))))
        (define result (send (new c1) c1-m2 'bbb))}
     `(#;[0 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [0 ,#'{(define c1 (class object%
                           (define/private (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [1 ,#'{(define c1 (class object%
                           (define/public (c1-m2 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [2 ,#'{(define c1 (class object%
                           (define/public (c2-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       ;; lltodo: wiw: methods get reordered too
       [3 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/private (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [4 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m1 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [5 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c2-m1 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [6 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/private (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [7 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c1-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [8 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c1-m2 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [9 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/private c2-field 'ccc))))
              (define result (send (new c1) c1-m2 'bbb))}]
       [10 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send c1-m2 (new c1) 'bbb))}]
       [11 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new another-def) c1-m2 'bbb))}]
       [12 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c2) c1-m2 'bbb))}]
       [13 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new result) c1-m2 'bbb))}]
       [14 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c1-m1 'bbb))}]
       [15 ,#'{(define c1 (class object%
                           (define/public (c1-m1 x) x)
                           (define/public (c1-m2 x) x)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (define/public (c2-m1 y) y)
                                (define/public c2-field 'ccc))))
              (define result (send (new c1) c2-m1 'bbb))}])))

  (test-begin
    #:name field-id-swap
    (test-mutation/sequence
     #'{(define c1 (class object%
                     (field [c1-f1 'aaa])
                     (define/public (c1-m1 x) c1-f1)))
        (define another-def 'aaa)
        (define c2 (box (class object%
                          (inherit-field c2-f1 [blah c2-f2])
                          (define/public (c2-m1 y) c2-f2))))
        (define result (send (new c1) c1-m1 'bbb))}
     `(#;[0 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [0 ,#'{(define c1 (class object%
                             (field [c2-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [1 ,#'{(define c1 (class object%
                             (field [c2-f2 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [2 ,#'{(define c1 (class object%
                           (field [c1-f1 'aaa])
                           (define/private (c1-m1 x) c1-f1)))
              (define another-def 'aaa)
              (define c2 (box (class object%
                                (inherit-field c2-f1 [blah c2-f2])
                                (define/public (c2-m1 y) c2-f2))))
              (define result (send (new c1) c1-m1 'bbb))}]
       [3 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c2-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [4 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c2-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [5 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c2-f2)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [6 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field [blah c2-f2] c2-f1)
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [7 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c1-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [8 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f2 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [9 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c1-f1])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [10 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f1])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [11 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/private (c2-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [12 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c1-m1 y) c2-f2))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [13 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c1-f1))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [14 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f1))))
                (define result (send (new c1) c1-m1 'bbb))}]
       [15 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send c1-m1 (new c1) 'bbb))}]
       [16 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new another-def) c1-m1 'bbb))}]
       [17 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c2) c1-m1 'bbb))}]
       [18 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new result) c1-m1 'bbb))}]
       [19 ,#'{(define c1 (class object%
                             (field [c1-f1 'aaa])
                             (define/public (c1-m1 x) c1-f1)))
                (define another-def 'aaa)
                (define c2 (box (class object%
                                  (inherit-field c2-f1 [blah c2-f2])
                                  (define/public (c2-m1 y) c2-f2))))
                (define result (send (new c1) c2-m1 'bbb))}]))))

;; Potential mutations that have been deferred:
;; - (hash a b ...) ~> (make-hash (list (cons a b) ...))
;;   Why? That form is not used in the benchmarks: only `zordoz` uses it, and
;;   only on a single line at that.
