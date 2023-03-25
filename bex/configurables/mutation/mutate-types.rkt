#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names)

(require racket/function
         syntax/parse
         racket/list
         racket/stream
         mutate/define
         mutate/low-level
         mutate/traversal
         "mutators.rkt")

(define-id-mutator known-type-generalization
  #:type "known-type-generalization"
  [Float #:-> Float-Complex]

  [Real #:-> Number]
  [Integer #:-> Real]
  [Index #:-> Integer]
  [Byte #:-> Index])

(define-id-mutator known-type-restriction
  #:type "known-type-restriction"
  [Float-Complex #:-> Float]

  [Number #:-> Real]
  [Real #:-> Integer]
  [Integer #:-> Index]
  [Index #:-> Byte])

(module+ test
  (require ruinit
           racket
           mutate/tests/testing-util)

  (test-begin
    #:name known-type-generalization
    (test-mutator* known-type-generalization
                   #'Byte
                   (list #'Index
                         #'Byte))
    (test-mutator* known-type-generalization
                   #'Real
                   (list #'Number
                         #'Real)))

  (test-begin
    #:name known-type-restriction
    (test-mutator* known-type-restriction
                   #'Index
                   (list #'Byte
                         #'Index))
    (test-mutator* known-type-restriction
                   #'Number
                   (list #'Real
                         #'Number))))

(define rearrange-type-positions rearrange-positional-exprs)

(define-syntax-class Arrow
  #:description "-> or ->*"
  #:commit
  (pattern {~or* {~datum ->}
                 {~datum ->*}}))

(define-mutator (drop-ho-function-args stx mutation-index counter)
  #:type "drop-ho-function-arg"

  (define-simple-mutator (drop-last-arg-if-arrow stx)
    #:type (current-mutator-type)
    #:pattern (inner-arrow:Arrow argTs ... argT resultT)
    #'(inner-arrow argTs ... resultT))

  (syntax-parse stx
    [(outer-arrow:Arrow
      arg-or-resT ...)
     (mutated-do-single
      [mutated-arg-or-resTs (mutate-in-sequence (attribute arg-or-resT)
                                                mutation-index
                                                counter
                                                drop-last-arg-if-arrow)]
      #:return
      (quasisyntax/loc stx
        (outer-arrow #,@mutated-arg-or-resTs)))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name drop-ho-function-args
    (test-mutator* drop-ho-function-args
                   #'(Number (Any . -> . Any) . -> . (Number String . -> . Any))
                   (list #'(Number (-> Any) . -> . (Number String . -> . Any))
                         #'(Number (Any . -> . Any) . -> . (Number . -> . Any))
                         #'(Number (Any . -> . Any) . -> . (Number String . -> . Any))))
    (test-mutator* drop-ho-function-args
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* drop-ho-function-args
                   #'Number
                   (list #'Number))
    (test-mutator* drop-ho-function-args
                   #'(Class ClassTop (Any . -> . Any))
                   (list #'(Class ClassTop (Any . -> . Any))))))

(define-simple-mutator (drop-union-branch stx)
  #:pattern ({~and {~or {~datum Union} {~datum U}} u} branch ...)
  (let ([branches (attribute branch)])
    (for/stream ([i (in-range (length branches))])
      (quasisyntax/loc stx
        (u #,@(list-drop branches i))))))
(define (list-drop l i)
  (define-values {left i+rest} (split-at l i))
  (append left (rest i+rest)))
#;(define (drop-union-branch stx mutation-index counter)
  (define (drop-branch stx mutation-index counter)
    (maybe-mutate stx
                  #'[]
                  mutation-index
                  counter))

  (log-mutation-type "drop-union-branch")
  (syntax-parse stx
    [({~and {~or {~datum Union} {~datum U}} u} branch ...)
     (mutated-do-single
      [mutated-branches (mutate-in-seq (syntax->list #'([branch] ...))
                                       mutation-index
                                       counter
                                       drop-branch)]
      #:return
      (syntax-parse mutated-branches
        [({~or* [branch*] []} ...)
         (syntax/loc stx
           (u {~? branch*} ...))]))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name drop-union-branch
    (test-mutator* drop-union-branch
                   #'(U A B C)
                   (list #'(U B C)
                         #'(U A C)
                         #'(U A B)
                         #'(U A B C)))
    (test-mutator* drop-union-branch
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* drop-union-branch
                   #'Number
                   (list #'Number))
    (test-mutator* drop-union-branch
                   #'(Class ClassTop (Any . -> . Any))
                   (list #'(Class ClassTop (Any . -> . Any))))))

(define-mutator (drop-class-methods stx mutation-index counter)
  #:type "class:drop-method"
  (define-mutator (drop-if-public-method stx mutation-index counter)
    #:type (current-mutator-type)
    (syntax-parse stx
      [([{~not {~or* {~datum init}
                     {~datum init-field}
                     {~datum init-rest}
                     {~datum field}
                     {~datum augment}}}
         T])
       (maybe-mutate stx
                     #'()
                     mutation-index
                     counter)]
      [else
       (no-mutation stx mutation-index counter)]))

  (syntax-parse stx
    [({~and class {~datum Class}} clause ...)
     (mutated-do-single
      [mutated-clauses (mutate-in-sequence (syntax->list #'([clause] ...))
                                           mutation-index
                                           counter
                                           drop-if-public-method)]
      #:return
      (syntax-parse mutated-clauses
        [({~or* [clause*] []} ...)
         (syntax/loc stx
           (class {~? clause*} ...))]))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name drop-class-methods
    (test-mutator* drop-class-methods
                   #'(Class (field [a Ta] [b Tb])
                            [m1 (Number . -> . String)]
                            [m2 (Number . -> . String)]
                            (init [c Tc])
                            [m3 (Number . -> . String)])
                   (list #'(Class (field [a Ta] [b Tb])
                                  [m2 (Number . -> . String)]
                                  (init [c Tc])
                                  [m3 (Number . -> . String)])
                         #'(Class (field [a Ta] [b Tb])
                                  [m1 (Number . -> . String)]
                                  (init [c Tc])
                                  [m3 (Number . -> . String)])
                         #'(Class (field [a Ta] [b Tb])
                                  [m1 (Number . -> . String)]
                                  [m2 (Number . -> . String)]
                                  (init [c Tc]))
                         #'(Class (field [a Ta] [b Tb])
                                  [m1 (Number . -> . String)]
                                  [m2 (Number . -> . String)]
                                  (init [c Tc])
                                  [m3 (Number . -> . String)])))
    (test-mutator* drop-class-methods
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* drop-class-methods
                   #'Number
                   (list #'Number))
    (test-mutator* drop-class-methods
                   #'(Class ClassTop (field [a Ta]))
                   (list #'(Class ClassTop (field [a Ta]))))))

(define-mutator (swap-class-implements stx mutation-index counter)
  #:type "class:swap-implements"
  (syntax-parse stx
    [({~and {~datum Class} class} before ... #:implements T after ...)
     (mutated-do-single
      [mutated-implements-stx (maybe-mutate (attribute T)
                                            #'ClassTop
                                            mutation-index
                                            counter)]
      #:return
      (quasisyntax/loc stx
        (class
          before ...
          #:implements #,mutated-implements-stx
          after ...)))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name swap-class-implements
    (test-mutator* swap-class-implements
                   #'(Class (field [a Ta] [b Tb])
                            #:implements FooBar
                            [m1 (Number . -> . String)])
                   (list #'(Class (field [a Ta] [b Tb])
                            #:implements ClassTop
                            [m1 (Number . -> . String)])
                         #'(Class (field [a Ta] [b Tb])
                            #:implements FooBar
                            [m1 (Number . -> . String)])))
    (test-mutator* swap-class-implements
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* swap-class-implements
                   #'Number
                   (list #'Number))
    (test-mutator* swap-class-implements
                   #'(Class ClassTop (field [a Ta]))
                   (list #'(Class ClassTop (field [a Ta]))))))

(define-mutator (make-optional-args-mandatory stx mutation-index counter)
  #:type "optional-args-mandatory"
  (syntax-parse stx
    [({~and {~datum ->*} arrow} (mandatory-dom ...) optional-doms
                                {~optional
                                 {~and {~seq _:keyword _}
                                       {~seq rest-kw ...}}}
                                rng)
     (maybe-mutate stx
                   (syntax/loc stx
                     (arrow (mandatory-dom ... . optional-doms)
                            {~? {~@ rest-kw ...}}
                            rng))
                   mutation-index
                   counter)]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name make-optional-args-mandatory
    (test-mutator* make-optional-args-mandatory
                   #'(->* {A B} {C D} R)
                   (list #'(->* {A B C D} R)
                         #'(->* {A B} {C D} R)))
    (test-mutator* make-optional-args-mandatory
                   #'(->* {A B} {C D} #:rest E R)
                   (list #'(->* {A B C D} #:rest E R)
                         #'(->* {A B} {C D} #:rest E R)))
    (test-mutator* make-optional-args-mandatory
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* make-optional-args-mandatory
                   #'Number
                   (list #'Number))
    (test-mutator* make-optional-args-mandatory
                   #'(Class ClassTop (field [a Ta]))
                   (list #'(Class ClassTop (field [a Ta]))))))

(define-mutator (replace-with-Any stx mutation-index counter)
  #:type "replace-with-Any"
  (maybe-mutate stx
                (syntax/loc stx Any)
                mutation-index
                counter))

(module+ test
  (test-begin
    #:name replace-with-Any
    (test-mutator* replace-with-Any
                   #'(->* {A B} {C D} R)
                   (list #'Any
                         #'(->* {A B} {C D} R)))
    (test-mutator* replace-with-Any
                   #'(Number Any . -> . String)
                   (list #'Any
                         #'(Number Any . -> . String)))
    (test-mutator* replace-with-Any
                   #'Number
                   (list #'Any
                         #'Number))
    (test-mutator* replace-with-Any
                   #'(Class ClassTop (field [a Ta]))
                   (list #'Any
                         #'(Class ClassTop (field [a Ta]))))))

(define-mutator (drop-case->-cases stx mutation-index counter)
  #:type "drop-case->-case"
  (define-mutator (drop-case stx mutation-index counter)
    #:type (current-mutator-type)
    (maybe-mutate stx
                  #'[]
                  mutation-index
                  counter))

  (syntax-parse stx
    [({~and arrow {~datum case->}}
      case ...)
     (mutated-do-single
      [mutated-cases (mutate-in-sequence (syntax->list #'([case] ...))
                                         mutation-index
                                         counter
                                         drop-case)]
      #:return
      (syntax-parse mutated-cases
        [({~or* [case*] []} ...)
         (syntax/loc stx
           (arrow {~? case*} ...))]))]
    [else
     (no-mutation stx mutation-index counter)]))

(module+ test
  (test-begin
    #:name drop-case->-cases
    (test-mutator* drop-case->-cases
                   #'(case-> (-> A B R)
                             (-> A B C R2)
                             (-> A B C D R2))
                   (list #'(case-> (-> A B C R2)
                                   (-> A B C D R2))
                         #'(case-> (-> A B R)
                                   (-> A B C D R2))
                         #'(case-> (-> A B R)
                                   (-> A B C R2))
                         #'(case-> (-> A B R)
                                   (-> A B C R2)
                                   (-> A B C D R2))))
    (test-mutator* drop-case->-cases
                   #'(Number Any . -> . String)
                   (list #'(Number Any . -> . String)))
    (test-mutator* drop-case->-cases
                   #'Number
                   (list #'Number))
    (test-mutator* drop-case->-cases
                   #'(Class ClassTop (field [a Ta]))
                   (list #'(Class ClassTop (field [a Ta]))))))

(define mutate-type
  (compose-mutators known-type-generalization
                    known-type-restriction
                    rearrange-type-positions
                    drop-ho-function-args
                    drop-union-branch
                    drop-class-methods
                    swap-class-implements
                    make-optional-args-mandatory
                    replace-with-Any
                    drop-case->-cases))

(define (mutate-type-if-inside-annotation stx mutation-index [counter 0])
  (define mutate (if (currently-inside-type-annotation?)
                     mutate-type
                     no-mutation))
  (mutate stx mutation-index counter))

(define active-mutator-names
  '("known-type-generalization"
    "known-type-restriction"
    "position-swap"
    "drop-ho-function-arg"
    "drop-union-branch"
    "class:drop-method"
    "class:swap-implements"
    "optional-args-mandatory"
    "replace-with-Any"
    "drop-case->-case"))

(define currently-inside-type-annotation? (make-parameter #f))
(define (select-type-annotation-exprs stx)
  (syntax-parse stx
    [({~and : {~datum :}} name:id {~datum :} arg-T ... arrow:Arrow res-T ...)
     (select-type-annotation-exprs
      (syntax/loc stx
        (: name (arrow arg-T ... res-T ...))))]
    [({~and colon/ann/cast
            {~or* {~datum :}
                  {~datum ann}
                  {~datum cast}}}
      e T)
     (list (attribute T)
           (λ (mutated-T)
             (quasisyntax/loc stx
               (colon/ann/cast e #,mutated-T)))
           (list (cons currently-inside-type-annotation? #t)))]
    [({~and inst {~datum inst}} e T ...)
     (list #'{T ...}
           (syntax-parser
             [{mutated-T ...}
              (syntax/loc stx
                (inst e mutated-T ...))]
             [Any
              (syntax/loc stx
                (inst e Any))])
           (list (cons currently-inside-type-annotation? #t)))]
    [other
     (list stx
           identity
           empty)]))

(define (mutate-benchmark module-body-stxs
                          mutation-index
                          #:program [the-program #f]
                          #:top-level-select
                          [top-level-selector select-type-annotations+define-body]
                          #:expression-select
                          [expression-selector select-type-annotation-exprs]
                          #:transform [transformer identity])
  (define mutate-expr
    (make-expr-mutator mutate-type-if-inside-annotation
                       #:select expression-selector))
  (define mutate-program
    (transformer
     (make-program-mutator mutate-expr
                           #:select top-level-selector)))
  (mutate-program module-body-stxs mutation-index))

(module+ test
  (define mutate-syntax (curry mutate-benchmark #:transform syntax-only))
  (define-test (test-mutation index orig-prog new-prog)
    (define mutated (mutate-syntax orig-prog index))
    (if (equal? mutated no-more-mutations-flag)
        (fail "Mutation index exceeded")
        (test-programs-equal?
         mutated
         new-prog)))
  (define-test (test-mutation/sequence orig-program expects)
    (for/and/test ([expect (in-list expects)])
                  (match-define (list mutation-index expected) expect)
                  (define result (test-mutation mutation-index
                                                orig-program
                                                expected))
                  (extend-test-message result
                                       " (mutation index: ~v)"
                                       mutation-index)))
  (test-begin
    #:name mutate-benchmark
    ; ho ->
    (test-mutation/sequence
     #'{(: f (-> (Any . -> . Any) Any))}
     `(#;[0 ,#'{(: f ((Any . -> . Any) . -> . Any))}]
       [0 ,#'{(: f (-> Any (Any . -> . Any)))}]
       [1 ,#'{(: f ((-> Any) . -> . Any))}]
       [2 ,#'{(: f Any)}]))
    ; U
    (test-mutation/sequence
     #'{(: f (U String
                (String . -> . String)
                False))}
     `(#;[0 ,#'{(: f (U String
                      (String . -> . String)
                      False))}]
       [0 ,#'{(: f (U (String . -> . String)
                      String
                      False))}]
       [1 ,#'{(: f (U (String . -> . String)
                      False))}]
       [2 ,#'{(: f (U String
                      False))}]
       [3 ,#'{(: f (U String
                      (String . -> . String)))}]
       [4 ,#'{(: f Any)}]
       [5 ,#'{(: f (Any String
                      (String . -> . String)
                      False))}]))
    ; classes
    (test-mutation/sequence
     #'{(: f (Class #:implements Nothing
                    (field [a Ta])
                    [m (->* {String} {Number} String)]))}
     `(#;[0 ,#'{(: f (Class #:implements Nothing
                          (field [a Ta])
                          [m (->* {String} {Number} String)]))}]
       [0 ,#'{(: f (Class Nothing #:implements
                          (field [a Ta])
                    [m (->* {String} {Number} String)]))}]
       [1 ,#'{(: f (Class #:implements Nothing
                          [m (->* {String} {Number} String)]
                          (field [a Ta])))}]
       [2 ,#'{(: f (Class #:implements Nothing
                          (field [a Ta])))}]
       [3 ,#'{(: f (Class #:implements ClassTop
                          (field [a Ta])
                          [m (->* {String} {Number} String)]))}]))
    ; ->* mandatory args
    (test-mutation/sequence
     #'{(: f (->* {A B} {C D} Any))}
     `(#;[0 ,#'{(: f (->* {A B} {C D} Any))}]
       [0 ,#'{(: f (->* {C D} {A B} Any))}]
       [1 ,#'{(: f (->* {A B C D} Any))}]
       [2 ,#'{(: f Any)}]))
    ; case->
    (test-mutation/sequence
     #'{(: f (case-> (Any . -> . Any)
                     (Any Any . -> . Any)))}
     `([0 ,#'{(: f (case-> (Any Any . -> . Any)
                           (Any . -> . Any)))}]
       [1 ,#'{(: f Any)}]
       [2 ,#'{(: f (case-> (Any Any . -> . Any)))}]
       [3 ,#'{(: f (case-> (Any . -> . Any)))}]
       [4 ,#'{(: f (Any (Any . -> . Any)
                        (Any Any . -> . Any)))}]
       [5 ,#'{(: f (case-> Any
                           (Any Any . -> . Any)))}]
       [6 ,#'{(: f (case-> (Any . Any . Any)
                           (Any Any . -> . Any)))}]
       ;; ...
       ))
    ; different types of annotations
    (test-mutation/sequence
     #'{(define x (list (ann f Integer)))}
     `([0 ,#'{(define x (list (ann f Real)))}]
       [1 ,#'{(define x (list (ann f Index)))}]
       [2 ,#'{(define x (list (ann f Any)))}]))
    (test-mutation/sequence
     #'{(define x (list (cast f Integer)))}
     `([0 ,#'{(define x (list (cast f Real)))}]
       [1 ,#'{(define x (list (cast f Index)))}]
       [2 ,#'{(define x (list (cast f Any)))}]))
    (test-mutation/sequence
     #'{(define x (list (inst f Integer Boolean)))}
     `([0 ,#'{(define x (list (inst f Any)))}]
       [1 ,#'{(define x (list (inst f Real Boolean)))}]
       [2 ,#'{(define x (list (inst f Index Boolean)))}]
       [3 ,#'{(define x (list (inst f Any Boolean)))}]
       [4 ,#'{(define x (list (inst f Integer Any)))}])))


  (test-begin
    #:name mutate-benchmark/traversal
    (ignore (define (define/function-syntax body)
              #`(define (f x) . #,body))
            (define (define/value-syntax body)
              #`(define f (λ (x) . #,body))))
    (for/and/test
     ([define-f-with-body (in-list (list define/function-syntax
                                         define/value-syntax))])
     (define p
       #`{(: f : Number -> Integer)
          #,(define-f-with-body
              #'[(: y String)
                 (define y (number->string x))
                 (: y-l Integer)
                 (define y-l (string-length y))
                 (+ x y-l)])
          (f 78)})
     (extend-test-message
      (and/test
       (test-mutation/sequence p
                               `([0 ,#`{(: f (-> Integer Number))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [1 ,#`{(: f Any)
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [2 ,#`{(: f (Any Number Integer))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [3 ,#`{(: f (-> Real Integer))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [4 ,#`{(: f (-> Any Integer))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [5 ,#`{(: f (-> Number Real))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [6 ,#`{(: f (-> Number Index))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [7 ,#`{(: f (-> Number Any))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [8 ,#`{(: f (-> Number Integer))
                                        #,(define-f-with-body
                                            #'[(: y Any)
                                               (define y (number->string x))
                                               (: y-l Integer)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [9 ,#`{(: f (-> Number Integer))
                                        #,(define-f-with-body
                                            #'[(: y String)
                                               (define y (number->string x))
                                               (: y-l Real)
                                               (define y-l (string-length y))
                                               (+ x y-l)])
                                        (f 78)}]
                                 [10 ,#`{(: f (-> Number Integer))
                                         #,(define-f-with-body
                                             #'[(: y String)
                                                (define y (number->string x))
                                                (: y-l Index)
                                                (define y-l (string-length y))
                                                (+ x y-l)])
                                         (f 78)}]
                                 [11 ,#`{(: f (-> Number Integer))
                                         #,(define-f-with-body
                                             #'[(: y String)
                                                (define y (number->string x))
                                                (: y-l Any)
                                                (define y-l (string-length y))
                                                (+ x y-l)])
                                         (f 78)}]))
       (equal? (mutate-syntax p 12) no-more-mutations-flag))
      @~a{ (define-f-with-body: @(object-name define-f-with-body))}))))
