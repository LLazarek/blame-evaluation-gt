#lang at-exp racket/base

(require "../util/optional-contracts.rkt"
         (except-in racket/contract/base
                    contract-out))

(provide (contract-out
          [expression-selector/c contract?]
          [select-any-expr expression-selector/c]
          [select-exprs-as-if-untyped expression-selector/c]))

(require racket/bool
         racket/format
         racket/function
         racket/list
         syntax/parse)

(define expression-selector/c
  (syntax?
   . -> .
   (or/c #f
         (list/c syntax?
                 (syntax? . -> . syntax?)))))

(define (select-any-expr expr)
  (list expr
        identity))

;; If `expr` contains no type annotation subexprs, select the expr as-is.
;; If `expr` contains type annotations, select all subexprs of `expr` not
;; associated with the type annotations; the reconstructor puts the annotations
;; back in.
;;
;; ASSUMPTION: the reconstructor assumes that the syntax it receives will have
;; the same number of subexprs as the selected syntax.
;;
;; Example:
;; (+ 2 (apply (λ (x) x) 42))
;;   selects everything, and the reconstructor is identity
;; (+ 2 (apply (λ ([x : Natural]) : Natural x) 42))
;;   same as above
;; (ann (+ 2 (apply (λ ([x : Natural]) : Natural x) 42)) T)
;;   selects the inner expr, and the reconstructor puts the `(ann ... T)` back
;; (for : T (...) foobar)
;;   selects (for (...) foobar), and the reconstructor puts the `: T` back
(define (select-exprs-as-if-untyped expr)
  (syntax-parse expr
    #:datum-literals [: ann cast inst row-inst]
    [(: . _) #f]
    [({~and the-annotation-thing
            {~or ann
                 cast
                 inst
                 row-inst}}
      e
      T ...)
     (list #'e
           (λ (new-e)
             (quasisyntax/loc expr
               (the-annotation-thing #,new-e T ...))))]
    [({~and e-1 {~not :}}
      ...+
      {~seq : T
            {~and e-i {~not :}} ...}
      ...+)
     (define e-1-count (length (attribute e-1)))
     (define e-i-counts (map length (attribute e-i)))
     (list
      #'(e-1 ... {~@ e-i ...} ...)
      (λ (mutated-stx)
        (define mutated-stx-parts (syntax->list mutated-stx))
        (unless (= (length mutated-stx-parts)
                   (apply + (cons e-1-count e-i-counts)))
          (raise-user-error
           'select-exprs-as-if-untyped
           @~a{
               Assumption violated: @;
               Mutated syntax given to reconstructor contains @;
               a different number of subexprs as original stx. @;
               Original @(length mutated-stx-parts): @expr
               Mutated  @(apply + (cons e-1-count e-i-counts)): @mutated-stx
               }))
        (define-values {mutated-e-1s remaining-stx-parts}
          (split-at mutated-stx-parts e-1-count))
        (define mutated-e-is
          (for/fold ([remaining-stx-parts remaining-stx-parts]
                     [mutated-e-is empty]
                     #:result (reverse mutated-e-is))
                    ([e-i-count (in-list e-i-counts)])
            (define-values {e-is now-remaining-stx}
              (split-at remaining-stx-parts e-i-count))
            (values now-remaining-stx
                    (cons e-is mutated-e-is))))
        (with-syntax ([[mutated-e-1 ...] mutated-e-1s]
                      [[[mutated-e-i ...] ...] mutated-e-is])
          (syntax/loc expr
            (mutated-e-1 ... {~@ : T mutated-e-i ...} ...)))))]
    [{~or* atom
           ({~and e-1 {~not :}} ...)}
     #:when (or (not (attribute atom))
                (false? (syntax->list #'atom)))
     (list this-syntax
           identity)]
    [other
     (error 'select-exprs-as-if-untyped
            @~a{Syntax @#'other doesn't match any patterns.})]))

(module+ test
  (require ruinit
           racket
           "mutate-test-common.rkt")
  (define-test (test-selector selector
                              stx
                              expected)
    (define result (selector stx))
    (match* {result expected}
      [{(list new-stx reconstructor) (not #f)}
       (and/test (test-programs-equal? new-stx expected)
                 (test-programs-equal? (reconstructor new-stx) stx))]
      [{(not #f) #f}
       (fail @~a{Selector matches with result: @result})]
      [{#f (not #f)}
       (fail @~a{Selector does not match when it should.})]
      [{#f #f} #t]))
  (test-begin
    #:name select-exprs-as-if-untyped
    (test-selector select-exprs-as-if-untyped
                   #'x
                   #'x)
    (test-selector select-exprs-as-if-untyped
                   #'42
                   #'42)
    (test-selector select-exprs-as-if-untyped
                   #'()
                   #'())
    (test-selector select-exprs-as-if-untyped
                   #'(: a T)
                   #f)
    (test-selector select-exprs-as-if-untyped
                   #'(ann x T)
                   #'x)
    (test-selector select-exprs-as-if-untyped
                   #'(cast x T)
                   #'x)
    (test-selector select-exprs-as-if-untyped
                   #'(inst x T1 T2)
                   #'x)
    (test-selector select-exprs-as-if-untyped
                   #'(row-inst x T1 T2 T3)
                   #'x)
    (test-selector select-exprs-as-if-untyped
                   #'(f a b 42 c)
                   #'(f a b 42 c))
    (test-selector select-exprs-as-if-untyped
                   #'[a : Natural 42]
                   #'[a 42])
    (test-selector select-exprs-as-if-untyped
                   #'(λ ([x : T]) : R (+ 2 2))
                   #'(λ ([x : T]) (+ 2 2)))
    (test-selector select-exprs-as-if-untyped
                   #'(for : T ([v : Boolean (in-list bools)])
                          (displayln v))
                   #'(for ([v : Boolean (in-list bools)])
                       (displayln v)))
    (test-selector select-exprs-as-if-untyped
                   #'(define (f [x : T]) : R (+ x 2))
                   #'(define (f [x : T]) (+ x 2))))

  (test-begin
    #:name select-exprs-as-if-untyped/reconstructor
    (ignore (match-define (list selected reconstruct)
              (select-exprs-as-if-untyped #'(+ 2 2))))
    (test-programs-equal? (reconstruct #'(- 2 2))
                          #'(- 2 2)))

  (require "../util/for-first-star.rkt")
  (test-begin
    #:name select-exprs-as-if-untyped/random-testing
    (not (for/first* ([i (in-range 1000)])
                     (define random-stx-datum
                       (contract-random-generate (listof symbol?) 1))
                     (define stx
                       (datum->syntax #f random-stx-datum))
                     (define seems-untyped? (member ': random-stx-datum))
                     (and seems-untyped?
                          (test-fail? (test-selector select-exprs-as-if-untyped
                                                     stx
                                                     stx))
                          random-stx-datum)))))
