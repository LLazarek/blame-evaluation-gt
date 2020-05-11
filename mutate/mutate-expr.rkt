#lang at-exp racket

(provide (contract-out
          [make-expr-mutator ({mutator/c}
                              {#:filter (syntax? . -> . boolean?)}
                              . ->* .
                              mutator/c)]
          [mutation-guard    (syntax? . -> . syntax?)]
          [mutation-guarded? (syntax? . -> . boolean?)]))

(require "mutator-lib.rkt"
         "mutated.rkt"
         "mutate-util.rkt"
         syntax/parse)

(define stx-prop:mutation-guarded? 'mutation-guarded?)
(define (mutation-guard stx)
  (syntax-property stx stx-prop:mutation-guarded? #t))
(define (mutation-guarded? stx)
  (syntax-property stx stx-prop:mutation-guarded?))

(define (make-expr-mutator mutator
                          #:filter [can-mutate? (const #t)])
  (define (should-mutate? expr)
    (and (not (mutation-guarded? expr))
         (can-mutate? expr)))

  (define (mutate-expr stx mutation-index counter)
    (cond [(and (<= counter mutation-index)
                (should-mutate? stx))
           (mdo (count-with [__ counter])
                (def outer-level-mutated-stx (mutator stx mutation-index __))
                (def result
                  (cond [(and (compound-expr? outer-level-mutated-stx)
                              (should-mutate? outer-level-mutated-stx))
                         (mdo* (def inner-parts-mutated-stx-split
                                 (mutate-in-seq (syntax-e outer-level-mutated-stx)
                                                mutation-index
                                                __
                                                mutate-expr))
                               [return
                                (datum->syntax stx
                                               inner-parts-mutated-stx-split
                                               stx
                                               stx)])]
                        [else (no-mutation outer-level-mutated-stx
                                           mutation-index
                                           __)]))
                [return result])]
          [else
           (no-mutation stx mutation-index counter)]))

  mutate-expr)

(module+ test
  (require ruinit
           "mutate-test-common.rkt")
  (test-begin
    #:name make-expr-mutator
    (ignore
     (define-value-mutator replace-any-datum-with-0
       #:type "test"
       #:bind-value value
       [(not (? list?)) #:-> 0])
     (define just-replace-any-datum-with-0
       (make-expr-mutator replace-any-datum-with-0)))
    (test-mutator* just-replace-any-datum-with-0
                   #'(begin 1 2 3)
                   (list #'(0 1 2 3)
                         #'(begin 0 2 3)
                         #'(begin 1 0 3)
                         #'(begin 1 2 0)
                         #'(begin 1 2 3)))
    (test-mutator* just-replace-any-datum-with-0
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (0 x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define 0 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 0)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (0 x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ 0 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 0))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42))))

    (ignore
     (define replace-datums-not-under-define-with-0
       (make-expr-mutator replace-any-datum-with-0
                          #:filter (syntax-parser
                                     [({~datum define} . _) #f]
                                     [else #t]))))
    (test-mutator* replace-datums-not-under-define-with-0
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (0 x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ 0 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 0))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42)))))

  (test-begin
    #:name make-mutate-expr/guarding
    (ignore
     (define replace-head-of-exprs-with-0-and-prevent-recur
       (make-expr-mutator
        (λ (stx mutation-index counter)
          (syntax-parse stx
            [(head . rest)
             (mdo* (def mutated-head (maybe-mutate #'head
                                                   #'0
                                                   mutation-index
                                                   counter))
                   [return
                    (mutation-guard #`(#,mutated-head . rest))])]
            [other #'other])))))
    (test-mutator* replace-head-of-exprs-with-0-and-prevent-recur
                   #'(#%module-begin
                      (define x 5)
                      (+ x 42))
                   (list #'(0
                            (define x 5)
                            (+ x 42))
                         #'(#%module-begin
                            (define x 5)
                            (+ x 42))))))
