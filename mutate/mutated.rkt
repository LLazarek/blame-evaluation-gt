#lang racket

(provide mutation-index?
         counter?
         mutated/c
         mmap
         mbind
         mdo
         mdo*
         (struct-out mutated)
         (struct-out mutated-program)
         mutation-applied-already?)

(require (for-syntax syntax/parse))

(define mutation-index? natural?)
(define counter? natural?)

;; mutated represents a piece of syntax that has been considered for
;; mutation; it may or may not have actually been mutated.
;; It carries with it the state of the mutation search _after_ its
;; consideration for mutation.
;; Thus, all mutation attempts after some mutated object should
;; use its accompanying counter value.
(struct mutated (stx new-counter) #:transparent)

(define (mutated/c a) (struct/c mutated a counter?))

;; lltodo: use parametric->/c?
(define A any/c)
(define B any/c)
(define C any/c)
;; maps over mutated-stx
(define/contract (mmap f m)
  ((A . -> . B)
   (mutated/c A)
   . -> .
   (mutated/c B))

  (mutated (f (mutated-stx m))
           (mutated-new-counter m)))

(define/contract (mbind f m)
  ((A counter? . -> . (mutated/c B))
   (mutated/c A)
   . -> .
   (mutated/c B))

  (f (mutated-stx m)
     (mutated-new-counter m)))

;; Performs sequential mutations with automatic threading of the
;; counter
(define-syntax (mdo stx)
  (syntax-parse stx
    #:datum-literals (count-with def return in def/value)
    [(_ [count-with (counter-id:id current-value:expr)]
        (def bound-id:expr m-expr:expr)
        more-clauses ...+)
     #'(match-let* ([counter-id current-value]
                    [(mutated bound-id counter-id) m-expr])
         (mdo [count-with (counter-id counter-id)]
              more-clauses ...))]
    [(_ [count-with (counter-id:id current-value:expr)]
        (def/value bound-id:expr non-m-expr:expr)
        more-clauses ...+)
     #'(match-let* ([bound-id non-m-expr])
         (mdo [count-with (counter-id current-value)]
              more-clauses ...))]
    ;; return <=> mmap
    [(_ [count-with (counter-id:id current-value:expr)]
        [return m-expr:expr])
     #'(mutated m-expr
                counter-id)]
    ;; in <=> mbind
    [(_ [count-with (counter-id:id current-value:expr)]
        [in m-expr:expr])
     #'m-expr]))

;; bind version that shows more clearly how this is similar to monadic do
;; but it doesn't simply support pattern matching for id like above..
#;(define-syntax (mdo stx)
  (syntax-parse stx
    #:datum-literals (count-with def return in)
    [(_ [count-with (counter-id:id current-value:expr)]
        (def bound-id:id m-expr:expr)
        more-clauses ...+)
     #'(mbind (λ (bound-id counter-id)
                (mdo [count-with (counter-id counter-id)]
                     more-clauses ...))
              (let ([counter-id current-value])
                m-expr))]
    ;; return <=> mmap
    [(_ [count-with (counter-id:id current-value:expr)]
        [return m-expr:expr])
     #'(mutated m-expr
                counter-id)]
    ;; in <=> mbind
    [(_ [count-with (counter-id:id current-value:expr)]
        [in m-expr:expr])
     #'m-expr]))

(define-syntax-rule (mdo* def-clause result-clause)
  (mdo [count-with (unused #f)]
       def-clause
       result-clause))

(struct mutated-program (stx mutated-id) #:transparent)

(define (mutation-applied-already? mutation-index counter)
  (> counter mutation-index))
