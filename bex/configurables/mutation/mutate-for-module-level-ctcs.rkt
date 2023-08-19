#lang at-exp racket/base

(provide mutate-benchmark
         active-mutator-names)

(require racket/function
         syntax/parse
         mutate/define
         mutate/low-level
         mutate/traversal
         (only-in mutate/mutators/code comparison-op-swap begin-drop)
         "mutators.rkt")

(define active-mutators
  (list
   ;; arithmetic
   arithmetic-op-swap
   ;; logical
   boolean-op-swap
   ;; hide-method
   class-method-publicity-swap
   ;; constants
   replace-constants
   ;; relational
   comparison-op-swap
   ;; conditional
   negate-conditionals
   ;; statement
   begin-drop
   ;; argument
   rearrange-positional-exprs))

(define active-mutator-names
  (map mutator-type active-mutators))

(define (mutate-benchmark module-body-stxs
                          mutation-index
                          #:program [the-program #f]
                          #:top-level-select
                          [top-level-selector select-define-body]
                          #:expression-select
                          [expression-selector select-exprs-as-if-untyped]
                          #:transformer [transform identity])
  (define mutate-expr
    (make-expr-mutator
     (apply compose-mutators active-mutators)
     #:select expression-selector))
  (define mutate-program
    (transform
     (make-program-mutator mutate-expr
                           #:select top-level-selector)))
  (mutate-program module-body-stxs mutation-index))
