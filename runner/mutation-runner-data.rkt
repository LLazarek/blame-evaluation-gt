#lang at-exp racket/base

(provide (struct-out run-status)
         outcomes
         index-exceeded-outcome
         index-exceeded?
         run-status/c
         run-outcome/c
         mutated-identifier?)

(require racket/contract
         racket/math
         "../util/path-utils.rkt")

(define index-exceeded-outcome 'index-exceeded)
(define outcomes `(,index-exceeded-outcome
                   blamed
                   runtime-error
                   type-error
                   oom
                   timeout
                   syntax-error
                   completed))

(struct run-status (mutated-module
                    index
                    mutated-id
                    outcome
                    blamed
                    errortrace-stack
                    context-stack
                    result-value)
  #:prefab)

(define module-name-or-library-path?
  (or/c module-name?
        ;; This case allows the benchmarks to
        ;; blame library code, which happens for
        ;; instance in the quadU mutant
        ;; "quad-main.rkt" @ 79
        library-path?))

(define run-outcome/c (apply or/c outcomes))
(define mutated-identifier? (or/c string? symbol?))

(define run-status/c
  (struct/dc run-status
             [mutated-module    module-name?]
             [index             natural?]
             [mutated-id        {outcome}
                                (if (equal? outcome index-exceeded-outcome)
                                    #f
                                    mutated-identifier?)]
             [outcome           run-outcome/c]
             [blamed            {outcome}
                                (cond [(member outcome '(blamed type-error))
                                       (listof module-name-or-library-path?)]
                                      [(equal? outcome 'runtime-error)
                                       ;; Some runtime errors come with blame, if the
                                       ;; primitive has a real contract,
                                       ;; and type errors identify a location too
                                       (or/c (listof module-name-or-library-path?) #f )]
                                      [else #f])]
             [errortrace-stack  {outcome}
                                (if (member outcome '(blamed runtime-error))
                                    (listof module-name-or-library-path?)
                                    #f)]
             [context-stack     {outcome}
                                (if (member outcome '(blamed runtime-error))
                                    (listof module-name-or-library-path?)
                                    #f)]
             [result-value      any/c]))

;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) index-exceeded-outcome))
