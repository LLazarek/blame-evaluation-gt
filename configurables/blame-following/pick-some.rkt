#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "blame-follower.rkt")
(provide (contract-out
          [follow-blame blame-follower/c]
          [selector:runtime-error-with-blame (parameter/c selector/c)]
          [selector:runtime-error            (parameter/c selector/c)]
          [selector:blame                    (parameter/c selector/c)]))

(require "../../runner/mutation-runner-data.rkt"
         racket/list)

;; The stack info passed to these are already filtered to just modules in the program
(define selector:runtime-error-with-blame (make-parameter #f))
(define selector:runtime-error (make-parameter #f))
(define selector:blame (make-parameter #f))

(define (follow-blame a-run-status program-config)
  (match a-run-status
    [(struct* run-status ([outcome (or 'runtime-error 'blamed)]
                          [blamed '()]))
     empty]
    [(struct* run-status ([outcome 'runtime-error]
                          [blamed (and blamed (not #f))]
                          [errortrace-stack errortrace]
                          [context-stack    context]))
     ((selector:runtime-error-with-blame) program-config blamed errortrace context)]
    [(struct* run-status ([outcome 'runtime-error]
                          [blamed #f]
                          [errortrace-stack errortrace]
                          [context-stack    context]))
     ((selector:runtime-error) program-config #f errortrace context)]
    [(struct* run-status ([outcome 'blamed]
                          [blamed blamed]
                          [errortrace-stack errortrace]
                          [context-stack    context]))
     ((selector:blame) program-config blamed errortrace context)]
    [(struct* run-status ([outcome 'type-error]
                          [blamed blamed]))
     blamed]
    [else
     (raise-user-error 'pick-some:follow-blame
                       @~a{
                           Got request to pick blamed locations from a run-status without blame:
                           @~s[a-run-status]
                           config: @~s[program-config]
                           })]))
