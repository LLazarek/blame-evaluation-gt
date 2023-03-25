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
     (raise-user-error
      'pick-some:follow-blame
      @~a{
          Got request to pick blamed locations from a run-status without any locations:
          @~s[a-run-status]
          config: @~s[program-config]
          })]))

(module+ test
  (require ruinit)
  (define (make-rs outcome
                   errortrace
                   context
                   #:blamed [blamed #f])
    (run-status "a-mod.rkt"
                42
                'mutated-id
                outcome
                blamed
                errortrace
                context
                #f))
  (parameterize ([selector:runtime-error-with-blame (const 'r/e+blame)]
                 [selector:runtime-error (const 'r/e)]
                 [selector:blame (const 'blame)])
    (test-begin
      (test-equal? (follow-blame (make-rs 'blamed
                                          #:blamed empty
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt" "main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt"))
                                 #hash(("ai.rkt" . types)
                                       ("benv.rkt" . types)
                                       ("denotable.rkt" . none)
                                       ("main.rkt" . none)
                                       ("structs.rkt" . none)
                                       ("time.rkt" . none)
                                       ("ui.rkt" . none)))
                   'blame)
      (test-equal? (follow-blame (make-rs 'runtime-error
                                          #:blamed empty
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt" "main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt"))
                                 #hash(("ai.rkt" . types)
                                       ("benv.rkt" . types)
                                       ("denotable.rkt" . none)
                                       ("main.rkt" . none)
                                       ("structs.rkt" . none)
                                       ("time.rkt" . none)
                                       ("ui.rkt" . none)))
                   'r/e+blame)
      (test-equal? (follow-blame (make-rs 'runtime-error
                                          #:blamed '("ui.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt" "main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt"))
                                 #hash(("ai.rkt" . types)
                                       ("benv.rkt" . types)
                                       ("denotable.rkt" . none)
                                       ("main.rkt" . none)
                                       ("structs.rkt" . none)
                                       ("time.rkt" . none)
                                       ("ui.rkt" . none)))
                   'r/e+blame)
      (test-equal? (follow-blame (make-rs 'runtime-error
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt" "main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt"))
                                 #hash(("ai.rkt" . types)
                                       ("benv.rkt" . types)
                                       ("denotable.rkt" . none)
                                       ("main.rkt" . none)
                                       ("structs.rkt" . none)
                                       ("time.rkt" . none)
                                       ("ui.rkt" . none)))
                   'r/e)
      (test-equal? (follow-blame (make-rs 'type-error
                                          #:blamed '("main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt" "main.rkt")
                                          '("ui.rkt" "main.rkt" "main.rkt" "main.rkt"))
                                 #hash(("ai.rkt" . types)
                                       ("benv.rkt" . types)
                                       ("denotable.rkt" . none)
                                       ("main.rkt" . none)
                                       ("structs.rkt" . none)
                                       ("time.rkt" . none)
                                       ("ui.rkt" . none)))
                   '("main.rkt"))
      (test-exn exn:fail:user?
                (follow-blame (make-rs 'completed #f #f)
                              #hash(("ai.rkt" . types)
                                    ("benv.rkt" . types)
                                    ("denotable.rkt" . none)
                                    ("main.rkt" . none)
                                    ("structs.rkt" . none)
                                    ("time.rkt" . none)
                                    ("ui.rkt" . none)))))))
