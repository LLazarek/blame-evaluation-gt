#lang at-exp racket/base

(provide (struct-out exn:fail:experiment:error)
         (struct-out exn:fail:experiment:argument-error)
         (struct-out exn:fail:experiment:user-error)
         raise-internal-experiment-error
         raise-internal-experiment-argument-error
         raise-experiment-user-error
         experiment-internal-error?)

(require racket/format)

(struct exn:fail:experiment:error exn:fail ())
(struct exn:fail:experiment:argument-error exn:fail:contract ())
(struct exn:fail:experiment:user-error exn:fail:user ())

(define (raise-internal-experiment-argument-error who expected got)
  (raise
   (exn:fail:experiment:argument-error
     @~a{
         @|who|: contract violation
           expected: @expected
           given: @~e[got]
           }
     (current-continuation-marks))))
(define (raise-internal-experiment-error who msg)
  (raise (exn:fail:experiment:error (~a who ": " msg)
                                    (current-continuation-marks))))
(define (raise-experiment-user-error who msg)
  (raise (exn:fail:experiment:user-error (~a who ": " msg)
                                         (current-continuation-marks))))

(define (experiment-internal-error? e)
  (or (exn:fail:experiment:argument-error? e)
      (exn:fail:experiment:user-error? e)
      (exn:fail:experiment:error? e)))
