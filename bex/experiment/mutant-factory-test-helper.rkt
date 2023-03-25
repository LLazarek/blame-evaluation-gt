#lang at-exp racket

(require ruinit
         syntax/parse/define
         "blame-trail-data.rkt"
         "../runner/mutation-runner.rkt")
(provide test-begin/with-env)

(define rt-main-body
  @~a{
      (define (foo x)
        (if (positive? x)
            (foo (- x))
            (* x x 3)))
      (define (main)
        (bar (foo 2) "yes"))
      (main)
      })
(define rt-second-body
  @~a{
      (provide bar)
      (define (bar x s)
        (if (positive? x)
            (- x)
            x))
      })
(define-test-env (setup-test-env! cleanup-test-env!)
  #:directories ([test-mutant-dir (simplify-path "./test-mutants")]
                 [test-bench (simplify-path "./test-bench")]
                 [test-bench/ut (simplify-path "./test-bench/untyped")]
                 [test-bench/t (simplify-path "./test-bench/typed")] ; empty

                 [realistic-test-bench (simplify-path "./real-test-bench")]
                 [realistic-test-bench/ut (simplify-path "./real-test-bench/untyped")]
                 [realistic-test-bench/t (simplify-path "./real-test-bench/typed")]
                 [realistic-test-bench/base (simplify-path "./real-test-bench/base")])
  #:files ([e-path (build-path test-bench/ut "e.rkt")
                   @~a{
                       #lang racket

                       (provide baz)

                       (define (baz x y)
                         (if (even? x)
                             y
                             (/ y x)))
                       }]
           [main-path (build-path test-bench/ut "main.rkt")
                      @~a{
                          #lang typed/racket

                          (require "loop.rkt")
                          (require/typed "e.rkt"
                            [baz (-> Number Number Number)])

                          (: foo (-> Number Number))
                          (define (foo x)
                            (- x))

                          (loop)
                          (foo (baz 0 22))
                          }]
           [loop-path (build-path test-bench/ut "loop.rkt")
                      @~a{
                          #lang typed/racket
                          (provide loop)
                          (: loop (-> Number))
                          (define (loop)
                            (if #f (loop) 42))
                          }]
           [a-text-file (build-path realistic-test-bench/base "a-text-file.txt")
                        @~a{foobar baz}]
           [mutant0-path "m0.rktd"
                         (format "~s\n"
                                 (run-status "m0.rkt"
                                             0
                                             'foo
                                             'blamed
                                             '("m0.rkt")
                                             empty
                                             empty
                                             #f))]
           [mutant1-path/1 "m11.rktd"
                           (format "~s\n"
                                   (run-status "m1.rkt"
                                               0
                                               'foo
                                               'blamed
                                               '("m1.rkt")
                                               empty
                                               empty
                                               #f))]
           [mutant1-path/2 "m12.rktd"
                           (format "~s\n"
                                   (run-status "m1.rkt"
                                               42
                                               'foo
                                               'type-error
                                               '("m1.rkt")
                                               #f
                                               #f
                                               #f))]
           [mutant2-path "m2.rktd"
                         (format "~s\n"
                                 (run-status "m1.kt"
                                             42
                                             'foo
                                             'type-error
                                             '("m2.rkt")
                                             #f
                                             #f
                                             #f))]
           [empty-file-path "empty-file.rktd"
                            ""]
           [partial-e-bt-aggregate-file
            "e.rkt_N.rktd"
            @~a{
                @~s[(blame-trail-summary
                     "e.rkt"
                     1
                     42
                     (list
                      (mutant-summary 187
                                      (run-status "e.rkt"
                                                  1
                                                  'foo
                                                  'type-error
                                                  '("e.rkt")
                                                  empty
                                                  empty
                                                  #f)
                                      111
                                      #;(hash "main.rkt" 'types
                                            "e.rkt" 'types
                                            "loop.rkt" 'types))))]
                }]

           [rt-main/ut (build-path realistic-test-bench/ut
                                   "main.rkt")
                       @~a{
                           #lang racket
                           (require "second.rkt")
                           @rt-main-body
                           }]
           [rt-second/ut (build-path realistic-test-bench/ut
                                     "second.rkt")
                         @~a{
                             #lang racket
                             @rt-second-body
                             }]
           [rt-main/t (build-path realistic-test-bench/t
                                  "main.rkt")
                      @~a{
                          #lang typed/racket
                          (require require-typed-check)
                          (require/typed/check
                           "second.rkt"
                           [bar (-> Integer String Nonpositive-Integer)])
                          (: foo (-> Integer Integer))
                          (: main (-> Integer))
                          @rt-main-body
                          }]
           [rt-second/t (build-path realistic-test-bench/t
                                    "second.rkt")
                        @~a{
                            #lang typed/racket
                            (: bar (-> Integer String Nonpositive-Integer))
                            @rt-second-body
                            }]

           [config-outcomes-log "tmp--test-config-outcomes-log.rktd" ""])

  #:provide)

(define-simple-macro (test-begin/with-env #:name name
                                          e ...)
  (test-begin
    #:name name
    #:short-circuit
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    e ...))
