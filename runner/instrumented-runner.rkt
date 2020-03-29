#lang at-exp racket

(provide make-instrumented-runner
         (struct-out mod)
         (struct-out program)
         mod/c
         program/c
         mod->name)

(require custom-load
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         "modgraph.rkt"
         "../util/path-utils.rkt")

(struct mod (path stx)
  #:transparent
  #:methods gen:equal+hash
  {(define (equal-proc m1 m2 recursive-equal?)
     (equal? (mod-path m1)
             (mod-path m2)))
   (define (hash-proc m recursive-hash)
     (recursive-hash (mod-path m)))
   (define (hash2-proc m recursive-hash)
     (match (mod-path m)
       [(? string? s) (string-length s)]
       [(? path? p) (string-length (path->string p))]
       [else 0]))})
(struct program (main others)
  #:transparent)

(define mod/c (struct/c mod path-string? syntax?))
(define (program-main-not-in-others? a-program)
  (not (member (program-main a-program)
               (program-others a-program))))
(define program/c (and/c (struct/c program mod/c (listof mod/c))
                         program-main-not-in-others?))

(define (mod->name m)
  (file-name-string-from-path (mod-path m)))

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(struct instrumented-module
  (path-string module-path file-path containing-directory stx))

(define/contract (make-instrumented-runner a-program
                                           instrument-module
                                           #:setup-namespace
                                           [setup-namespace! (λ _ (void))]
                                           #:before-main
                                           [do-before-main! (λ _ (void))]
                                           #:make-result
                                           [make-result (λ (ns r) r)])
  (->i ([a-program program/c]
        [instrument-module (mod/c . -> . syntax?)])
       (#:setup-namespace [setup-namespace! (namespace? . -> . void?)]
        #:before-main [do-before-main! (namespace? . -> . any)]
        #:make-result [make-result (namespace? any/c . -> . any/c)])

       [result (-> any)])

  (define (make-instrumented-module m)
    (define path-string/simplified (path->string (simplify-path (mod-path m))))
    (define module-path `(file ,path-string/simplified))
    (define file-path (module-path->path module-path))
    (define-values (module-containing-directory ___1 ___2)
      (split-path file-path))
    (define module-stx/instrumented (instrument-module m))
    (instrumented-module path-string/simplified
                         module-path
                         file-path
                         module-containing-directory
                         module-stx/instrumented))

  (match-define (struct* program ([main main-module]
                                  [others other-modules-to-instrument]))
    a-program)
  (define main/instrumented (make-instrumented-module main-module))
  (define others/instrumented
    (map make-instrumented-module other-modules-to-instrument))
  ;; Modules must be loaded in order such that loading one module doesn't
  ;; cause another one to be loaded before it gets instrumented
  (define others/instrumented/ordered
    (order-by-dependencies others/instrumented
                           instrumented-module-stx
                           instrumented-module-path-string))

  (define others+main/instrumented/ordered
    (append others/instrumented/ordered
            (list main/instrumented)))


  (define ns (make-base-namespace))
  (setup-namespace! ns)

  (define (run)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure
                    ;; instrumented versions are loaded
                    (make-custom-load/use-compiled
                     #:blacklist
                     (curryr member
                             (map instrumented-module-file-path
                                  others+main/instrumented/ordered)))]
                   [current-namespace ns])

      ;; Eval the instrumented modules one at a time
      (for ([m (in-list others+main/instrumented/ordered)])
        (parameterize
            ;; Ensure relative load paths work
            ([current-load-relative-directory
              (instrumented-module-containing-directory m)]
             [current-module-declare-name
              (module-path-resolve (instrumented-module-module-path m))]
             [current-directory
              (instrumented-module-containing-directory m)])
          (displayln @~a{
                         Eval'ing module inside @(instrumented-module-containing-directory m)
                         with name @(module-path-resolve (instrumented-module-module-path m))
                         with stx: @(syntax->datum (instrumented-module-stx m))
                         })
          (eval (instrumented-module-stx m))))

      ;; Run the main module
      (parameterize
          ;; Ensure relative load paths work
          ([current-load-relative-directory
            (instrumented-module-containing-directory main/instrumented)]
           [current-directory
            (instrumented-module-containing-directory main/instrumented)])
        (do-before-main! ns)
        (define result
          (eval
           `(require ,(instrumented-module-module-path main/instrumented))))

        (make-result ns result))))

  run)

(module+ test
  (require ruinit)
  (test-begin
    #:name make-instrumented-runner
    (ignore
     (define m (mod "m.rkt" #'(module m racket
                                (#%module-begin
                                 (define (f x) (* x x))
                                 (displayln (f 2))))))
     (define run-m (make-instrumented-runner
                    (program m empty)
                    mod-stx)))
    (test-equal? (with-output-to-string run-m)
                 "4\n")


    (ignore
     (define m1 (mod "m1.rkt" #'(module m1 racket
                                  (#%module-begin
                                   (require "m2.rkt")
                                   (define (f x) (* x (g x)))
                                   (displayln (f 2))))))
     (define m2 (mod "m2.rkt" #'(module m2 racket
                                  (#%module-begin
                                   (provide g)
                                   (require "m3.rkt")
                                   (displayln 'm2)
                                   (define (g x) (+ (* x x) n))))))
     (define m3 (mod "m3.rkt" #'(module m3 racket
                                  (#%module-begin
                                   (provide n)
                                   (displayln 'm3)
                                   (define n 42)))))
     (define run-m1 (make-instrumented-runner
                     (program m1 (list m2 m3))
                     mod-stx)))
    (test-equal? (with-output-to-string run-m1)
                 @~a{
                     m3
                     m2
                     92
                     
                     }))

  (test-begin
    #:name mod-equality
    (test-equal? (mod "foobar.rkt" #'(1 2 3))
                 (mod "foobar.rkt" #'(1 2 3)))
    (test-equal? (mod "foobar.rkt" #'(1 2 3))
                 (mod "foobar.rkt" #'(1 2 3 4 5)))
    (not/test
     (test-equal? (mod "foobar.rkt" #'(1 2 3))
                  (mod "something-else.rkt" #'(1 2 3))))))

