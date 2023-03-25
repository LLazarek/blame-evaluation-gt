#lang at-exp racket

(provide make-instrumented-runner
         exn:fail:runner:module-evaluation?
         exn:fail:runner:module-evaluation-error
         exn:fail:runner:runtime?
         exn:fail:runner:runtime-error

         exn:fail:runner-wrapped?
         exn:fail:runner-unwrap

         run-with:require

         (struct-out resolved-module)
         mod->resolved-module)

(require custom-load
         (only-in syntax/modresolve [resolve-module-path module-path->path])
         "modgraph.rkt"
         "../util/program.rkt"
         "../util/path-utils.rkt"
         "../util/optional-contracts.rkt"
         "../configurables/configurables.rkt")

(define (module-path-resolve mod-path [load? #f])
  ((current-module-name-resolver) mod-path #f #f load?))

(struct resolved-module (path-string module-path file-path containing-directory stx)
  #:transparent)

;; Requires `main-mod-path` and then `(submod main-mod-path main)`, if it has a main submodule
(define (run-with:require main-mod-path)
  (define mod-res (eval `(require ,main-mod-path)))
  (define has-main-submod?
    (eval `(module-declared? '(submod ,main-mod-path main))))
  (if has-main-submod?
      (eval `(require (submod ,main-mod-path main)))
      mod-res))

;; Module evaluation is basically compilation. Not even top level forms run
;; until the runtime phase.
(struct exn:fail:runner:module-evaluation exn:fail (error))
(struct exn:fail:runner:runtime exn:fail (error))

(define (mod->resolved-module m)
  (define path-string/simplified (path->string (simplify-path (mod-path m))))
  (define module-path `(file ,path-string/simplified))
  (define file-path (module-path->path module-path))
  (define-values (module-containing-directory ___1 ___2)
    (split-path file-path))
  (resolved-module path-string/simplified
                   module-path
                   file-path
                   module-containing-directory
                   (mod-stx m)))

(define ((write-module-to-disk! base-path write-to-dir on-module-exists) a-mod)
  (define path (resolved-module-path-string a-mod))
  (define rel-path (find-relative-path base-path path))
  (define new-path (simple-form-path (build-path write-to-dir rel-path)))
  (make-parent-directory* new-path)
  (call-with-output-file new-path #:exists on-module-exists
    (λ (out) (pretty-write (syntax->datum (resolved-module-stx a-mod)) out))))

;; The returned thunk will throw one of the above exceptions if `a-program`
;; raises an exception in the process of begin evaluated.
;; `exn:fail:runner:module-evaluation?` is raised if just evaluating the modules
;; throws an exception.
;; `exn:fail:runner:runtime?` is raised if calling `run-main` raises an exception.
(define/contract (make-instrumented-runner a-program
                                           instrument-module
                                           #:setup-namespace
                                           [setup-namespace! (λ _ (void))]
                                           #:before-main
                                           [do-before-main! (λ _ (void))]
                                           #:make-result
                                           [make-result (λ (ns r) r)]
                                           #:run-with
                                           [run-main run-with:require]

                                           #:modules-base-path
                                           [base-path #f]
                                           #:write-modules-to
                                           [write-to-dir #f]
                                           #:on-module-exists
                                           [on-module-exists 'error])
  (->i ([a-program program/c]
        [instrument-module (mod/c . -> . syntax?)])
       (#:setup-namespace [setup-namespace! (namespace? . -> . void?)]
        #:before-main [do-before-main! (namespace? . -> . any)]
        #:make-result [make-result (namespace? any/c . -> . any/c)]
        #:run-with [run-main ((list/c 'file path-string?) . -> . any/c)]

        #:modules-base-path [base-path (or/c simple-form-path? #f)]
        #:write-modules-to [write-to-dir (or/c path-string? #f)]
        #:on-module-exists [on-module-exists (or/c 'error 'replace)])

       #:pre/desc {base-path write-to-dir}
       (or (not (and write-to-dir
                     (not (unsupplied-arg? write-to-dir))
                     (or (not base-path)
                         (unsupplied-arg? base-path))))
           "must specify #:modules-base-path if #:write-modules-to is specified")

       [result (-> any)])

  (define (make-instrumented-module m)
    (define instrumented-mod (mod (mod-path m)
                                  (instrument-module m)))
    (mod->resolved-module instrumented-mod))

  (define instrument-program (configured:instrument-program))
  (match-define (program main/instrumented others/instrumented)
    (instrument-program a-program
                        make-instrumented-module
                        #;(match-lambda
                            [(program main-module other-modules-to-instrument)
                             (define main/instrumented (make-instrumented-module main-module))
                             (define others/instrumented
                               (map make-instrumented-module other-modules-to-instrument))
                             (program main/instrumented others/instrumented)])))
  ;; Modules must be loaded in order such that loading one module doesn't
  ;; cause another one to be loaded before it gets instrumented
  (define others/instrumented/ordered
    (order-by-dependencies others/instrumented
                           resolved-module-stx
                           resolved-module-path-string))

  (define others+main/instrumented/ordered
    (append others/instrumented/ordered
            (list main/instrumented)))

  (when write-to-dir
    (for-each (write-module-to-disk! base-path write-to-dir on-module-exists)
              others+main/instrumented/ordered))

  (define ns (make-base-namespace))
  (setup-namespace! ns)

  (define (run)
    (parameterize ([current-load/use-compiled
                    ;; Prevent loading from bytecode to ensure
                    ;; instrumented versions are loaded
                    (make-custom-load/use-compiled
                     #:blacklist
                     (curryr member
                             (map resolved-module-file-path
                                  others+main/instrumented/ordered)))]
                   [current-namespace ns])

      ;; Eval the instrumented modules one at a time
      (with-handlers ([exn? (λ (e)
                              (raise
                               (exn:fail:runner:module-evaluation "Module evaluation exception"
                                                                  (current-continuation-marks)
                                                                  e)))])
        (for ([m (in-list others+main/instrumented/ordered)])
          (parameterize
              ;; Ensure relative load paths work
              ([current-load-relative-directory
                (resolved-module-containing-directory m)]
               [current-module-declare-name
                (module-path-resolve (resolved-module-module-path m))]
               [current-directory
                (resolved-module-containing-directory m)])
            (eval (resolved-module-stx m)))))

      ;; Run the main module
      (with-handlers ([exn? (λ (e)
                              (raise
                               (exn:fail:runner:runtime "Runtime exception"
                                                        (current-continuation-marks)
                                                        e)))])
        (parameterize
            ;; Ensure relative load paths work
            ([current-load-relative-directory
              (resolved-module-containing-directory main/instrumented)]
             [current-directory
              (resolved-module-containing-directory main/instrumented)])
          (do-before-main! ns)
          (define result (run-main (resolved-module-module-path main/instrumented)))

          (make-result ns result)))))

  run)

(define (exn:fail:runner-unwrap wrapped-e)
  (match wrapped-e
    [(? exn:fail:runner:module-evaluation?)
     (exn:fail:runner:module-evaluation-error wrapped-e)]
    [(? exn:fail:runner:runtime?)
     (exn:fail:runner:runtime-error wrapped-e)]))
(define (exn:fail:runner-wrapped? inner-exn-pred)
  (λ (wrapped-e)
    (match wrapped-e
      [(or (? exn:fail:runner:module-evaluation?)
           (? exn:fail:runner:runtime?))
       (inner-exn-pred (exn:fail:runner-unwrap wrapped-e))]
      [else #f])))

(module+ test
  (require ruinit
           racket/runtime-path)
  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (install-configuration! test-config)
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
     (define m (mod "m.rkt" #'(module m racket
                                (#%module-begin
                                 (define (f x) (* x x))
                                 (module+ main (displayln (f 2)))))))
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
    #:name exns
    (ignore
     (define m/type-error
       (mod "m.rkt"
            #'(module m typed/racket
                (#%module-begin
                 (: x Number)
                 (define x "hello")))))
     (define m/runtime-error
       (mod "m.rkt"
            #'(module m racket
                (#%module-begin
                 (+ 2 "3")))))
     (define m/segfault
       (mod "m.rkt"
            #'(module m typed/racket
                (#%module-begin
                 (module A typed/racket
                   (provide f)
                   (: f : String -> Number)
                   (define (f x)
                     (string-length x)))
                 (require/typed 'A
                   [f (-> Number Number)])
                 (f 5))))))

    (test-exn (λ (e)
                (and (exn:fail:runner:module-evaluation? e)
                     (exn:fail:syntax? (exn:fail:runner:module-evaluation-error e))))
              ((make-instrumented-runner (program m/type-error empty)
                                         mod-stx)))
    (test-exn (λ (e)
                (and (exn:fail:runner:runtime? e)
                     (exn:fail:contract? (exn:fail:runner:runtime-error e))))
              ((make-instrumented-runner (program m/runtime-error empty)
                                         mod-stx)))

    (test-exn (λ (e)
                (and (exn:fail:runner:runtime? e)
                     (exn:fail? (exn:fail:runner:runtime-error e))))
              ((make-instrumented-runner (program m/segfault empty)
                                         mod-stx)))))

