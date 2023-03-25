#lang at-exp racket

(require "../util/program.rkt"
         "../util/path-utils.rkt"
         "../util/ctc-utils.rkt"
         "../util/read-module.rkt"
         "../util/experiment-exns.rkt")

(provide (contract-out
          [unified-benchmark/c contract?]
          [unify-program-for-running
           (program/c . -> . unified-benchmark/c)]
          [unified-module-path-of?
           (path-string? path-string? . -> . boolean?)]

          [make-unified-program
           (path-to-existant-file?
            (listof path-to-existant-file?)
            . -> .
            unified-benchmark/c)]
          [find-unified-module-to-mutate
           (path-string? (listof mod/c) . -> . (or/c #f mod/c))]))

(define unification-directory-name
  "unified")

(define ((path-to-file-in/c dir-name) p)
  (match (explode-path/string p)
    [(list _ ... (== dir-name) _) #t]
    [else #f]))

(define (unified-mod/c #:base-or-both-ok? base-both-ok?)
  (suggest/c
   (simple-flat-contract-with-explanation
    (λ (m)
      ((and/c
        path-string?
        (if base-both-ok?
            (or/c (and/c (not/c path-to-existant-file?)
                         (path-to-file-in/c unification-directory-name))
                  (path-to-file-in/c "base")
                  (path-to-file-in/c "both"))
            (and/c (not/c path-to-existant-file?)
                   (path-to-file-in/c unification-directory-name))))
       (mod-path m)))
    @~a{
        a mod/c in the unification directory (@unification-directory-name) @;
        @(if base-both-ok?
             "or base/ or both/"
             "")
        })
   "suggestion"
   @~a{
       if the mod path looks right, check if the unification directory already exists. @;
       It should not exist!
       }))

(define unified-benchmark/c
  (and/c
   program/c
   (struct/dc program
              [main (unified-mod/c #:base-or-both-ok? #f)]
              [others (listof (unified-mod/c #:base-or-both-ok? #t))])))

;; Prepares a configured program for running.
;;
;; A configured program has a mixture of modules from a `typed` directory,
;; an `untyped` directory, and a `base` directory.
;; `typed` and `untyped` are mirrors of each other, so we want to prevent
;; the following situation:
;; program := ([main: .../a-benchmark/untyped/main.rkt]
;;             [others: (.../a-benchmark/typed/helper.rkt
;;                       .../a-benchmark/base/adapter.rkt)])
;; When we set up to run our program, we load up the modules
;;   .../a-benchmark/typed/helper.rkt
;;   .../a-benchmark/base/adapter.rkt
;; at each of their respective paths, and then the main module
;;   .../a-benchmark/untyped/main.rkt
;; And then we run main.rkt.
;; When we do that, racket will look for `helper.rkt` at
;;   .../a-benchmark/untyped/helper.rkt
;; because `main.rkt` is in `untyped`.
;; And we didn't load any module at that path, so it will pull it
;; from the file system, completely skipping the typed version of
;; `helper.rkt` that we loaded.
;;
;; So to prevent this, we need to put all of the modules together in an
;; imaginary directory at the same level as `typed` and `untyped`.
;;
;; The modules in the `both` directory also need to be placed in the
;; same directory as the rest of the program. This takes care of that
;; too.
(define (unify-program-for-running a-program)
  (match-define (program raw-main raw-others)
    a-program)
  (program (unify-module-for-running raw-main)
           (map unify-module-for-running raw-others)))

(define (unified-path-for a-mod)
  (define relocated-path-parts
    (match (explode-path/string (mod-path a-mod))
      [(list before ... (or "untyped" "typed" "both") name)
       (append before (list unification-directory-name name))]
      [(and (list _ ... "base" _)
            other-parts)
       other-parts]
      [else
       (raise-internal-experiment-argument-error
        'unify-module-for-running
        "a module from a benchmark (ie conforming to standard benchmark structure)"
        a-mod)]))
  (apply build-path relocated-path-parts))

(define (unify-module-for-running a-mod)
  (define unified-path (unified-path-for a-mod))
  (define stx (mod-stx a-mod))
  (mod unified-path
       (replace-stx-location stx unified-path)))


(define (unified-module-path-of? orig-mod-path maybe-unified-mod-path)
  (match* {(explode-path/string orig-mod-path)
           (explode-path/string maybe-unified-mod-path)}
    [{(list before-parts/orig ... (or "untyped" "typed" "both") name/orig)
      (list before-parts/unif ... (== unification-directory-name) name/unif)}
     (and (equal? before-parts/orig before-parts/unif)
          (equal? name/orig name/unif))]
    [{(and parts/orig (list _ ... "base" _))
      parts/unif}
     (equal? parts/orig parts/unif)]
    [{_ _} #f]))



;; Convenience functions
(define (make-unified-program main others)
  (unify-program-for-running
   (make-program main
                 others)))

(define (find-unified-module-to-mutate module-to-mutate-raw-path
                                       unified-program-mods)
  (findf (match-lambda [(mod path _)
                        (unified-module-path-of? module-to-mutate-raw-path path)])
         unified-program-mods))


(module+ test
  (require ruinit)

  (test-begin
    #:name unification
    (test-equal? (unify-module-for-running (mod "/foo/bar/a-bench/untyped/main.rkt"
                                                #'(module main racket
                                                    (#%module-begin a b c))))
                 (mod (build-path
                       "/foo/bar/a-bench"
                       unification-directory-name
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                                                #'(module main racket
                                                    (#%module-begin a b c))))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       unification-directory-name
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/base/main.rkt"
                                                #'(module main racket
                                                    (#%module-begin a b c))))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       "base"
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/both/main.rkt"
                                                #'(module main racket
                                                    (#%module-begin a b c))))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       unification-directory-name
                       "main.rkt")
                      #'(a b c)))


    (test-equal? (unify-program-for-running
                  (program
                   (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                        #'(module main racket
                            (#%module-begin a b c)))
                   (list (mod "/foo/untyped/bar/a-bench/typed/helper.rkt"
                              #'(module main racket
                                  (#%module-begin a b c)))
                         (mod "/foo/untyped/bar/a-bench/base/adapter.rkt"
                              #'(module main racket
                                  (#%module-begin a b c)))
                         (mod "/foo/untyped/bar/a-bench/both/lib.rkt"
                              #'(module main racket
                                  (#%module-begin a b c))))))
                 (program
                  (mod (build-path
                        "/foo/untyped/bar/a-bench"
                        unification-directory-name
                        "main.rkt")
                       #'(module main racket
                           (#%module-begin a b c)))
                  (list (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              unification-directory-name
                              "helper.rkt")
                             #'(module main racket
                                 (#%module-begin a b c)))
                        (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              "base"
                              "adapter.rkt")
                             #'(module main racket
                                 (#%module-begin a b c)))
                        (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              unification-directory-name
                              "lib.rkt")
                             #'(module main racket
                                 (#%module-begin a b c)))))))

  (test-begin
    #:name unified-benchmark/c
    (test-exn exn:fail:contract?
              (contract unified-benchmark/c
                        (program
                         (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                              #'(module main racket
                                  (#%module-begin a b c)))
                         (list (mod "/foo/untyped/bar/a-bench/typed/helper.rkt"
                                    #'(module main racket
                                        (#%module-begin a b c)))
                               (mod "/foo/untyped/bar/a-bench/base/adapter.rkt"
                                    #'(module main racket
                                        (#%module-begin a b c)))
                               (mod "/foo/untyped/bar/a-bench/both/lib.rkt"
                                    #'(module main racket
                                        (#%module-begin a b c)))))
                        'pos 'neg))
    (contract unified-benchmark/c
              (program
               (mod (build-path
                     "/foo/untyped/bar/a-bench"
                     unification-directory-name
                     "main.rkt")
                    #'(module main racket
                        (#%module-begin a b c)))
               (list (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           unification-directory-name
                           "helper.rkt")
                          #'(module main racket
                              (#%module-begin a b c)))
                     (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           "base"
                           "adapter.rkt")
                          #'(module main racket
                              (#%module-begin a b c)))
                     (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           unification-directory-name
                           "lib.rkt")
                          #'(module main racket
                              (#%module-begin a b c)))))
              'pos 'neg)
    (contract unified-benchmark/c
              (program
               (mod (build-path
                     "/proj/blgt/gtp-benchmarks/benchmarks/kcfa"
                     unification-directory-name
                     "main.rkt")
                    #'(module main racket
                        (#%module-begin a b c)))
               (list (mod (build-path
                           "/proj/blgt/gtp-benchmarks/benchmarks/kcfa"
                           unification-directory-name
                           "benv-adapted.rkt")
                          #'(module main racket
                              (#%module-begin a b c)))))
              'pos 'neg)

    (contract (listof (unified-mod/c #:base-or-both-ok? #t))
              (list (mod (simple-form-path "../../gtp-benchmarks/benchmarks/kcfa/both/benv-adapted.rkt")
                         #'(module main racket
                             (#%module-begin a b c))))
              'pos 'neg))

  (test-begin
    #:name unified-module-path-of?
    (unified-module-path-of? "/foo/bar/baz/benchmarks/sieve/typed/a.rkt"
                             "/foo/bar/baz/benchmarks/sieve/unified/a.rkt")
    (unified-module-path-of? "/foo/bar/baz/benchmarks/sieve/untyped/a.rkt"
                             "/foo/bar/baz/benchmarks/sieve/unified/a.rkt")
    (unified-module-path-of? "/foo/bar/baz/benchmarks/sieve/both/type-interface.rkt"
                             "/foo/bar/baz/benchmarks/sieve/unified/type-interface.rkt")
    (not (unified-module-path-of? "/foo/bar/baz/benchmarks/sieve/base/a.rkt"
                                  "/foo/bar/baz/benchmarks/sieve/unified/a.rkt"))
    (unified-module-path-of? "/foo/bar/baz/benchmarks/sieve/base/a.rkt"
                             "/foo/bar/baz/benchmarks/sieve/base/a.rkt")
    (unified-module-path-of? "/project/blgt/gtp-benchmarks/benchmarks/sieve/both/type-interface.rkt"
                             "/project/blgt/gtp-benchmarks/benchmarks/sieve/unified/type-interface.rkt")))
