#lang at-exp racket

(require "instrumented-runner.rkt"
         "../util/read-module.rkt"
         "../util/path-utils.rkt")

(provide (contract-out
          [make-program
           (path-string? (listof path-string?) . -> . program/c)]
          [make-mod
           (path-string? . -> . mod/c)]
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

(define unified-benchmark/c
  (and/c program/c
         (struct/dc
          program
          [main
           (λ (m)
             ((and/c path-string?
                     (not/c path-to-existant-file?)
                     (path-to-file-in/c unification-directory-name))
              (mod-path m)))]
          [others (main)
                  (listof
                   (λ (m)
                     ((and/c path-string?
                             (not/c path-to-existant-file?)
                             (or/c (path-to-file-in/c unification-directory-name)
                                   (path-to-file-in/c "base")
                                   (path-to-file-in/c "both")))
                      (mod-path m))))])))

(define (make-mod path-str)
  (define path (simple-form-path path-str))
  (mod path
       (read-module path)))

(define (make-program main others)
  (program (make-mod main)
           (map make-mod others)))

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
(define (unify-program-for-running a-program)
  (match-define (program raw-main raw-others)
    a-program)
  (program (unify-module-for-running raw-main)
           (map unify-module-for-running raw-others)))

(define (unify-module-for-running a-mod)
  (define relocated-path-parts
    (match (explode-path/string (mod-path a-mod))
      [(list before ... (or "untyped" "typed") name)
       (append before (list unification-directory-name name))]
      [(and (list _ ... (or "base" "both") _)
            other-parts)
       other-parts]
      [else
       (raise-argument-error
        'unify-module-for-running
        "a module from a benchmark (ie conforming to standard benchmark structure)"
        a-mod)]))
  (mod (apply build-path relocated-path-parts)
       (mod-stx a-mod)))


(define (unified-module-path-of? orig-mod-path maybe-unified-mod-path)
  (match* {(explode-path/string orig-mod-path)
           (explode-path/string maybe-unified-mod-path)}
    [{(list before-parts/orig ... (or "untyped" "typed") name/orig)
      (list before-parts/unif ... (== unification-directory-name) name/unif)}
     (and (equal? before-parts/orig before-parts/unif)
          (equal? name/orig name/unif))]
    [{(and parts/orig (list _ ... (or "base" "both") _))
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
                                                #'(a b c)))
                 (mod (build-path
                       "/foo/bar/a-bench"
                       unification-directory-name
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                                                #'(a b c)))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       unification-directory-name
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/base/main.rkt"
                                                #'(a b c)))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       "base"
                       "main.rkt")
                      #'(a b c)))
    (test-equal? (unify-module-for-running (mod "/foo/untyped/bar/a-bench/both/main.rkt"
                                                #'(a b c)))
                 (mod (build-path
                       "/foo/untyped/bar/a-bench"
                       "both"
                       "main.rkt")
                      #'(a b c)))


    (test-equal? (unify-program-for-running
                  (program
                   (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                        #'(a))
                   (list (mod "/foo/untyped/bar/a-bench/typed/helper.rkt"
                              #'(b))
                         (mod "/foo/untyped/bar/a-bench/base/adapter.rkt"
                              #'(b))
                         (mod "/foo/untyped/bar/a-bench/both/lib.rkt"
                              #'(c)))))
                 (program
                  (mod (build-path
                        "/foo/untyped/bar/a-bench"
                        unification-directory-name
                        "main.rkt")
                       #'(a))
                  (list (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              unification-directory-name
                              "helper.rkt")
                             #'(b))
                        (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              "base"
                              "adapter.rkt")
                             #'(b))
                        (mod (build-path
                              "/foo/untyped/bar/a-bench"
                              "both"
                              "lib.rkt")
                             #'(c))))))

  (test-begin
    #:name unified-benchmark/c
    (test-exn exn:fail:contract?
              (contract unified-benchmark/c
                        (program
                         (mod "/foo/untyped/bar/a-bench/untyped/main.rkt"
                              #'(a))
                         (list (mod "/foo/untyped/bar/a-bench/typed/helper.rkt"
                                    #'(b))
                               (mod "/foo/untyped/bar/a-bench/base/adapter.rkt"
                                    #'(b))
                               (mod "/foo/untyped/bar/a-bench/both/lib.rkt"
                                    #'(c))))
                        'pos 'neg))
    (contract unified-benchmark/c
              (program
               (mod (build-path
                     "/foo/untyped/bar/a-bench"
                     unification-directory-name
                     "main.rkt")
                    #'(a))
               (list (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           unification-directory-name
                           "helper.rkt")
                          #'(b))
                     (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           "base"
                           "adapter.rkt")
                          #'(b))
                     (mod (build-path
                           "/foo/untyped/bar/a-bench"
                           "both"
                           "lib.rkt")
                          #'(c))))
              'pos 'neg)))