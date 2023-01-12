#lang at-exp racket

(provide (struct-out mod)
         (struct-out program)
         (contract-out
          [mod/c contract?]
          [program/c contract?]
          [make-program
           (path-string? (listof path-string?) . -> . program/c)]
          [make-mod
           (path-string? . -> . mod/c)]
          [mod->name (mod/c . -> . string?)]
          [find-program-base-path
           (program/c . -> . path-string?)]
          [program->mods
           (program/c . -> . (listof mod/c))]
          [program-mod-with-name
           (string? program/c . -> . (or/c mod/c #f))]))

(require "path-utils.rkt"
         "read-module.rkt")

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
       [else 0]))}
  #:methods gen:custom-write
  {(define (write-proc mod port mode)
     (display @~a{#<mod: @~s[(mod-path mod)] #<syntax from @~s[(syntax-source (mod-stx mod))]>>} port))})
(struct program (main others)
  #:transparent)

(define mod/c (struct/c mod path-string? syntax?))
(define (program-main-not-in-others? a-program)
  (not (member (program-main a-program)
               (program-others a-program))))
(define program/c (and/c (struct/c program mod/c (listof mod/c))
                         program-main-not-in-others?))

(define (make-mod path-str)
  (define path (simple-form-path path-str))
  (mod path
       (read-module path)))

(define (make-program main others)
  (program (make-mod main)
           (map make-mod others)))

(define (mod->name m)
  (file-name-string-from-path (mod-path m)))

(define (program->mods a-program)
  (cons (program-main a-program)
        (program-others a-program)))

(define (find-program-base-path a-program)
  (define files
    (map mod-path
         (program->mods a-program)))
  (define main (first files))
  (define candidate-subpath-parts
    (in-combinations (explode-path main)))
  (define ((path-prefix?-of a-path) subpath)
    (string-prefix? (path->string a-path)
                    (path->string subpath)))
  (define candidate-subpaths
    (sequence-filter
     (path-prefix?-of main)
     (sequence-map (match-lambda
                     ['() (string->path "nothing")]
                     [parts (apply build-path parts)])
                   candidate-subpath-parts)))
  (for/last ([candidate candidate-subpaths]
             #:when (for/and ([f (in-list files)])
                      ((path-prefix?-of f) candidate)))
    candidate))

(define (program-mod-with-name mod-name a-program)
  (findf (compose1 (path-ends-with mod-name) mod-path)
         (program->mods a-program)))

(module+ test
  (require ruinit)

  (test-begin
    #:name mod-equality
    (test-equal? (mod "foobar.rkt" #'(1 2 3))
                 (mod "foobar.rkt" #'(1 2 3)))
    (test-equal? (mod "foobar.rkt" #'(1 2 3))
                 (mod "foobar.rkt" #'(1 2 3 4 5)))
    (not/test
     (test-equal? (mod "foobar.rkt" #'(1 2 3))
                  (mod "something-else.rkt" #'(1 2 3)))))

  (test-begin
    #:name find-program-base-path
    (test-equal?
     (find-program-base-path
      (program (mod (string->path "/foo/bar/bench/untyped/main.rkt") #'())
               (list (mod (string->path "/foo/bar/bench/typed/baz.rkt") #'())
                     (mod (string->path "/foo/bar/bench/typed/bez.rkt") #'()))))
     (string->path "/foo/bar/bench"))
    (test-equal?
     (find-program-base-path
      (program (mod (string->path "/foo/bar/bench/typed/main.rkt") #'())
               (list (mod (string->path "/foo/bar/bench/typed/baz.rkt") #'())
                     (mod (string->path "/foo/bar/bench/typed/bez.rkt") #'()))))
     (string->path "/foo/bar/bench/typed"))))
