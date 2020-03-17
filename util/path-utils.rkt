#lang at-exp racket

(provide (all-defined-out))

(define (file-name-string-from-path f)
  (define-values {_1 name _2} (split-path f))
  (path->string name))

(define ((path-ends-with name) p)
  (define p-name (file-name-string-from-path p))
  (string=? p-name name))

(define (pick-file-by-name files name)
  (findf (path-ends-with name) files))

(define path-to-existant-directory?
  (and/c path-string? directory-exists?))
(define path-to-existant-file?
  (and/c path-string? file-exists?))

(module+ test
  (require ruinit)
  (test-begin
    #:name file-name-string-from-path
    (test-equal? (file-name-string-from-path (string->path "/foo/bar/baz.txt"))
                 "baz.txt")
    (test-equal? (file-name-string-from-path "/foo/bar/baz.txt")
                 "baz.txt")
    (test-equal? (file-name-string-from-path "/foo/bar/")
                 "bar")
    (test-equal? (file-name-string-from-path "/foo/bar")
                 "bar"))

  (test-begin
    #:name path-ends-with
    ((path-ends-with "foo.rkt") "/foo/bar/foo.rkt")
    (not/test ((path-ends-with "foo.rkt") "/foo/bar/bar.rkt"))
    ((path-ends-with "bar") "/foo/bar")
    ((path-ends-with "bar") "/foo/bar/"))

  (test-begin
    #:name pick-file-by-name
    (test-equal? (pick-file-by-name '("foo" "bar" "/abc/def.rkt" "a/p/e.rkt")
                                    "def.rkt")
                 "/abc/def.rkt")
    (test-equal? (pick-file-by-name '("foo" "bar" "/abc/def.rkt" "a/p/def.rkt")
                                    "def.rkt")
                 "/abc/def.rkt")
    (test-equal? (pick-file-by-name '("foo" "bar")
                                    "def.rkt")
                 #f)
    (test-equal? (pick-file-by-name '()
                                    "def.rkt")
                 #f)))
