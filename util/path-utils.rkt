#lang at-exp racket

(provide (all-defined-out))

(define (file-name-string-from-path f)
  (define-values {_1 name _2} (split-path f))
  (path->string name))

(define ((path-ends-with name) p)
  (define p-name (file-name-string-from-path p))
  (string=? p-name name))

(define (pick-file-by-name files name #:key [key values])
  (findf (compose1 (path-ends-with name) key) files))

(define path-to-existant-directory?
  (and/c path-string? directory-exists?))
(define path-to-existant-file?
  (and/c path-string? file-exists?))

(define (module-name? s)
  (and (string? s)
       (not (regexp-match? @regexp{[\/]} s))))

(define (library-path? s)
  (and (string? s)
       (regexp-match? @regexp{^\.\./base/} s)))

(define (paths=? a b)
  (equal? (simple-form-path a)
          (simple-form-path b)))

(define (explode-path/string p)
  (map path->string (explode-path p)))

(define (build-path-string . parts)
  (path->string (apply build-path parts)))

(define (benchmark-mod-relative-path-parts full-path)
  (match (explode-path/string full-path)
    [(list _ ... bench u/t name)
     (list bench u/t name)]
    [else
     (error 'benchmark-mod-relative-path-parts
            @~a{
                Unexpected benchmark path shape: @full-path
                })]))

(define (benchmark-mod-relative-path full-path)
  (apply build-path-string
         (benchmark-mod-relative-path-parts full-path)))

(define (file-or-directory-checksum path)
  (match (string-split
          (call-with-output-string
           (λ (str-out)
             (if (path-to-existant-file? path)
                 (parameterize ([current-output-port str-out])
                   (system @~a{md5sum @path}))
                 (parameterize ([current-output-port str-out]
                                [current-directory path])
                   (system
                    @~a{
                        find . -type f -exec md5sum '{}' ';' @;
                        | sort -k 2 @;
                        | md5sum
                        }))))))
    [(list* sum _) sum]
    [else #f]))

(define (path-replace-filename p new-name)
  (define-values {parent name _2} (split-path (simple-form-path p)))
  (build-path parent new-name))

(define (simple-form-path? path)
  (and (path? path)
       (complete-path? path)
       (for/and ([p (in-list (explode-path path))])
         (path-for-some-system? p))))

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
                 #f))

  (test-begin
    #:name benchmark-mod-relative-path
    (test-equal? (benchmark-mod-relative-path "/foo/bar/gregor/untyped/a.rkt")
                 "gregor/untyped/a.rkt"))

  (test-begin
    #:name path-replace-filename
    (paths=? (path-replace-filename "/foo/bar/hello.rkt" "bye")
             "/foo/bar/bye")))
