#lang at-exp racket/base

(require racket/contract/base
         racket/format
         racket/match)

(provide (all-defined-out))

(define config/c (hash/c string? any/c))

(define config-set hash-set)
(define config-ref hash-ref)

(define (config-at-max-precision-for? name config)
  (equal? (hash-ref config name) 'types))

(define (increment-config-precision-for name config
                                        #:increment-types-error?
                                        [error-if-already-types? #t])
  (match (hash-ref config name)
    ['none (hash-set config name 'types)]
    [else
     #:when error-if-already-types?
     (error 'increment-config-precision-for
            @~a{Given config with value @name already at types: @config})]
    [else config]))
(define (increment-config-precision-for-all names config
                                            #:increment-types-error?
                                            [error-if-already-types? #t])
  (for/fold ([config config])
            ([name (in-list names)])
    (increment-config-precision-for name config
                                    #:increment-types-error? error-if-already-types?)))

(define (serialize-config config)
  (define ordered-mods (sort (hash-keys config) string<?))
  (string->number
   (list->string
    (for/list ([mod (in-list ordered-mods)])
      (match (hash-ref config mod)
        ['types #\1]
        ['none  #\0])))))
(define (deserialize-config config-number
                            #:reference [reference-config #f]
                            #:modules [mods (hash-keys reference-config)])
  (define ordered-mods (reverse (sort mods string<?)))
  (define ordered-mod-chars (reverse (string->list (~a config-number))))
  (for/hash ([mod (in-list ordered-mods)]
             [char (in-sequences ordered-mod-chars
                                 (in-cycle (in-value #\0)))])
    (values mod
            (match char
              [#\1 'types]
              [#\0 'none]))))

(module+ test
  (require ruinit
           racket)

  (test-begin
    #:name test:increment-config-precision-for
    (test-equal? (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'none
                        "bazzle.rkt" 'types))
                 (hash "baz.rkt" 'types
                       "bazzle.rkt" 'types))
    (test-exn exn:fail?
              (increment-config-precision-for
                  "baz.rkt"
                  (hash "baz.rkt" 'types
                        "bazzle.rkt" 'types))))

  (test-begin
    #:name config-at-max-precision-for?
    (not/test (config-at-max-precision-for?
               "baz.rkt"
               (hash "baz.rkt" 'none
                     "bazzle.rkt" 'types)))
    (config-at-max-precision-for?
     "baz.rkt"
     (hash "baz.rkt" 'types
           "bazzle.rkt" 'types)))

  (test-begin
    #:name serialize/deserialize-config
    (test-equal? (serialize-config #hash(("a" . types) ("b" . none) ("c" . types)))
                 101)
    (test-equal? (serialize-config #hash(("a" . none) ("b" . none) ("c" . types)))
                 1)
    (test-equal? (serialize-config #hash(("a" . none) ("d" . types) ("c" . none)))
                 1)
    (ignore (define-simple-test (test-round-trip config)
              (test-equal? (deserialize-config (serialize-config config)
                                               #:reference config)
                           config)))
    (test-round-trip #hash(("a" . types) ("b" . none) ("c" . types)))
    (test-round-trip #hash(("a" . none) ("b" . none) ("c" . types)))
    (test-round-trip #hash(("a" . none) ("d" . types) ("c" . none)))))
