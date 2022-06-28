#lang at-exp racket

(provide make-config-serializer
         make-config-deserializer
         1-to-1-map->converters)

(require syntax/parse/define
         "../../util/path-utils.rkt")

(define-simple-macro (1-to-1-map->converters {~seq k v} ...)
  (let ([hf (hash {~@ k v} ...)]
        [hb (hash {~@ v k} ...)])
    (values (λ (left) (hash-ref hf left))
            (λ (right) (hash-ref hb right)))))

(define (make-config-serializer level->digit-char [key<? string<?])
  (λ (config)
    (define ordered-mods (sort (hash-keys config) key<?))
    (string->number
     (list->string
      (for/list ([mod (in-list ordered-mods)])
        (level->digit-char (hash-ref config mod)))))))

(define (make-config-deserializer digit-char->level benchmark->mutatable-modules)
  (λ (digits #:benchmark reference-benchmark)
    (define mods (benchmark->mutatable-modules reference-benchmark))
    (define ordered-mods (reverse (sort mods string<?)))
    (define ordered-mod-chars (reverse (string->list (~a digits))))
    (for/hash ([mod (in-list ordered-mods)]
               [char (in-sequences ordered-mod-chars
                                   (in-cycle (in-value #\0)))])
      (values mod
              (digit-char->level char)))))

(module+ test
  (require ruinit
           "../../configurations/configure-benchmark.rkt"
           (submod "../../configurations/configure-benchmark.rkt" test-env))

  (test-begin
    #:name serialize/deserialize-config
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)
    (ignore (define-values {level->digit digit->level}
              (1-to-1-map->converters 'types #\1
                                      'none  #\0))
            (define serialize-config (make-config-serializer level->digit))
            (define deserialize-config (make-config-deserializer digit->level
                                                                 (λ (b)
                                                                   (map file-name-string-from-path
                                                                        (benchmark-typed b)))))

            (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (serialize-config #hash(("main.rkt" . types)
                                         ("a.rkt" . none)
                                         ("b.rkt" . types)))
                 11)
    (test-equal? (serialize-config #hash(("main.rkt" . none)
                                         ("a.rkt" . none)
                                         ("b.rkt" . types)))
                 10)
    (test-equal? (serialize-config #hash(("main.rkt" . none)
                                         ("a.rkt" . types)
                                         ("b.rkt" . none)))
                 100)
    (ignore (define-simple-test (test-round-trip config)
              (test-equal? (deserialize-config (serialize-config config)
                                               #:benchmark a-benchmark)
                           config)))
    (test-round-trip #hash(("main.rkt" . types) ("a.rkt" . none) ("b.rkt" . types)))
    (test-round-trip #hash(("main.rkt" . none) ("a.rkt" . none) ("b.rkt" . types)))
    (test-round-trip #hash(("main.rkt" . none) ("a.rkt" . types) ("b.rkt" . none)))))
