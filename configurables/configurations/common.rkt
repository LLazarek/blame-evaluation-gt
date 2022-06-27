#lang at-exp racket

(provide make-config-serializer
         make-config-deserializer
         1-to-1-map->converters)

(require syntax/parse/define)

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

(define (make-config-deserializer digit-char->level)
  (λ (config-number
      #:reference [reference-config #f]
      #:modules [mods (hash-keys reference-config)])
    (define ordered-mods (reverse (sort mods string<?)))
    (define ordered-mod-chars (reverse (string->list (~a config-number))))
    (for/hash ([mod (in-list ordered-mods)]
               [char (in-sequences ordered-mod-chars
                                   (in-cycle (in-value #\0)))])
      (values mod
              (digit-char->level char)))))

(module+ test
  (require ruinit)

  (test-begin
    #:name serialize/deserialize-config
    (ignore (define-values {level->digit digit->level}
              (1-to-1-map->converters 'types #\1
                                      'none  #\0))
            (define serialize-config (make-config-serializer level->digit))
            (define deserialize-config (make-config-deserializer digit->level)))
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
