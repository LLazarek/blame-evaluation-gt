#lang racket

(provide string->value
         simple-memoize)

(define (string->value s)
  (with-input-from-string s read))

(define (simple-memoize f #:on-disk [cache-path #f])
  (let ([cache (if (and cache-path (file-exists? cache-path))
                   (make-hash (hash->list (file->value cache-path)))
                   (make-hash))])
    (λ args
      (cond [(hash-ref cache args #f) => values]
            [else
             (define result (apply f args))
             (hash-set! cache args result)
             (when cache-path (write-to-file cache cache-path #:exists 'replace))
             result]))))
