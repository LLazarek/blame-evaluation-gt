#lang at-exp racket

(require "../util/optional-contracts.rkt")
(provide (contract-out
          [load-specific (string? string? symbol? . -> . any/c)]
          [load-configured (path-string? string? symbol? . -> . any/c)]))

(require racket/runtime-path)

(define-runtime-path configurables-path "all-configurables.rktd")
(define implementation-paths-root
  (let-values ([{parent _1 _2} (split-path configurables-path)])
    parent))

(unless (file-exists? configurables-path)
  (raise-user-error 'configurables
                    @~a{Unable to find configuration at @configurables-path}))

(define configurables (file->value configurables-path))

(unless (hash? configurables)
  (raise-user-error 'configurables
                    @~a{Malformed configurables at @configurables}))

(define (read-implementation-for category name)
  (hash-ref (hash-ref configurables
                      category
                      (λ _ (error
                            'read-implementation-for
                            @~a{@category not found in configurables})))
            name
            (λ _ (error
                  'read-implementation-for
                  @~a{@name not found in configurables for @category}))))

(define (load-specific category name id)
  (define implementation-rel-path (read-implementation-for category name))
  (define implementation-abs-path (build-path implementation-paths-root
                                              implementation-rel-path))
  (dynamic-require
   `(file ,(path->string implementation-abs-path))
   id
   (λ _
     (error
      'load-specific
      @~a{
          Implementation from configurables @implementation-rel-path @;
          either does not exist or does not provide @id
          }))))

(define (load-configured config-path category id)
  (define config (file->value config-path))
  (define name (hash-ref config category))
  (load-specific category name id))
