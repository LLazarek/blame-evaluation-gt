#lang at-exp racket

(require "../util/optional-contracts.rkt")
(provide (contract-out
          [load-specific (string? string? symbol? . -> . any/c)]
          [load-configured (path-string? string? symbol? . -> . any/c)]
          [current-configuration-path (parameter/c (or/c #f path-string?))]))

(require racket/runtime-path)

(define-runtime-path configurables-path "all-configurables.rktd")
(define implementation-paths-root
  (let-values ([{parent _1 _2} (split-path configurables-path)])
    parent))

(unless (file-exists? configurables-path)
  (raise-user-error 'configurables
                    @~a{Unable to find configuration at @configurables-path}))

(define configurable-interface?
  (or/c
   ; a name
   string?

   ; a name and parameters
   (cons/c string?
           (listof symbol?))))
(define configurables/c
  (hash/c string?
          (hash/c configurable-interface?
                  path-string?)))
(define configurables (file->value configurables-path))

(unless (hash? configurables)
  (raise-user-error 'configurables
                    @~a{Malformed configurables at @configurables}))

(struct implementation (relative-path ; path-string?
                        parameters ; listof symbol?
                        ))

;; string? string? . -> . implementation?
(define (read-implementation-for category name)
  (match (hash-ref configurables
                   category
                   (λ _ (error
                         'read-implementation-for
                         @~a{@category not found in configurables})))
    [(hash-table [(== name) path] _ ...) (implementation path empty)]
    [(hash-table [(list* (== name) params) path] _ ...) (implementation path params)]
    [else
     (error
      'read-implementation-for
      @~a{@name not found in configurables for @category})]))

(define (load-specific category name id parameter-values)
  (define the-implementation (read-implementation-for category name))
  (define relative-path (implementation-relative-path the-implementation))
  (define absolute-path (build-path implementation-paths-root
                                    relative-path))
  (define mod-path `(file ,(path->string absolute-path)))

  (define parameters (implementation-parameters the-implementation))
  (unless (equal? (length parameters) (length parameter-values))
    (raise-user-error 'configurables
                      @~a{
                          Configurable for @category @name given the wrong number of parameters.
                          Expected: @~e[parameters]
                          Given:    @~e[parameter-values]
                          }))
  (for ([parameter-name (in-list parameters)]
        [value          (in-list parameter-values)])
    (define parameter
      (dynamic-require mod-path
                       parameter-name
                       (λ _
                         (error 'load-specific
                                @~a{
                                    Implementation from configurables @relative-path either @;
                                    does not exist or does not provide parameter @parameter-name
                                    }))))
    (parameter value))
  (dynamic-require
   mod-path
   id
   (λ _
     (error 'load-specific
            @~a{
                Implementation from configurables @relative-path either does not exist or @;
                does not provide @id
                }))))

(define (load-configured config-path category id)
  (define config (file->value config-path))
  (define-values {name parameter-values}
    (match (hash-ref config category)
      [(list (? string? name) vals ...) (values name vals)]
      [(? string? name)  (values name empty)]
      [other (raise-user-error
              'load-configured
              @~a{
                  Malformed configuration value in @config-path
                  Expected: either a list starting with a string, or a string
                  Got:      @~e[other]
                  })]))
  (load-specific category name id parameter-values))

(define current-configuration-path (make-parameter #f))
