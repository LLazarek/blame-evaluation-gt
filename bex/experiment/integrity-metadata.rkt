#lang at-exp racket/base

(provide create/check-metadata-integrity!
         (struct-out metadata-info)
         (struct-out metadata-id)
         metadata-info->id)

(require racket/file
         racket/function
         racket/match
         racket/runtime-path
         (prefix-in db: "../db/db.rkt")
         "../util/path-utils.rkt"
         "../configurables/mutant-sampling/use-pre-selected-samples.rkt"
         "../configurables/module-instrumentation/type-with-transient.rkt"
         "../configurables/benchmark-runner/load-pre-computed-result.rkt"
         "../configurables/bt-root-sampling/pre-selected.rkt")


(define dbs
  (hash 'transient-special-cases     transient-special-cases-db
        'pre-selected-bt-roots       pre-selected-bt-root-db
        'pre-computed-mutant-results pre-computed-results-db
        'mutant-samples              pre-selected-mutant-samples-db))

(define optional-dbs '(transient-special-cases
                       pre-computed-mutant-results))


(define-runtime-path configurables-dir "../configurables")

(struct metadata-info (file benchmark config config-outcome-checking)
  #:transparent)
(struct metadata-id (benchmark-name
                     benchmark-checksum
                     config-name
                     config-checksum
                     dbs
                     config-outcomes-mode
                     config-outcomes-log
                     config-outcomes-count)
  #:prefab)

;; metadata-info? -> boolean?
(define (create/check-metadata-integrity! metadata)
  (if (file-exists? (metadata-info-file metadata))
      (check-integrity! metadata)
      (create! metadata)))

(define (db-checksums)
  (for/hash ([{name get-db-path} (in-hash dbs)])
    (define checksum
      (with-handlers ([exn:fail? (const #f)])
        (db:checksum
         (db:get (build-path configurables-dir (get-db-path))))))
    (values name checksum)))

;; metadata-info? -> metadata-id?
(define (metadata-info->id metadata)
  (define config (metadata-info-config metadata))
  (define benchmark-path (metadata-info-benchmark metadata))
  (match-define (list config-outcomes-mode config-outcomes-log)
    (metadata-info-config-outcome-checking metadata))
  (define config-outcomes-count
    (if (file-exists? config-outcomes-log)
        (length (file->list config-outcomes-log))
        0))

  (metadata-id
   (file-name-string-from-path benchmark-path) (file-or-directory-checksum benchmark-path)
   (file-name-string-from-path config)         (file-or-directory-checksum config)
   (db-checksums)
   config-outcomes-mode
   (file-name-string-from-path config-outcomes-log)
   config-outcomes-count))

;; metadata-info? -> boolean?
(define (create! metadata)
  (write-to-file (metadata-info->id metadata)
                 (metadata-info-file metadata))
  #t)

(define (invariant-metadata-id-parts-only id)
  (struct-copy metadata-id id
               [config-outcomes-count #f]
               [dbs #f]))

;; Are the dbs listed by `other` compatible with those in `base`?
(define (db-compatible-with? other base)
  (for/and ([{name checksum} (in-hash base)])
    (match* {checksum (hash-ref other name #f)}
      [{#f other-sum}
       (or (member name optional-dbs)
           (not other-sum))]
      [{(? string? base-sum) other-sum} (equal? base-sum other-sum)])))

;; metadata-info? -> boolean?
(define (check-integrity! metadata)
  (define current-id (metadata-info->id metadata))
  (define recorded-id (file->value (metadata-info-file metadata)))
  (match (metadata-id-config-outcomes-mode recorded-id)
    ['record
     ;; The outcomes log is allowed to have grown since last time we ran, so
     ;; long as we're in `record` mode
     (and (>= (metadata-id-config-outcomes-count current-id)
              (metadata-id-config-outcomes-count recorded-id))
          (db-compatible-with? (metadata-id-dbs current-id)
                               (metadata-id-dbs recorded-id))
          (equal? (invariant-metadata-id-parts-only current-id)
                  (invariant-metadata-id-parts-only recorded-id)))]
    ['check
     (and (db-compatible-with? (metadata-id-dbs current-id)
                               (metadata-id-dbs recorded-id))
          (equal? (invariant-metadata-id-parts-only current-id)
                  (invariant-metadata-id-parts-only recorded-id)))]))
