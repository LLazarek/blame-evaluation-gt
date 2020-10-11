#lang at-exp racket/base

(provide create/check-metadata-integrity!
         (struct-out metadata-info)
         (struct-out metadata-id))

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

(define (without-config-outcomes-count id)
  (struct-copy metadata-id id
               [config-outcomes-count #f]))

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
          (equal? (without-config-outcomes-count current-id)
                  (without-config-outcomes-count recorded-id)))]
    ['check
     (equal? current-id recorded-id)]))
