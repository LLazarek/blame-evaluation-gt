#lang at-exp racket/base

(provide create/check-metadata-integrity!
         (struct-out metadata-info)
         (struct-out metadata-id))

(require racket/file
         racket/function
         racket/runtime-path
         (prefix-in db: "../db/db.rkt")
         "../util/path-utils.rkt"
         "../configurables/configurables.rkt")

(define-runtime-path configurables-dir "../configurables")

(struct metadata-info (file benchmark config)
  #:transparent)
(struct metadata-id (benchmark-name
                     benchmark-checksum
                     config-name
                     config-checksum
                     dbs)
  #:prefab)

;; metadata-info? -> boolean?
(define (create/check-metadata-integrity! metadata)
  (if (file-exists? (metadata-info-file metadata))
      (check-integrity! metadata)
      (create! metadata)))

;; metadata-info? -> metadata-id?
(define (metadata-info->id metadata)
  (define config (metadata-info-config metadata))
  (define samples-checksum
    (with-handlers ([exn:fail? (const #f)])
      (db:checksum
       (db:get (build-path configurables-dir
                           (load-configured config
                                            "mutant-sampling"
                                            'mutation-analysis-samples-db))))))
  (define special-cases-checksum
    (with-handlers ([exn:fail? (const #f)])
      (db:checksum
       (db:get (build-path configurables-dir
                           (load-configured config
                                            "module-instrumentation"
                                            'transient-special-cases-db))))))
  (define pre-computed-results-checksum
    (with-handlers ([exn:fail? (const #f)])
      (db:checksum
       (db:get (build-path configurables-dir
                           (load-configured config
                                            "benchmark-runner"
                                            'pre-computed-results-db))))))
  (define benchmark-path (metadata-info-benchmark metadata))
  (metadata-id
   (file-name-string-from-path benchmark-path) (file-or-directory-checksum benchmark-path)
   (file-name-string-from-path config)         (file-or-directory-checksum config)
   (hash "mutation-analysis-samples-db" samples-checksum
         "transient-special-cases-db" special-cases-checksum
         "pre-computed-results-db" pre-computed-results-checksum)))

;; metadata-info? -> boolean?
(define (create! metadata)
  (write-to-file (metadata-info->id metadata)
                 (metadata-info-file metadata))
  #t)

;; metadata-info? -> boolean?
(define (check-integrity! metadata)
  (equal? (metadata-info->id metadata)
          (file->value (metadata-info-file metadata))))
