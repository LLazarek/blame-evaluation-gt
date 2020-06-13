#lang at-exp racket/base

;; Very simple file-system-based database

(require racket/file
         racket/format
         racket/function
         racket/match
         racket/path
         racket/pretty
         "../util/optional-contracts.rkt")

(provide (contract-out
          [path-to-db? (path-string? . -> . boolean?)]
          [get (path-string? . -> . db?)]
          [new! ({path-string?} {path-string?} . ->* . any)]
          [read ({db? any/c} {failure-result/c} . ->* . any/c)]
          [write! (db? hash? . -> . db?)]
          [keys (db? . -> . (listof? any/c))]))

;; A DB is a pair of:
;; - a file of metadata
;; - a directory containing data
;;
;; A DB is uniquely identified by its metadata file.
;; The metadata file contains a single hash table of the form:
;; #hash((data-directory . <data-directory-path>)
;;       (map . #hash((id . <data-directory-path>/<file>) ...)))

(struct db (path metadata))

(define (db-path->data-dir-path path)
  (~a path "-data"))

(define (new! path [maybe-data-dir-path #f])
  (define simple-path (simple-form-path path))
  (cond [(file-exists? path)
         (define old-db (get path #f #f #f))
         (delete-directory/files (hash-ref (db-metadata old-db)
                                           'data-directory))
         (delete-file path)
         (new! simple-path)]
        [else
         (define data-dir-path
           (cond [maybe-data-dir-path maybe-data-dir-path]
                 [else
                  (define-values {parent name _} (split-path simple-path))
                  (define data-dir-name (db-path->data-dir-path name))
                  (build-path parent data-dir-name)]))
         (make-directory* data-dir-path)
         (with-output-to-file simple-path
           (thunk (pretty-write
                   (hash 'data-directory (path->string data-dir-path)
                         'map (hash)))))]))

(define (path-to-db? path)
  (and (file-exists? path)
       (match (file->value path)
         [(hash-table ['data-directory (? directory-exists?)]
                      ['map (? hash?)]) #t]
         [else #f])))

(define (get path)
  (unless (path-to-db? path)
    (error 'get @~a{@path doesn't exist or it doesn't look like a db}))
  (db (simple-form-path path)
      (file->value path)))

(define (read a-db key [fail-result
                        (λ _
                          (error 'read
                                 @~a{key not found in db: @~e[key]}))])
  (define db-map (hash-ref (db-metadata a-db) 'map))
  (file->value (hash-ref db-map key fail-result)))

(define (write! a-db contents)
  (define metadata (db-metadata a-db))
  (define data-dir (hash-ref metadata 'data-directory))
  (delete-directory/files data-dir)
  (make-directory data-dir)
  (define new-map
    (for/hash ([{key data} (in-hash contents)]
               [i (in-naturals)])
      (define outfile (path->string (build-path data-dir (~a i))))
      (with-output-to-file outfile
        (thunk (write data)))
      (values key outfile)))
  (define new-metadata
    (hash-set metadata 'map new-map))
  (define path (db-path a-db))
  (with-output-to-file path
    #:exists 'replace
    (thunk (pretty-write new-metadata)))
  (get path))

(define (keys a-db)
  (hash-keys (hash-ref (db-metadata a-db) 'map)))
