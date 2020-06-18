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
;; #hash((data-directory . <relative-data-directory-path>)
;;       (map . #hash((id . <file-name>) ...)))
;;
;; Note that the data-directory-path is relative to the metadata file
;; location.

(struct db (path ; path to the db metadata file
            map ; mapping from keys to file names in data dir
            data-dir ; absolute path to the data dir
            ))

(define (db-path->data-dir-path path)
  (~a path "-data"))

(define (parent-dir path)
  (define-values {parent name _} (split-path (simple-form-path path)))
  parent)

(define (find-db-data-path db-path rel-data-directory)
  (build-path (parent-dir db-path) rel-data-directory))

(define (write-serialized-db! a-db)
  (define path (db-path a-db))
  (define data-dir-relative-to-db-path
    (find-relative-path (parent-dir path)
                        (db-data-dir a-db)))
  (with-output-to-file path
    #:exists 'truncate
    (thunk (pretty-write
            (hash 'data-directory
                  (path->string data-dir-relative-to-db-path)
                  'map
                  (db-map a-db))))))
(define (read-serialized-db! path)
  (define metadata (file->value path))
  (define data-dir-path (find-db-data-path path
                                           (hash-ref metadata 'data-directory)))
  (db (simple-form-path path)
      (hash-ref metadata 'map)
      data-dir-path))

(define (new! path [maybe-data-dir-path #f])
  (define simple-path (simple-form-path path))
  (cond [(file-exists? path)
         (define old-db (get path))
         (delete-directory/files (db-data-dir old-db))
         (delete-file path)
         (new! simple-path maybe-data-dir-path)]
        [else
         (define data-dir-path
           (cond [(path-string? maybe-data-dir-path)
                  (simple-form-path maybe-data-dir-path)]
                 [else
                  (define-values {parent name _} (split-path simple-path))
                  (define data-dir-name (db-path->data-dir-path name))
                  (build-path parent data-dir-name)]))
         (make-directory* data-dir-path)
         (write-serialized-db! (db simple-path
                                   (hash)
                                   data-dir-path))]))

(define (path-to-db? path)
  (and (file-exists? path)
       (match (file->value path)
         [(hash-table ['data-directory rel-data-path]
                      ['map (? hash?)])
          (directory-exists?
           (find-db-data-path path rel-data-path))]
         [else #f])))

(define (get path)
  (unless (path-to-db? path)
    (error 'get @~a{@path doesn't exist or it doesn't look like a db}))
  (read-serialized-db! path))

(define (read a-db key [fail-result
                        (λ _
                          (error 'read
                                 @~a{key not found in db: @~e[key]}))])
  (define file-name (hash-ref (db-map a-db) key fail-result))
  (define data-dir-path (db-data-dir a-db))
  (define file-path (build-path data-dir-path file-name))
  (file->value file-path))

(define (write! a-db contents)
  (define data-dir (db-data-dir a-db))
  (delete-directory/files data-dir)
  (make-directory data-dir)
  (define new-map
    (for/hash ([{key data} (in-hash contents)]
               [i (in-naturals)])
      (define file-name (~a i))
      (define outfile (path->string (build-path data-dir file-name)))
      (with-output-to-file outfile
        (thunk (write data)))
      (values key file-name)))
  (write-serialized-db! (db (db-path a-db)
                            new-map
                            data-dir))
  (get (db-path a-db)))

(define (keys a-db)
  (hash-keys (db-map a-db)))
