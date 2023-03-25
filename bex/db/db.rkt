#lang at-exp racket/base

;; Very simple file-system-based database
;; todo: tests

(require racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/path
         racket/pretty
         racket/port
         (except-in racket/contract/base contract-out)
         openssl/md5
         "../util/optional-contracts.rkt"
         "../util/path-utils.rkt"
         "../util/experiment-exns.rkt")

(provide (contract-out
          [path-to-db? (path-string? . -> . boolean?)]
          [get (path-string? . -> . db?)]
          [new! ({path-string?} {path-string?} . ->* . any)]
          [read ({db? any/c} {failure-result/c #:reader (path-string? . -> . any/c)} . ->* . any/c)]
          [write! ({db? hash?} {#:writer (any/c . -> . any)} . ->* . db?)]
          [set! (db? any/c any/c . -> . any)]
          [keys (db? . -> . (listof any/c))]

          [checksum (db? . -> . string?)]))

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
;;
;;
;; DB's are not really intended to be used concurrently.
;;
;; The only function that considers concurrency is `set!`, which will obtain a
;; lock of the DB to perform the write, so that multiple processes can in
;; parallel do `set!`s on a DB without losing any one of their data.
;; Reading operations *do not consider concurrency*.
;; The takeaway is that DBs can be banged on concurrently by `set!`s, but
;; reading and writing should not happen concurrently.

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
  (and (path-string? path)
       (file-exists? path)
       (match (file->value path)
         [(hash-table ['data-directory rel-data-path]
                      ['map (? hash?)])
          (directory-exists?
           (find-db-data-path path rel-data-path))]
         [else #f])))

(define (get path)
  (unless (path-to-db? path)
    (raise-internal-experiment-argument-error 'get
                                              "a path-to-db?"
                                              path))
  (read-serialized-db! path))

(define (read a-db
              key
              [fail-result
               (λ _
                 (raise-internal-experiment-argument-error 'read
                                                           "a key in the db"
                                                           key))]
              #:reader [read-from-file file->value])
  (define file-name (hash-ref (db-map a-db) key #f))
  (cond [file-name
         (define data-dir-path (db-data-dir a-db))
         (define file-path (build-path data-dir-path file-name))
         (read-from-file file-path)]
        [(procedure? fail-result) (fail-result)]
        [else fail-result]))

(define (write! a-db contents #:writer [write-value-to-file write-to-file])
  (define data-dir (db-data-dir a-db))
  (delete-directory/files data-dir)
  (make-directory data-dir)
  (define new-map
    (for/hash ([{key data} (in-hash contents)]
               [i (in-naturals)])
      (define file-name (~a i))
      (define outfile (path->string (build-path data-dir file-name)))
      (write-value-to-file data outfile)
      (values key file-name)))
  (write-serialized-db! (db (db-path a-db)
                            new-map
                            data-dir))
  (get (db-path a-db)))

(define WRITE-TIMEOUT-SECS (* 5 60))
(define (set! a-db key value #:writer [write-value-to-file write-to-file])
  (define lock-file (build-path (db-data-dir a-db) "write-lock"))
  (call-with-file-lock/timeout
   #f
   #:lock-file lock-file
   'exclusive
   (thunk
    (let ([a-db (read-serialized-db! (db-path a-db))])
      (define (find-next-index)
        (define numbers (map string->number (hash-values (db-map a-db))))
        (define next-index
          (if (empty? numbers)
              0
              (add1 (apply max 0 numbers))))
        (~a next-index))

      (define data-dir-path (db-data-dir a-db))
      (define file-name (hash-ref (db-map a-db)
                                  key
                                  find-next-index))
      (define file-path (build-path data-dir-path file-name))
      (when (file-exists? file-path)
        (delete-file file-path))
      (write-value-to-file value
                           file-path)
      (define new-db (struct-copy db a-db [map (hash-set (db-map a-db) key file-name)]))
      (write-serialized-db! new-db)))
   (thunk
    (raise-internal-experiment-error 'db:set!
                                     @~a{Timeout waiting on lock to write db: @(db-path a-db)}))
   #:max-delay 1))

(define (keys a-db)
  (hash-keys (db-map a-db)))

(define (checksum a-db)
  (define part-sums (list (file-or-directory-checksum (db-path a-db))
                          (file-or-directory-checksum (db-data-dir a-db))))
  (define double-sum (apply string-append part-sums))
  (call-with-input-string double-sum md5))

(module+ test
  (require (except-in racket set! read)
           ruinit)

  (define-test-env {setup! cleanup!}
    #:directories ([test-dir "./test-dir"])
    #:files ())

  (test-begin
    #:before (setup!)
    #:after (cleanup!)

    ;; Basics
    (ignore (define a-db-path (build-path test-dir "a.rktdb")))
    (not (path-to-db? a-db-path))
    (ignore (define a-db (begin (new! a-db-path)
                                (get a-db-path))))
    (path-to-db? a-db-path)
    (test-equal? (keys a-db) empty)
    (test-exn exn:fail? (read a-db "key1"))

    (ignore (write! a-db (hash "key1" 42 "key77" 77))
            (define a-db/2* (get a-db-path)))
    (set=? (keys a-db/2*) '("key1" "key77"))
    (test-equal? (read a-db/2* "key1") 42)
    (test-equal? (read a-db/2* "key77") 77)

    (ignore (write! a-db (hash "key1" 42))
            (define a-db/2 (get a-db-path)))
    (test-equal? (keys a-db/2) '("key1"))
    (test-equal? (read a-db/2 "key1") 42)

    ;; Concurrent set! integrity.
    ;; Two consecutive set!'s don't overwrite each other
    (ignore (set! a-db/2 "key2" 43)
            (set! a-db/2 "key3" 50)
            (define a-db/3 (get a-db-path)))
    (set=? (keys a-db/3) '("key1" "key2" "key3"))
    (test-equal? (read a-db/3 "key1") 42)
    (test-equal? (read a-db/3 "key2") 43)
    (test-equal? (read a-db/3 "key3") 50)

    ;; Concurrent set!'s don't either
    (ignore (define thd
              (thread
               (thunk (set! a-db/3 "key4" 4 #:writer (λ (v f)
                                                       (sleep 0.5)
                                                       (write-to-file v f))))))
            (sleep 0.1)
            (set! a-db/3 "key5" 5)
            (thread-wait thd)
            (define a-db/4 (get a-db-path)))
    (set=? (keys a-db/4) '("key1" "key2" "key3" "key4" "key5"))
    (test-equal? (read a-db/4 "key1") 42)
    (test-equal? (read a-db/4 "key2") 43)
    (test-equal? (read a-db/4 "key3") 50)
    (test-equal? (read a-db/4 "key4") 4)
    (test-equal? (read a-db/4 "key5") 5)))
