#lang at-exp racket

(provide find-data-files
         read-data
         mutator-names
         read-mutants-by-mutator

         (struct-out mutant)
         (struct-out blame-trail)
         (struct-out benchmark-data-files)

         mutator-name?

         transform-mutator-name)

(require (prefix-in db: "../db/db.rkt")
         "../configurables/configurables.rkt"
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../experiment/blame-trail-data.rkt"
         racket/hash
         rscript)

(struct benchmark-data-files (name data-dir log progress-log)
  #:transparent)

(struct mutant (benchmark mod index) #:transparent)

(struct blame-trail (mutant-id trail-id configurations) #:transparent)

(define mutator-name? string?)

(define (find-data-files dir)
  (match (map path->string (directory-list dir))
    [(and (list (regexp #rx"^[A-Za-z0-9]+$") ...)
          benchmark-dirs)
     ;; new format: directory per benchmark, directory has
     ;; {$bench.log $bench-progress.log data/}
     (for/list ([benchmark (in-list benchmark-dirs)])
       (define (benchmark-path f)
         (build-path dir benchmark f))
       (define data-dir
         (if (path-to-existant-directory? (benchmark-path "data"))
             (benchmark-path "data")
             (benchmark-path benchmark)))
       (benchmark-data-files benchmark
                             data-dir
                             (benchmark-path (~a benchmark ".log"))
                             (benchmark-path (~a benchmark "-progress.log"))))]
    [old-format-files
     ;; old format: each benchmark has
     ;; {$bench.log $bench-progress.log $bench/}
     (define benchmark-dirs
       (filter (λ (f) (regexp-match? #rx"^[A-Za-z0-9]+$" f))
               old-format-files))
     (for/list ([benchmark (in-list benchmark-dirs)])
       (benchmark-data-files benchmark
                             (build-path dir benchmark)
                             (build-path dir (~a benchmark ".log"))
                             (build-path dir (~a benchmark "-progress.log"))))]))

(define/contract (read-data dir mutant-mutators)
  (path-to-existant-directory?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name? (listof blame-trail?)))

  (define dir-data-files (find-data-files dir))
  (for/fold ([data-by-mutator (hash)])
            ([a-benchmark-data-files (in-list dir-data-files)])
    (hash-union data-by-mutator
                (read-data-for-benchmark a-benchmark-data-files
                                         mutant-mutators)
                #:combine append)))

(define ((append-to-list new-elements) a-list)
  (append a-list new-elements))

(define/contract (read-data-for-benchmark data-files
                                          mutant-mutators)
  (benchmark-data-files?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name? (listof blame-trail?)))

  (define recorded-data
    (for/fold ([data-by-mutator (hash)])
              ([mutant-trails-file
                (in-directory (benchmark-data-files-data-dir data-files))]
               #:when (regexp-match? #rx".rkt_[0-9]+.rktd" mutant-trails-file))
      (define mutant-trails
        (blame-trail-summaries->blame-trails (file->list mutant-trails-file)
                                             data-files))
      (match (filter (negate blame-trail?)
                     mutant-trails)
        ['() (void)]
        [bad-stuff
         (displayln
          @~a{
              Found bad data in @mutant-trails-file @;
              including @(first bad-stuff) ...
              })])
      (define mutator
        (match mutant-trails
          [(cons (struct* blame-trail ([mutant-id id]))
                 _)
           (hash-ref mutant-mutators
                     id
                     (thunk
                      (raise-user-error 'read-data-for-benchmark
                                        @~a{Can't find mutator for @id})))]
          [else (error 'read-data-for-benchmark
                       @~a{Mutant trail file @mutant-trails-file is empty})]))

      (hash-update data-by-mutator
                   mutator
                   (append-to-list mutant-trails)
                   empty)))

  (define lost-data
    (recover-lost-BTs-in data-files mutant-mutators))

  (hash-union recorded-data
              lost-data
              #:combine append))

(define (blame-trail-summaries->blame-trails trail-summaries the-benchmark-data-files)
  (define benchmark-name (benchmark-data-files-name the-benchmark-data-files))
  (map (match-lambda [(blame-trail-summary mod-name
                                           index
                                           id
                                           configurations)
                      (blame-trail (mutant benchmark-name mod-name index)
                                   id
                                   configurations)])
       trail-summaries))

(define warned? (box #f))
(define/contract (recover-lost-BTs-in data-files mutant-mutators)
  (benchmark-data-files?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name? (listof blame-trail?)))

  (define log (benchmark-data-files-log data-files))
  (unless (unbox warned?)
    (displayln "WARNING: Recovery of lost BTs not yet implemented"
               (current-error-port))
    (set-box! warned? #t))
  ;; lltodo
  (hash))

(define (mutator-names data)
  (hash-keys data))

(define/contract (read-mutants-by-mutator summaries-db)
  (db:path-to-db? . -> . (hash/c mutant? mutator-name?))

  (define db (db:get summaries-db))
  (for*/hash ([benchmark (in-list (db:keys db))]
              [{mod mod-summary} (in-hash (db:read db benchmark))]
              [{mutator-name indices} (in-hash (summary-valid-indices mod-summary))]
              [index (in-list indices)])
    (values (mutant benchmark mod index) mutator-name)))

(define (transform-mutator-name mutator)
  mutator)

