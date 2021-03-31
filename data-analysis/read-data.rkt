#lang at-exp racket

(provide find-data-files
         read-blame-trails-by-mutator/across-all-benchmarks
         mutator-names
         read-mutants-by-mutator

         (struct-out mutant)
         (struct-out blame-trail)
         (struct-out benchmark-data-files)

         mutator-name?)

(require (prefix-in db: "../db/db.rkt")
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../experiment/blame-trail-data.rkt"
         "../util/mutant-util.rkt"
         "data-adapter.rkt"
         racket/hash
         rscript)

(struct benchmark-data-files (name data-dir log progress-log)
  #:transparent)

(struct blame-trail (mutant-id
                     trail-id
                     ; Note: summaries are in reverse order! Last summary of trail is first.
                     mutant-summaries) #:transparent)

(define mutator-name? string?)

;; mode-data-dir := a directory containing data for each benchmark, all of which
;; was collected under the same experiment configuration

;; path-string? . -> . (listof benchmark-data-files?)
;; dir: a "mode" directory, containing the data for each benchmark,
;; either in subdirs (new format) or all together (old format)
(define (find-data-files mode-data-dir)
  ;; lltodo: it would be better if this info was recorded in the metadata file,
  ;; and this just found and read from those files
  (match (map path->string (directory-list mode-data-dir))
    [(and (list (regexp #rx"^[A-Za-z0-9]+$") ...)
          benchmark-dirs)
     ;; new format: directory per benchmark, directory has
     ;; {$bench.log $bench-progress.log data/}
     (for/list ([benchmark (in-list benchmark-dirs)])
       (define (benchmark-path f)
         (build-path mode-data-dir benchmark f))
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
                             (build-path mode-data-dir benchmark)
                             (build-path mode-data-dir (~a benchmark ".log"))
                             (build-path mode-data-dir (~a benchmark "-progress.log"))))]))

(define/contract (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir mutant-mutators)
  (path-to-existant-directory?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name? (listof blame-trail?)))

  (define dir-data-files (find-data-files mode-data-dir))
  (for/fold ([data-by-mutator (hash)])
            ([a-benchmark-data-files (in-list dir-data-files)])
    (hash-union data-by-mutator
                (read-blame-trails-by-mutator a-benchmark-data-files
                                              mutant-mutators)
                #:combine append)))

(define ((append-to-list new-elements) a-list)
  (append a-list new-elements))

(define/contract (read-blame-trails-by-mutator data-files
                                               mutant-mutators)
  (benchmark-data-files?
   (hash/c mutant? mutator-name?)
   . -> .
   (hash/c mutator-name? (listof blame-trail?)))

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
                    (raise-user-error 'read-blame-trails-by-mutator
                                      @~a{Can't find mutator for @id})))]
        [else (error 'read-blame-trails-by-mutator
                     @~a{Mutant trail file @mutant-trails-file is empty})]))

    (hash-update data-by-mutator
                 mutator
                 (append-to-list mutant-trails)
                 empty)))

(define (blame-trail-summaries->blame-trails trail-summaries the-benchmark-data-files)
  (define benchmark-name (benchmark-data-files-name the-benchmark-data-files))
  (map (match-lambda [(blame-trail-summary mod-name
                                           index
                                           id
                                           mutant-summaries)
                      (blame-trail (mutant benchmark-name mod-name index)
                                   id
                                   (map adapt-mutant-summary
                                        mutant-summaries))])
       trail-summaries))

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

