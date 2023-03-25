#lang at-exp racket

(provide find-data-files
         read-blame-trails-by-mutator/across-all-benchmarks
         mutator-names
         read-mutants-by-mutator
         blame-trail-summaries->blame-trails

         (struct-out mutant)
         (struct-out blame-trail)
         (struct-out benchmark-data-files)

         read-mutants-by-mutator)

(require bex/mutation-analysis/mutation-analysis-summaries
         bex/experiment/blame-trail-data
         bex/experiment/integrity-metadata
         bex/util/mutant-util
         bex/configurations/config
         "data-adapter.rkt"
         "experiment-info.rkt"
         racket/hash
         rscript)

(struct benchmark-data-files (name data-dir log progress-log metadata)
  #:transparent)

(struct blame-trail (mutant-id
                     trail-id
                     mode-config-name
                     ; Note: summaries are in reverse order! Last summary of trail is first.
                     mutant-summaries)
  #:transparent)

;; mode-data-dir := a directory containing data for each benchmark, all of which
;; was collected under the same experiment configuration

;; path-string? . -> . (listof benchmark-data-files?)
;; dir: a "mode" directory, containing the data for each benchmark,
;; either in subdirs (new format) or all together (old format)
(define (find-data-files mode-data-dir)
  ;; lltodo: it would be better if this info was recorded in the metadata file,
  ;; and this just found and read from those files
  ;; Just remove support for the old format data at that point.
  (define (find-new-format-data)
    ;; new format: directory per benchmark, directory has
    ;; {$bench.log $bench-progress.log data/}
    (filter-not false?
                (for/list ([benchmark (in-list (directory-list mode-data-dir))])
                  (define (benchmark-path f)
                    (build-path mode-data-dir benchmark f))
                  (define data-dir
                    (if (path-to-existant-directory? (benchmark-path "data"))
                        (benchmark-path "data")
                        (benchmark-path benchmark)))
                  (define log (benchmark-path (~a benchmark ".log")))
                  (define progress-log (benchmark-path (~a benchmark "-progress.log")))
                  (define metadata-file (benchmark-path (~a benchmark "-metadata.rktd")))
                  (and (path-to-existant-directory? data-dir)
                       (path-to-existant-file? log)
                       (path-to-existant-file? progress-log)
                       (path-to-existant-file? metadata-file)
                       (benchmark-data-files (~a benchmark)
                                             data-dir
                                             log
                                             progress-log
                                             metadata-file)))))
  (define (find-old-format-data)
    ;; old format: each benchmark has
    ;; {$bench.log $bench-progress.log $bench/}
    (define benchmark-dirs
      (filter (λ (f) (regexp-match? #rx"^[A-Za-z0-9]+$" f))
              (directory-list mode-data-dir)))
    (filter-not false?
                (for/list ([benchmark (in-list benchmark-dirs)])
                  (define data-dir (build-path mode-data-dir benchmark))
                  (define log (build-path mode-data-dir (~a benchmark ".log")))
                  (define progress-log (build-path mode-data-dir (~a benchmark "-progress.log")))
                  (and (path-to-existant-directory? data-dir)
                       (path-to-existant-file? log)
                       (path-to-existant-file? progress-log)
                       (benchmark-data-files (~a benchmark)
                                             data-dir
                                             log
                                             progress-log
                                             #f)))))
  (match (find-new-format-data)
    ['() (find-old-format-data)]
    [some-data some-data]))

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
              (in-list (directory-list (benchmark-data-files-data-dir data-files)
                                       #:build? #t))]
             #:when (regexp-match? #rx".rkt_[0-9]+.rktd$" mutant-trails-file))
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
  (define benchmark (benchmark-name->benchmark benchmark-name))
  (define mode-config-name (and (benchmark-data-files-metadata the-benchmark-data-files)
                                (metadata-id-config-name
                                 (file->value
                                  (benchmark-data-files-metadata the-benchmark-data-files)))))
  (define (deserialize-mutant-summary ms)
    (match ms
      [(struct* mutant-summary ([config (? serialized-config? n)]))
       (struct-copy mutant-summary ms [config (deserialize-config n #:benchmark benchmark)])]
      [(struct* mutant-summary ([config (? hash? n)])) ms]
      [else
       (error 'blame-trail-summaries->blame-trails
              @~a{
                  Got a mutant summary with a config that satisfies neither @;
                  `serialized-config?` nor `hash?`:
                  @~s[ms]
                  })]))
  (map (match-lambda [(blame-trail-summary mod-name
                                           index
                                           id
                                           mutant-summaries)
                      (blame-trail (mutant benchmark-name mod-name index)
                                   id
                                   mode-config-name
                                   (map (compose1 deserialize-mutant-summary
                                                  adapt-mutant-summary)
                                        mutant-summaries))])
       trail-summaries))

(define (mutator-names data)
  (hash-keys data))


