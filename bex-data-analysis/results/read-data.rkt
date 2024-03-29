#lang at-exp racket

(provide find-data-files
         read-blame-trails-by-mutator/across-all-benchmarks
         read-blame-trails-by-mutator/across-all-benchmarks/from-data-files
         mutator-names
         read-mutants-by-mutator
         blame-trail-summaries->blame-trails
         deserialize-mutant-summary

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
         "blame-trails.rkt"
         racket/hash
         (only-in rscript
                  path-to-existant-directory?
                  path-to-existant-file?))

(struct benchmark-data-files (name data-dir log progress-log metadata)
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

(define/contract (read-blame-trails-by-mutator/across-all-benchmarks/from-data-files mode-data-dir mutant-mutators)
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

(require db
         bex/util/for
         data-frame
         "util.rkt")
(define (read-blame-trail-db->df db-path-or-conn [query "SELECT * FROM trails"])
  (define conn (if (connection? db-path-or-conn)
                   db-path-or-conn
                   (sqlite3-connect #:database db-path-or-conn
                                    #:mode 'read-only)))
  (define cache (make-hash))
  (define (make/get-mutant-summary-deserializer benchmark-name)
    (cond [(hash-ref cache benchmark-name #f) => values]
          [else
           (define benchmark-struct
             (string->value (query-value conn
                                         "SELECT benchmark FROM benchmarks WHERE name = $1"
                                         benchmark-name)))
           (define d (deserialize-mutant-summary benchmark-struct))
           (hash-set! cache benchmark-name d)
           d]))
  (begin0 (for/data-frame {mode benchmark mutant id mutant-summaries mutator success}
            ([{m b mt id m-s mu ok?} (in-query conn query)])
            (define serialized-m-s (string->value m-s))
            (values m
                    b
                    (string->value mt)
                    id
                    (map (make/get-mutant-summary-deserializer b) serialized-m-s)
                    mu
                    (= ok? 1)))
    (unless (connection? db-path-or-conn)
      (disconnect conn))))
(define/contract (read-blame-trails-by-mutator/across-all-benchmarks db-path-or-conn [mode #f] [use-pre-computed-success? #t])
  ({(or/c connection? path-to-existant-file?)}
   {(or/c string? #f)
    boolean?}
   . ->* .
   (hash/c mutator-name? (listof blame-trail?)))

  (define db-conn (if (connection? db-path-or-conn)
                      db-path-or-conn
                      (sqlite3-connect #:database db-path-or-conn
                                       #:mode 'read-only)))
  (for/hash/fold ([{mutator mutant trail-id mode mutant-summaries success?}
                   #;(apply in-query
                          db-conn
                          (~a "select mutator, mutant, id, mode, mutant_summaries, success from trails"
                              (if mode
                                  " where mode = $1"
                                  ""))
                          (or (list mode) empty))
                   (in-data-frame (read-blame-trail-db->df
                                   db-conn
                                   (if mode
                                       (bind-prepared-statement
                                        (prepare db-conn
                                                 "SELECT * FROM trails where mode = $1")
                                        (list mode))
                                       "SELECT * FROM trails"))
                                  "mutator" "mutant" "id" "mode" "mutant-summaries" "success")])
    #:init (make-immutable-hash (map list (query-list db-conn "select distinct name from mutators")))
    #:combine cons
    #:default empty
    (values mutator
            (blame-trail mutant
                         trail-id
                         mode
                         mutant-summaries
                         (if use-pre-computed-success?
                             success?
                             (satisfies-BT-hypothesis? mutant-summaries mode))))))

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

(define ((deserialize-mutant-summary benchmark) ms)
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

(define (blame-trail-summaries->blame-trails trail-summaries the-benchmark-data-files)
  (define benchmark-name (benchmark-data-files-name the-benchmark-data-files))
  (define benchmark (benchmark-name->benchmark benchmark-name))
  (define mode-config-name (and (benchmark-data-files-metadata the-benchmark-data-files)
                                (metadata-id-config-name
                                 (file->value
                                  (benchmark-data-files-metadata the-benchmark-data-files)))))
  (map (match-lambda [(blame-trail-summary mod-name
                                           index
                                           id
                                           mutant-summaries)
                      (define summaries
                        (map (compose1 (deserialize-mutant-summary benchmark)
                                       adapt-mutant-summary)
                             mutant-summaries))
                      (blame-trail (mutant benchmark-name mod-name index)
                                   id
                                   mode-config-name
                                   summaries
                                   (satisfies-BT-hypothesis? summaries mode-config-name))])
       trail-summaries))

(define (mutator-names data)
  (hash-keys data))


