#lang at-exp racket/base

(provide mutation-analysis-summaries-db
         (struct-out summary)
         module-summaries->benchmark-summary
         (struct-out benchmark-summary)
         mutator-name?
         read-mutants-by-mutator)

(require racket/runtime-path
         racket/hash
         racket/contract
         (prefix-in db: "../db/db.rkt")
         "../util/mutant-util.rkt")

(define-runtime-path default-summaries-db "summaries/default.rktdb")
(define mutation-analysis-summaries-db (make-parameter default-summaries-db))

(define mutator-name? string?)

(struct summary (valid-indices      ; (hash/c mutator-name? (listof natural?))
                 max-index          ; natural?
                 triggered-mutators ; (listof mutator-name?)
                 )
  #:prefab)

;; This contains the same information as the hash table
;; containing summaries produced by `summarize-mutation-analysis.rkt`
;; but in a form that makes sampling much easier
;; since we sample mutants from a benchmark *across modules*.
(struct benchmark-summary
  (mutants-by-mutator ; (hash/c mutator-name? (listof mutant?))
   )
  #:prefab)

;; (hash/c module-name? summary?) -> benchmark-summary?
(define (module-summaries->benchmark-summary module-summaries)
  (define ((mutant-for mod-name) index) (mutant #f mod-name index))
  (define (module-summary->benchmark-summary mod-name mod-summary)
    (for/hash ([{mutator indices} (in-hash
                                   (summary-valid-indices mod-summary))])
      (values mutator
              (map (mutant-for mod-name) indices))))

  (define all-mutants-by-mutator
    (for/fold ([all-mutants-by-mutator (hash)])
              ([{mod-name mod-summary} (in-hash module-summaries)])
      (define summary-for-this-mod
        (module-summary->benchmark-summary mod-name mod-summary))
      (hash-union all-mutants-by-mutator
                  summary-for-this-mod
                  #:combine append)))
  (benchmark-summary all-mutants-by-mutator))

(define/contract (read-mutants-by-mutator summaries-db)
  (db:path-to-db? . -> . (hash/c mutant? mutator-name?))

  (define db (db:get summaries-db))
  (for*/hash ([benchmark (in-list (db:keys db))]
              [{mod mod-summary} (in-hash (db:read db benchmark))]
              [{mutator-name indices} (in-hash (summary-valid-indices mod-summary))]
              [index (in-list indices)])
    (values (mutant benchmark mod index) mutator-name)))
