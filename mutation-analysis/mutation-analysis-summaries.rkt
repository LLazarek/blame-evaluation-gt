#lang at-exp racket/base

(provide mutation-analysis-summaries-db
         (struct-out summary))

(require racket/runtime-path)

(define-runtime-path default-summaries-db "summaries/default.rktdb")
(define mutation-analysis-summaries-db (make-parameter default-summaries-db))

(struct summary (valid-indices      ; (hash/c mutator-name? (listof natural?))
                 max-index          ; natural?
                 triggered-mutators ; (listof mutator-name?)
                 )
  #:prefab)

