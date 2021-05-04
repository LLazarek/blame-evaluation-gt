#lang racket

(provide bt-id?
         bt->id
         bts-by-id)

(require "read-data.rkt")

(define bt-id? (list/c mutant? natural?))

(define/contract (bt->id bt)
  (blame-trail? . -> . bt-id?)

  (list (blame-trail-mutant-id bt)
        (blame-trail-trail-id bt)))

(define/contract (bts-by-id bts-by-mutator)
  ((hash/c string? (listof blame-trail?)) . -> . (hash/c bt-id? blame-trail?))

  (for*/hash ([bts (in-hash-values bts-by-mutator)]
              [bt (in-list bts)])
    (values (bt->id bt) bt)))
