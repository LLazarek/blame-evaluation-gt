#lang at-exp racket/base

(provide (struct-out mutant-summary)
         (struct-out blame-trail-summary))

(struct mutant-summary (id
                        run-status
                        config)
  #:prefab)
(struct blame-trail-summary (mutated-module-name
                             mutation-index
                             id
                             mutants)
  #:prefab)
