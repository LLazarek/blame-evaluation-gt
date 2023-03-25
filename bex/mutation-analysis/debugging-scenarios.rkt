#lang racket/base

(provide (struct-out scenario))

(struct scenario (mutant ; mutant?
                  config ; config/c
                  )
  #:prefab)
