#lang at-exp racket/base

(require (prefix-in db: "db.rkt"))

(provide db-path-relative-to?)

(define ((db-path-relative-to? base) path)
  (define resolved (build-path base path))
  (db:path-to-db? resolved))
