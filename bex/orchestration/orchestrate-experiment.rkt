#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../experiment-status.txt")

(provide type-mistakes)
(define type-mistakes
  (experiment-config dbs:type-api-mutations
                     data:type-api-mistakes))
(provide code-mistakes)
(define code-mistakes
  (experiment-config dbs:blgt-erasure-biased-thesis
                     data:blgt-erasure-biased-thesis))

(with-configuration [(begin0 zythos
                       (set-field! enabled-machines
                                   zythos
                                   '("fix" "allagash")))
                     code-mistakes]
  #:status-in status-file
  (run-mode TR)
  (run-mode TR-stack-first)
  (run-mode TR-null)
  (run-mode transient-newest)
  (run-mode transient-oldest)
  (run-mode transient-stack-first)
  (run-mode erasure-stack-first))
