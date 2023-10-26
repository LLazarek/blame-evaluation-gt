#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../../experiment-status.txt")

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
