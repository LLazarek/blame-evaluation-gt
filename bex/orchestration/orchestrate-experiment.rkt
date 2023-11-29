#lang s-exp "experiment-lang.rkt"

(require "experiment-info.rkt")

(define-runtime-path status-file "../../../experiment-status.txt")

(with-configuration [zythos-local/one-job-per-mutant/batched
                     blutil]
  #:status-in status-file
  #:manual-outcome-recording
  (run-mode blame #:record-outcomes)
  (run-mode stack)
  (run-mode null))
