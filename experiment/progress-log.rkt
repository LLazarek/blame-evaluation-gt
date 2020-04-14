#lang at-exp racket

(provide initialize-progress-log!)

(define (initialize-progress-log! path #:exists [exists 'replace])
  (define log-out (open-output-file path #:exists exists))
  (file-stream-buffer-mode log-out 'line)
  (define (log-progress! v)
    (writeln v log-out))
  (define (finalize-log!)
    (close-output-port log-out))
  (values log-progress! finalize-log!))
