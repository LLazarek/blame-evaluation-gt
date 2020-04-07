#lang at-exp racket

(provide initialize-progress-log!)

(define (initialize-progress-log! path)
  (define log-out (open-output-file path #:exists 'replace))
  (file-stream-buffer-mode log-out 'line)
  (define (log-progress! v)
    (writeln v log-out))
  (define (finalize-log!)
    (close-output-port log-out))
  (values log-progress! finalize-log!))
