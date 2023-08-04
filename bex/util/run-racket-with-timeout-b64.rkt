#lang racket/base

(require net/base64
         racket/list
         racket/system)

(define timeout (find-executable-path "timeout"))
(define racket (find-executable-path "timeout"))
(define args (for/list ([s (current-command-line-arguments)])
               (bytes->string/utf-8 (base64-decode s))))
(system* timeout
         (append (list "-k"
                       5
                       (first args)
                       racket)
                 (rest args)))
