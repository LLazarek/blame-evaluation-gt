#lang at-exp racket/base

(provide read-module
         read-module/port
         replace-stx-location)

(require syntax/modread
         racket/pretty)

(define (read-module path)
  (call-with-input-file path
    read-module/port))

(define (read-module/port input-port
                          #:source [source (object-name input-port)])
  (check-module-form
   (with-module-reading-parameterization
     (λ ()
       (port-count-lines! input-port)
       (read-syntax source input-port)))
   'ignored source))

(define (replace-stx-location stx new-file-name)
  (define-values {read-port write-port} (make-pipe))
  (pretty-write (syntax->datum stx) write-port)
  (read-module/port read-port #:source new-file-name))

(module+ test
  (require racket
           ruinit)
  (define-test-env {setup! cleanup!}
    #:directories ([test-dir "./tmp-test-dir"])
    #:files ([m (build-path test-dir "m.rkt")
                @~a{
                    #lang racket
                    (define x 1)
                    (+ x 2)
                    }]))
  (require syntax/location)
  (test-begin
    #:name read-module
    #:before (setup!)
    #:after (cleanup!)
    (ignore (define m-stx (read-module m)))
    (test-equal? (syntax->datum m-stx)
                 '(module m racket
                    (#%module-begin
                     (define x 1)
                     (+ x 2))))
    (test-equal? (syntax-source-file-name m-stx)
                 (file-name-from-path m))
    (test-equal? (syntax-source m-stx)
                 m))
  (test-begin
    #:name read-module/port
    #:before (setup!)
    #:after (cleanup!)
    (ignore
     (define other-mod.rkt "some-other-mod.rkt")
     (define-values {read-port write-port}
       (make-pipe #f other-mod.rkt))
     (pretty-write '(module m racket
                    (#%module-begin
                     (define x 1)
                     (+ x 2)))
                   write-port)
     (define m-stx (read-module/port read-port)))
    (test-equal? (syntax->datum m-stx)
                 '(module m racket
                    (#%module-begin
                     (define x 1)
                     (+ x 2))))
    (test-equal? (syntax-source-file-name m-stx)
                 (file-name-from-path other-mod.rkt))
    (test-equal? (syntax-source m-stx)
                 other-mod.rkt)


    (ignore
     (define-values {read-port write-port} (make-pipe))
     (pretty-write '(module m racket
                      (#%module-begin
                       (define x 1)
                       (+ x 2)))
                   write-port)
     (define m-stx (read-module/port read-port
                                     #:source other-mod.rkt)))
    (test-equal? (syntax->datum m-stx)
                 '(module m racket
                    (#%module-begin
                     (define x 1)
                     (+ x 2))))
    (test-equal? (syntax-source-file-name m-stx)
                 (file-name-from-path other-mod.rkt))
    (test-equal? (syntax-source m-stx)
                 other-mod.rkt)))
