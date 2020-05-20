#lang at-exp racket

(provide (except-out (struct-out process-Q)
                     process-Q-empty?
                     process-Q-enq
                     process-Q-wait
                     process-Q-active-count
                     process-Q-waiting-count
                     process-Q-data
                     process-Q-get-data
                     process-Q-set-data)
         (struct-out process-info)
         (contract-out
          [rename short:process-Q-empty? process-Q-empty?
                  empty?/c]
          [rename short:process-Q-enq process-Q-enq
                  enq/c]
          [rename short:process-Q-wait process-Q-wait
                  wait/c]
          [rename short:process-Q-active-count process-Q-active-count
                  active-count/c]
          [rename short:process-Q-waiting-count process-Q-waiting-count
                  waiting-count/c]
          [rename short:process-Q-get-data process-Q-get-data
                  get-data/c]
          [rename short:process-Q-set-data process-Q-set-data
                  set-data/c]

          [process-Q/c (contract? . -> . contract?)]

          [process-will/c contract?]
          [process-info/c contract?]))

(module+ internal
  (provide process-Q-data))

(require syntax/parse/define
         (for-syntax racket/syntax))

(struct process-Q (empty?
                   enq
                   wait
                   active-count
                   waiting-count
                   get-data
                   set-data

                   data))

(struct process-info (data ctl will) #:transparent)
(define process-will/c (process-Q? process-info? . -> . process-Q?))
(define process-info/c
  (struct/dc process-info
             [data any/c]
             [ctl ((or/c 'status 'wait 'interrupt 'kill) . -> . any)]
             [will process-will/c]))

(define empty?/c (process-Q? . -> . boolean?))
(define enq/c ({process-Q? (-> process-info/c)}
               {any/c}
               . ->* .
               process-Q?))
(define wait/c (process-Q? . -> . (and/c process-Q? process-Q-empty?)))
(define active-count/c (process-Q? . -> . natural?))
(define waiting-count/c (process-Q? . -> . natural?))
(define get-data/c (process-Q? . -> . any/c))
(define set-data/c (process-Q? any/c . -> . process-Q?))

(define (process-Q/c data/c)
  (struct/dc process-Q
             [empty?         empty?/c]
             [enq            enq/c]
             [wait           wait/c]
             [active-count   active-count/c]
             [waiting-count  waiting-count/c]
             [get-data       get-data/c]
             [set-data       set-data/c]

             [data           (or/c data/c
                                   (box/c data/c))]))

(define-simple-macro (define-method-shorthands prefix:id [field-name:id ...])
  #:with [accessor ...] (map (λ (field-name) (format-id this-syntax
                                                        "process-Q-~a"
                                                        field-name))
                             (syntax-e #'[field-name ...]))
  #:with [shorthand-id ...] (map (λ (accessor) (format-id this-syntax
                                                          "~a~a"
                                                          #'prefix accessor))
                                 (syntax-e #'[accessor ...]))
  (begin
    (define (shorthand-id a-proc-q . other-args)
      (apply (accessor a-proc-q) a-proc-q other-args))
    ...))

(define-method-shorthands short:
  [empty?
   enq
   wait
   active-count
   waiting-count
   get-data
   set-data])
