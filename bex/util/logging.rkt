#lang at-exp racket

(provide with-collected-log-messages)

(require racket/logging)

;; Returns at least one value, but possibly more:
;; first the list of messages collected, then the results of `thunk` (however many there are)
(define (with-collected-log-messages logger level topic message-proc
          thunk)
  (define message-list-box (box empty))
  (define (record-message! v)
    (set-box! message-list-box (cons v (unbox message-list-box))))
  (call-with-values
   (λ _ (with-intercepted-logging
          (λ (vec) (record-message! (message-proc vec)))
          thunk
          #:logger logger
          level
          topic))
   (λ results (apply values (reverse (unbox message-list-box)) results))))

(module+ test
  (require ruinit)
  (test-begin
    (ignore (define-logger wclm-test)
            (define-values {messages _}
              (with-collected-log-messages wclm-test-logger 'info 'wclm-test
                (match-lambda [(vector level
                                       str
                                       payload
                                       _)
                               str]
                              [other #f])
                (thunk
                 (for ([i 5])
                   (log-wclm-test-info (~a i)))))))
    (test-equal? messages
                 (build-list 5 (curry ~a "wclm-test: ")))))
