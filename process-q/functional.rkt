#lang at-exp racket

(provide (struct-out process-info)
         (contract-out
          [proc-Q? (any/c . -> . boolean?)]
          [proc-Q-empty? (proc-Q? . -> . boolean?)]
          [make-process-Q
           ({(and/c natural? (>/c 0))}
            {any/c}
            . ->* .
            (and/c proc-Q? proc-Q-empty?))]
          [process-info/c contract?]
          [enq-process (proc-Q? (-> process-info/c) . -> . proc-Q?)]
          [rename wait proc-Q-wait
                  (proc-Q? . -> . (and/c proc-Q? proc-Q-empty?))]
          [proc-Q-active-count (proc-Q? . -> . natural?)]
          [proc-Q-data (proc-Q? . -> . any/c)]
          [proc-Q-data-set (proc-Q? any/c . -> . proc-Q?)]

          [close-process-ports! (process-info/c . -> . any)]))

(require "funq.rkt")

(struct proc-Q (active-limit active active-count waiting data))

(define (proc-Q-data-set q v)
  (struct-copy proc-Q q
               [data v]))

(struct process-info (stdout stdin pid stderr ctl will) #:transparent)
(define process-will/c (proc-Q? process-info? . -> . proc-Q?))
(define process-info/c
  (struct/dc process-info
             [stdout (or/c #f input-port?)]
             [stdin (or/c #f input-port?)]
             [pid natural?]
             [stderr (or/c #f input-port?)]
             [ctl ((or/c 'status 'wait 'interrupt 'kill) . -> . any)]
             [will (proc-Q? process-info? . -> . proc-Q?)]))

(define (make-process-Q process-limit [data-init #f])
  (proc-Q process-limit empty 0 empty-Q data-init))

(define (proc-Q-empty? q)
  (and (zero? (proc-Q-active-count q))
       (zero? (Q-size (proc-Q-waiting q)))))

;; start-process should return a process-info?
(define (enq-process q start-process)
  (sweep-dead/spawn-new-processes
   (struct-copy proc-Q q
                [waiting (enq (proc-Q-waiting q) start-process)])))

(define (wait q #:delay [delay 1])
  (let loop ([current-q q])
    (define new-q (sweep-dead/spawn-new-processes current-q))
    (match new-q
      [(struct* proc-Q
                ([active-count 0]
                 [waiting (? empty-Q?)]))
       new-q]
      [else
       (sleep delay)
       (loop new-q)])))

(define (sweep-dead/spawn-new-processes q)
  (define-values {still-active dead}
    (partition (λ (info)
                 (equal? ((process-info-ctl info) 'status)
                         'running))
               (proc-Q-active q)))
  (define still-active-count (length still-active))
  (define temp-q (struct-copy proc-Q q
                              [active still-active]
                              [active-count still-active-count]))
  (define temp-q+wills
    (for/fold ([temp-q+wills temp-q])
              ([dead-proc (in-list dead)])
      ((process-info-will dead-proc) temp-q+wills dead-proc)))
  (define free-spawning-capacity
    (- (proc-Q-active-limit temp-q+wills)
       still-active-count))
  (define procs-waiting-to-spawn
    (Q-size (proc-Q-waiting temp-q+wills)))
  (define procs-to-spawn
    (if (< free-spawning-capacity procs-waiting-to-spawn)
        free-spawning-capacity
        procs-waiting-to-spawn))
  (for/fold ([new-q temp-q+wills])
            ([i (in-range procs-to-spawn)])
    (spawn-next-process new-q)))

(define (spawn-next-process q)
  (match q
    [(proc-Q limit active active-count waiting data)
     (define start-process (Q-first waiting))
     (define the-process-info (start-process))
     (proc-Q limit
             (cons the-process-info active)
             (add1 active-count)
             (Q-rest waiting)
             data)]))

(define (close-process-ports! info)
  (match-define (struct* process-info
                         ([stdout stdout]
                          [stdin stdin]
                          [stderr stderr]))
    info)
  (close-output-port stdin)
  (close-input-port stdout)
  (close-input-port stderr))

(module+ test
  (require ruinit)

  (define (simple-process cmd will)
    (apply process-info
           (append (process cmd)
                   (list will))))

  (test-begin
    #:name basic
    (ignore (define q (make-process-Q 1))
            (define will-called? (box #f))
            (define q1 (enq-process q
                                    (λ _
                                      (simple-process
                                       "sleep 1; echo hi"
                                       (λ (q info)
                                         (set-box! will-called? #t)
                                         (close-process-ports! info)
                                         q))))))
    (test-= (Q-size (proc-Q-waiting q1)) 0)
    (not (unbox will-called?))
    (test-= (proc-Q-active-count q1) 1)

    (ignore (define q1* (wait q1)))
    (test-= (Q-size (proc-Q-waiting q1*)) 0)
    (test-= (Q-size (proc-Q-waiting q1)) 0)
    (unbox will-called?)
    (test-= (proc-Q-active-count q1*) 0)
    (test-= (proc-Q-active-count q1) 1))

  (test-begin
    #:name will
    (ignore (define q (make-process-Q 1))
            (define will-1-called? (box #f))
            (define will-2-called? (box #f))
            (define will-3-called? (box #f))
            (define q1 (enq-process q
                                    (λ _
                                      (simple-process
                                       "sleep 2; echo hi"
                                       (λ (q* info)
                                         (set-box! will-1-called? #t)
                                         (close-process-ports! info)
                                         q*)))))
            (define q2 (enq-process q1
                                    (λ _
                                      (simple-process
                                       "echo good"
                                       (λ (q* info)
                                         (set-box! will-2-called? #t)
                                         (close-process-ports! info)
                                         (enq-process
                                          q*
                                          (λ _
                                            (simple-process
                                             "echo bye"
                                             (λ (q** info)
                                               (set-box! will-3-called? #t)
                                               (close-process-ports! info)
                                               q**))))))))))
    (test-= (proc-Q-active-count q1) 1)
    (test-= (Q-size (proc-Q-waiting q1)) 0)
    (test-= (proc-Q-active-count q2) 1)
    (test-= (Q-size (proc-Q-waiting q2)) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (wait q2)))
    (test-= (Q-size (proc-Q-waiting q1)) 0)
    (test-= (Q-size (proc-Q-waiting q2)) 1)
    (test-= (Q-size (proc-Q-waiting q2*)) 0)
    (test-= (proc-Q-active-count q1) 1)
    (test-= (proc-Q-active-count q2) 1)
    (test-= (proc-Q-active-count q2*) 0)
    (unbox will-1-called?)
    (unbox will-2-called?)
    (unbox will-3-called?))

  (test-begin
    #:name a-little-complex
    (ignore (define q (make-process-Q 2))
            (define wills-called? (vector #f #f #f #f #f))
            (define (will-for i)
              (λ (the-q* info)
                (vector-set! wills-called?
                             i
                             (port->string (process-info-stdout info)))
                (close-process-ports! info)
                (match i
                  [(or 0 2)
                   (define i+ (match i
                                [0 3]
                                [2 4]))
                   (enq-process
                    the-q*
                    (λ _ (simple-process @~a{echo @i+}
                                         (will-for i+))))]
                  [else the-q*])))
            (define the-q
              (for/fold ([the-q q])
                        ([i (in-range 3)])
                (enq-process
                 the-q
                 (λ _
                   (simple-process @~a{echo @i}
                                   (will-for i)))))))
    (test-= (proc-Q-active-count the-q) 2)
    (test-= (Q-size (proc-Q-waiting the-q)) 1)

    (ignore (define the-q* (wait the-q)))
    (test-= (proc-Q-active-count the-q) 2)
    (test-= (Q-size (proc-Q-waiting the-q)) 1)
    (test-= (proc-Q-active-count the-q*) 0)
    (test-= (Q-size (proc-Q-waiting the-q*)) 0)
    (for/and/test ([i (in-range 5)])
                  (test-equal? (vector-ref wills-called? i)
                               (~a i "\n")))))
