#lang at-exp racket

(provide (contract-out
          [make-process-Q
           ({(and/c natural? (>/c 0))}
            {any/c}
            . ->* .
            (and/c process-Q?
                   fun-process-Q?
                   process-Q-empty?))]
          [fun-process-Q? (any/c . -> . boolean?)]))

(require "funq.rkt"
         "interface.rkt")

(struct fun-process-Q process-Q (active-limit active active-count waiting))

(define (fun-process-Q-set-data q v)
  (struct-copy fun-process-Q q
               [data #:parent process-Q v]))

(define (make-process-Q process-limit [data-init #f])
  (fun-process-Q fun-process-Q-empty?
                 enq-process
                 wait
                 fun-process-Q-active-count
                 fun-process-Q-waiting-count
                 process-Q-data
                 fun-process-Q-set-data

                 data-init

                 process-limit
                 empty
                 0
                 empty-Q))

(define (fun-process-Q-empty? q)
  (and (zero? (fun-process-Q-active-count q))
       (zero? (Q-size (fun-process-Q-waiting q)))))

;; start-process should return a process-info?
(define (enq-process q start-process)
  (sweep-dead/spawn-new-processes
   (struct-copy fun-process-Q q
                [waiting (enq (fun-process-Q-waiting q) start-process)])))

(define (wait q #:delay [delay 1])
  (let loop ([current-q q])
    (define new-q (sweep-dead/spawn-new-processes current-q))
    (match new-q
      [(struct* fun-process-Q
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
               (fun-process-Q-active q)))
  (define temp-q (struct-copy fun-process-Q q
                              [active still-active]
                              [active-count (length still-active)]))
  (define temp-q+wills
    (for/fold ([temp-q+wills temp-q])
              ([dead-proc (in-list dead)])
      ((process-info-will dead-proc) temp-q+wills dead-proc)))
  (define free-spawning-capacity
    (- (fun-process-Q-active-limit temp-q+wills)
       (fun-process-Q-active-count temp-q+wills)))
  (define procs-waiting-to-spawn
    (Q-size (fun-process-Q-waiting temp-q+wills)))
  (define procs-to-spawn
    (if (< free-spawning-capacity procs-waiting-to-spawn)
        free-spawning-capacity
        procs-waiting-to-spawn))
  (for/fold ([new-q temp-q+wills])
            ([i (in-range procs-to-spawn)])
    (spawn-next-process new-q)))

(define (spawn-next-process q)
  (match q
    [(struct* fun-process-Q
              ([active-limit limit]
               [active active]
               [active-count active-count]
               [waiting waiting]))
     (define data (process-Q-data q))
     (define start-process (Q-first waiting))
     (define the-process-info (start-process))
     (struct-copy fun-process-Q q
                  [active (cons the-process-info active)]
                  [active-count (add1 active-count)]
                  [waiting (Q-rest waiting)])]))

(define (fun-process-Q-waiting-count q)
  (Q-size (fun-process-Q-waiting q)))


(module+ test
  (require ruinit)

  (struct simple-process-info (stdout stdin pid stderr)
    #:transparent)
  (define (simple-process cmd will)
    (define proc-info+ctl (process cmd))
    (process-info (apply simple-process-info
                         (drop-right proc-info+ctl 1))
                  (last proc-info+ctl)
                  will))

  (define (close-process-ports! info)
    (match-define (struct* process-info
                           ([data (struct* simple-process-info
                                           ([stdout stdout]
                                            [stdin stdin]
                                            [stderr stderr]))]))
      info)
    (close-output-port stdin)
    (close-input-port stdout)
    (close-input-port stderr))

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
    (test-= (Q-size (fun-process-Q-waiting q1)) 0)
    (not (unbox will-called?))
    (test-= (fun-process-Q-active-count q1) 1)

    (ignore (define q1* (wait q1)))
    (test-= (Q-size (fun-process-Q-waiting q1*)) 0)
    (test-= (Q-size (fun-process-Q-waiting q1)) 0)
    (unbox will-called?)
    (test-= (fun-process-Q-active-count q1*) 0)
    (test-= (fun-process-Q-active-count q1) 1))

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
    (test-= (fun-process-Q-active-count q1) 1)
    (test-= (Q-size (fun-process-Q-waiting q1)) 0)
    (test-= (fun-process-Q-active-count q2) 1)
    (test-= (Q-size (fun-process-Q-waiting q2)) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (wait q2)))
    (test-= (Q-size (fun-process-Q-waiting q1)) 0)
    (test-= (Q-size (fun-process-Q-waiting q2)) 1)
    (test-= (Q-size (fun-process-Q-waiting q2*)) 0)
    (test-= (fun-process-Q-active-count q1) 1)
    (test-= (fun-process-Q-active-count q2) 1)
    (test-= (fun-process-Q-active-count q2*) 0)
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
                             (port->string (simple-process-info-stdout
                                            (process-info-data info))))
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
    (test-= (fun-process-Q-active-count the-q) 2)
    (test-= (Q-size (fun-process-Q-waiting the-q)) 1)

    (ignore (define the-q* (wait the-q)))
    (test-= (fun-process-Q-active-count the-q) 2)
    (test-= (Q-size (fun-process-Q-waiting the-q)) 1)
    (test-= (fun-process-Q-active-count the-q*) 0)
    (test-= (Q-size (fun-process-Q-waiting the-q*)) 0)
    (for/and/test ([i (in-range 5)])
                  (test-equal? (vector-ref wills-called? i)
                               (~a i "\n"))))

  (test-begin
    #:name wait
    (fun-process-Q-empty? (wait (make-process-Q 2))))

  (test-begin
    #:name process-limit
    ;; observed bug:
    ;; You have three active procs,
    ;; 0. running, will: nothing
    ;; 1. done, will: spawn another
    ;; 2. running, will: nothing
    ;;
    ;; And one waiting proc,
    ;; 3. waiting, will: nothing
    ;;
    ;; Now you call wait. It sweeps 1, and executes its will which enq's another
    ;; proc (4), which is waiting at first, but then sweep is called again,
    ;; which decides to spawn 3 because only two procs are active, filling the Q
    ;; back up to three active and that sweep call returns. But now we return to
    ;; the first sweep call, which says that it has a `free-spawning-capacity`
    ;; of 1 and pulls 4 off the waiting list and spawns a proc, making four
    ;; active procs at once!
    (ignore (define current-active (box 0))
            (define active-history (box empty))
            (define (record-active-proc! do)
              (define new-active (do (unbox current-active)))
              (set-box! current-active new-active)
              (set-box! active-history
                        (cons new-active
                              (unbox active-history))))
            (define (enq-proc! q i)
              (enq-process q
                           (λ _
                             (record-active-proc! add1)
                             (simple-process
                              (match i
                                [2 "echo done"]
                                [else "sleep 2"])
                              (λ (q* info)
                                (record-active-proc! sub1)
                                (close-process-ports! info)
                                (match i
                                  [2 (enq-proc! q* 4)]
                                  [else q*]))))))
            (define q (for/fold ([q (make-process-Q 3)])
                                ([i (in-range 4)])
                        (enq-proc! q i)))
            (define q* (wait q)))
    (extend-test-message
     (not (findf (>/c 3) (unbox active-history)))
     "process-Q spawns active processes exceeding the process limit"))

  (test-begin
    #:name wait/long-running-procs
    (ignore
     (define done-vec (vector #f #f #f #f))
     (define q (for/fold ([q (make-process-Q 2)])
                         ([i (in-range 4)])
                 (enq-process q
                              (λ _
                                (simple-process
                                 "sleep 10"
                                 (λ (q* info)
                                   (close-process-ports! info)
                                   (vector-set! done-vec i #t)
                                   q*))))))
            (define q/done (wait q)))
    (fun-process-Q-empty? q/done)
    (andmap identity (vector->list done-vec))))
