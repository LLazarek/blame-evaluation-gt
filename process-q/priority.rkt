#lang at-exp racket

(provide (contract-out
          [make-process-Q
           ({(and/c natural? (>/c 0))}
            {any/c}
            . ->* .
            (and/c process-Q?
                   priority-process-Q?
                   process-Q-empty?))]
          [priority-process-Q? (any/c . -> . boolean?)]))

(require (prefix-in PQ: pfds/heap/pairing)
         "interface.rkt"
         (submod "interface.rkt" internal))

(struct priority-process-Q process-Q (active-limit

                                      active
                                      active-count

                                      waiting
                                      waiting-count))

(define (priority-process-Q-set-data q v)
  (struct-copy priority-process-Q q
               [data #:parent process-Q v]))

(struct process (thunk priority))
(define (process-priority< a b)
  (< (process-priority a)
     (process-priority b)))

(define (make-process-Q process-limit [data-init #f])
  (priority-process-Q priority-process-Q-empty?
                 enq-process
                 wait
                 priority-process-Q-active-count
                 priority-process-Q-waiting-count
                 process-Q-data
                 priority-process-Q-set-data

                 data-init

                 process-limit
                 empty
                 0
                 (PQ:heap process-priority<)
                 0))

(define (priority-process-Q-empty? q)
  (and (zero? (priority-process-Q-active-count q))
       (zero? (priority-process-Q-waiting-count q))))

;; start-process should return a process-info?
(define (enq-process q start-process [priority 0])
  (sweep-dead/spawn-new-processes
   (struct-copy priority-process-Q q
                [waiting (PQ:insert (process start-process priority)
                                    (priority-process-Q-waiting q))]
                [waiting-count (add1 (priority-process-Q-waiting-count q))])))

(define (wait q #:delay [delay 1])
  (let loop ([current-q q])
    (define new-q (sweep-dead/spawn-new-processes current-q))
    (match new-q
      [(struct* priority-process-Q
                ([active-count  0]
                 [waiting-count 0]))
       new-q]
      [else
       (sleep delay)
       (loop new-q)])))

(define (sweep-dead/spawn-new-processes q)
  (define-values {still-active dead}
    (partition (λ (info)
                 (equal? ((process-info-ctl info) 'status)
                         'running))
               (priority-process-Q-active q)))
  (define temp-q (struct-copy priority-process-Q q
                              [active still-active]
                              [active-count (length still-active)]))
  (define temp-q+wills
    (for/fold ([temp-q+wills temp-q])
              ([dead-proc (in-list dead)])
      ((process-info-will dead-proc) temp-q+wills dead-proc)))
  (define free-spawning-capacity
    (- (priority-process-Q-active-limit temp-q+wills)
       (priority-process-Q-active-count temp-q+wills)))
  (define procs-waiting-to-spawn
    (priority-process-Q-waiting-count temp-q+wills))
  (define procs-to-spawn
    (if (< free-spawning-capacity procs-waiting-to-spawn)
        free-spawning-capacity
        procs-waiting-to-spawn))
  (for/fold ([new-q temp-q+wills])
            ([i (in-range procs-to-spawn)])
    (spawn-next-process new-q)))

(define (spawn-next-process q)
  (match q
    [(struct* priority-process-Q
              ([active-limit limit]
               [active active]
               [active-count active-count]
               [waiting waiting]
               [waiting-count waiting-count]))
     (define data (process-Q-data q))
     (define start-process (process-thunk (PQ:find-min/max waiting)))
     (define the-process-info (start-process))
     (struct-copy priority-process-Q q
                  [active (cons the-process-info active)]
                  [active-count (add1 active-count)]
                  [waiting (PQ:delete-min/max waiting)]
                  [waiting-count (sub1 waiting-count)])]))


(module+ test
  (require ruinit
           (prefix-in r/sys: racket/system))

  (struct simple-process-info (stdout stdin pid stderr)
    #:transparent)
  (define (simple-process cmd will)
    (define proc-info+ctl (r/sys:process cmd))
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
                                         q)))
                                    0)))
    (test-= (priority-process-Q-waiting-count q1) 0)
    (not (unbox will-called?))
    (test-= (priority-process-Q-active-count q1) 1)

    (ignore (define q1* (wait q1)))
    (test-= (priority-process-Q-waiting-count q1*) 0)
    (test-= (priority-process-Q-waiting-count q1) 0)
    (unbox will-called?)
    (test-= (priority-process-Q-active-count q1*) 0)
    (test-= (priority-process-Q-active-count q1) 1))

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
                                         q*)))
                                    0))
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
                                               q**)))
                                          0))))
                                    0)))
    (test-= (priority-process-Q-active-count q1) 1)
    (test-= (priority-process-Q-waiting-count q1) 0)
    (test-= (priority-process-Q-active-count q2) 1)
    (test-= (priority-process-Q-waiting-count q2) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (wait q2)))
    (test-= (priority-process-Q-waiting-count q1) 0)
    (test-= (priority-process-Q-waiting-count q2) 1)
    (test-= (priority-process-Q-waiting-count q2*) 0)
    (test-= (priority-process-Q-active-count q1) 1)
    (test-= (priority-process-Q-active-count q2) 1)
    (test-= (priority-process-Q-active-count q2*) 0)
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
                                         (will-for i+)))
                    0)]
                  [else the-q*])))
            (define the-q
              (for/fold ([the-q q])
                        ([i (in-range 3)])
                (enq-process
                 the-q
                 (λ _
                   (simple-process @~a{echo @i}
                                   (will-for i)))
                 0))))
    (test-= (priority-process-Q-active-count the-q) 2)
    (test-= (priority-process-Q-waiting-count the-q) 1)

    (ignore (define the-q* (wait the-q)))
    (test-= (priority-process-Q-active-count the-q) 2)
    (test-= (priority-process-Q-waiting-count the-q) 1)
    (test-= (priority-process-Q-active-count the-q*) 0)
    (test-= (priority-process-Q-waiting-count the-q*) 0)
    (for/and/test ([i (in-range 5)])
                  (test-equal? (vector-ref wills-called? i)
                               (~a i "\n"))))

  (test-begin
    #:name wait
    (priority-process-Q-empty? (wait (make-process-Q 2))))

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
                                  [else q*]))))
                           0))
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
                                   q*)))
                              0)))
            (define q/done (wait q)))
    (priority-process-Q-empty? q/done)
    (andmap identity (vector->list done-vec)))

  (test-begin
    #:name priority-simple
    (ignore
     (define order-box (box 0))
     (define spawn-vec (vector #f #f #f #f))
     (define (record-spawned! index)
       (define position (unbox order-box))
       (set-box! order-box (add1 position))
       (vector-set! spawn-vec index position))
     (define q (for/fold ([q (make-process-Q 2)])
                         ([i (in-range 4)])
                 (enq-process q
                              (λ _
                                (record-spawned! i)
                                (simple-process
                                 "echo done"
                                 (λ (q* info)
                                   (close-process-ports! info)
                                   q*)))
                              (match i
                                [2 3]
                                [3 2]
                                [else i]))))
     (define q/done (wait q)))
    (priority-process-Q-empty? q/done)
    (test-equal? spawn-vec
                 #(0 1 3 2)))

  (test-begin
    #:name priority-children
    (ignore
     (define order-box (box 0))
     (define spawn-vec (vector #f #f #f #f #f))
     (define (record-spawned! index)
       (define position (unbox order-box))
       (set-box! order-box (add1 position))
       (vector-set! spawn-vec index position))
     (define (enq-proc q i [priority i])
       (enq-process q
                    (λ _
                      (record-spawned! i)
                      (simple-process
                       (match i
                         [1 "sleep 1"]
                         [else "echo done"])
                       (λ (q* info)
                         (close-process-ports! info)
                         (match i
                           [0 (enq-proc q* 4 0)]
                           [else q*]))))
                    priority))
     (define q (for/fold ([q (make-process-Q 2)])
                         ([i (in-range 4)])
                 (enq-proc q i)))
     (define q/done (wait q)))
    (priority-process-Q-empty? q/done)
    (test-match spawn-vec
                (vector 0
                        1
                        _
                        _
                        2))))
