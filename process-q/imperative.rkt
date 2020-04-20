#lang at-exp racket

(provide (contract-out
          [make-process-Q
           ({(and/c natural? (>/c 0))}
            {any/c}
            . ->* .
            (and/c process-Q?
                   imperative-process-Q?
                   process-Q-empty?))]
          [imperative-process-Q? (any/c . -> . boolean?)]))

(require "interface.rkt"
         (submod "interface.rkt" internal)
         data/queue)

(struct imperative-process-Q process-Q (active-limit
                                        active
                                        waiting))

;; Async management allows dead processes to be recognized asynchronously and
;; replaced by procs already on the waiting Q. However, it's still the case that
;; the only way to execute process wills is by calling `enq-process!` or `wait`.
;; Hence, it's only likely to make a difference in a few cases:
;; 1. Processes complete much faster than calls to `enq-process!`,
;;    and for some reason you can't block on `wait`.
;;    If this is the case, #:manage-async? is preferable because it manages
;;    synchronization of internal data structures where calling `wait` in a
;;    thread will *not*.
;; 2. Wills don't matter.
;;
;; At least for my current use-case, neither of these are true so I'm going to
;; drop it for now.
(define (make-process-Q process-limit [data-init #f])
  (define active-set (mutable-set))
  (define waiting-Q (make-queue))
  (imperative-process-Q imperative-process-Q-empty?
                        enq-process!
                        wait
                        imperative-process-Q-active-count
                        imperative-process-Q-waiting-count
                        imperative-process-Q-get-mutable-data
                        imperative-process-Q-set-mutable-data!

                        (box data-init)

                        process-limit
                        active-set
                        waiting-Q))

(define (imperative-process-Q-get-mutable-data q)
  (unbox (process-Q-data q)))

(define (imperative-process-Q-set-mutable-data! q v)
  (set-box! (process-Q-data q) v)
  q)

(define imperative-process-Q-active-count
  (compose1 set-count imperative-process-Q-active))
(define imperative-process-Q-waiting-count
  (compose1 queue-length imperative-process-Q-waiting))

(define (imperative-process-Q-empty? q)
  (and (zero? (imperative-process-Q-active-count q))
       (queue-empty? (imperative-process-Q-waiting q))))

;; start-process should return a process-info?
(define (enq-process! q start-process)
  (enqueue! (imperative-process-Q-waiting q) start-process)
  (sweep-dead/spawn-new-processes! q)
  q)

(define (wait q #:delay [delay 1])
  (let loop ()
    (sleep 1)
    (match q
      [(? imperative-process-Q-empty?) q]
      [q
       (sweep-dead/spawn-new-processes! q)
       (loop)])))

(define (sweep-dead/spawn-new-processes! q)
  (define active-set (imperative-process-Q-active q))
  (define dead-set
    (for/set (;; Avoiding `in-mutable-set` in case modification messes with it
              [info (in-list (set->list active-set))]
              #:unless (equal? ((process-info-ctl info) 'status)
                               'running))
      (set-remove! active-set info)
      info))

  (for ([dead-proc (in-set dead-set)])
    ((process-info-will dead-proc) q dead-proc))
  (define free-spawning-capacity
    (- (imperative-process-Q-active-limit q)
       (imperative-process-Q-active-count q)))
  (define procs-waiting-to-spawn
    (imperative-process-Q-waiting-count q))
  (define procs-to-spawn
    (if (< free-spawning-capacity procs-waiting-to-spawn)
        free-spawning-capacity
        procs-waiting-to-spawn))
  (displayln
   @~a{
       Spawning @procs-to-spawn procs because there are @;
       @procs-waiting-to-spawn waiting to spawn and @;
       @(imperative-process-Q-active-count q) @;
       / @;
       @(imperative-process-Q-active-limit q) @;
       active right now
       })
  (for ([i (in-range procs-to-spawn)])
    (spawn-next-process! q)))

(define (spawn-next-process! q)
  #;(->i ([q imperative-process-Q?])
       #:pre/desc {q}
       (or (and (not (zero? (imperative-process-Q-waiting-count q)))
                (>= (- (imperative-process-Q-active-limit q)
                       (imperative-process-Q-active-count q))
                    1))
           "q must be able to spawn another process")
       any)

  (define start-next-process (dequeue! (imperative-process-Q-waiting q)))
  (define the-process-info (start-next-process))
  (set-add! (imperative-process-Q-active q)
            the-process-info))


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
            (define q1 (enq-process! q
                                     (λ _
                                       (simple-process
                                        "sleep 1; echo hi"
                                        (λ (q info)
                                          (set-box! will-called? #t)
                                          (close-process-ports! info)
                                          q))))))
    (test-equal? (process-Q-get-data q) #f)

    (test-eq? q q1)
    (test-= (imperative-process-Q-waiting-count q) 0)
    (not (unbox will-called?))
    (test-= (imperative-process-Q-active-count q) 1)

    (ignore (define q1* (wait q1)))
    (test-eq? q q1*)
    (test-= (imperative-process-Q-waiting-count q) 0)
    (unbox will-called?)
    (test-= (imperative-process-Q-active-count q) 0))

  (test-begin
    #:name will
    (ignore (define q (make-process-Q 1))
            (define will-1-called? (box #f))
            (define will-2-called? (box #f))
            (define will-3-called? (box #f))
            (define q1 (enq-process! q
                                     (λ _
                                       (simple-process
                                        "sleep 2; echo hi"
                                        (λ (q* info)
                                          (set-box! will-1-called? #t)
                                          (close-process-ports! info)
                                          q*)))))
            (define q2 (enq-process! q1
                                     (λ _
                                       (simple-process
                                        "echo good"
                                        (λ (q* info)
                                          (set-box! will-2-called? #t)
                                          (close-process-ports! info)
                                          (enq-process!
                                           q*
                                           (λ _
                                             (simple-process
                                              "echo bye"
                                              (λ (q** info)
                                                (set-box! will-3-called? #t)
                                                (close-process-ports! info)
                                                q**))))))))))
    (test-eq? q q1)
    (test-eq? q q2)
    (test-= (imperative-process-Q-active-count q) 1)
    (test-= (imperative-process-Q-waiting-count q) 1)
    (not (unbox will-1-called?))
    (not (unbox will-2-called?))
    (not (unbox will-3-called?))

    (ignore (define q2* (wait q2)))
    (test-eq? q q2*)
    (test-= (imperative-process-Q-waiting-count q) 0)
    (test-= (imperative-process-Q-active-count q) 0)
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
                   (enq-process!
                    the-q*
                    (λ _ (simple-process @~a{echo @i+}
                                         (will-for i+))))]
                  [else the-q*])))
            (define the-q
              (for/fold ([the-q q])
                        ([i (in-range 3)])
                (enq-process!
                 the-q
                 (λ _
                   (simple-process @~a{echo @i}
                                   (will-for i)))))))
    (test-eq? q the-q)
    (test-= (imperative-process-Q-active-count the-q) 2)
    (test-= (imperative-process-Q-waiting-count the-q) 1)

    (ignore (define the-q* (wait the-q)))
    (test-eq? q the-q*)
    (test-= (imperative-process-Q-active-count q) 0)
    (test-= (imperative-process-Q-waiting-count q) 0)
    (for/and/test ([i (in-range 5)])
                  (test-equal? (vector-ref wills-called? i)
                               (~a i "\n"))))

  (test-begin
    #:name wait
    (imperative-process-Q-empty? (wait (make-process-Q 2))))

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
              (enq-process! q
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
     "process-Q spawns active processes exceeding the process limit")
    (extend-test-message
     (findf (=/c 3) (unbox active-history))
     "process-Q doesn't spawns active processes up to the process limit"))

  (test-begin
      #:name wait/long-running-procs
      (ignore
       (define done-vec (vector #f #f #f #f))
       (define q (for/fold ([q (make-process-Q 2)])
                           ([i (in-range 4)])
                   (enq-process! q
                                 (λ _
                                   (simple-process
                                    "sleep 10"
                                    (λ (q* info)
                                      (close-process-ports! info)
                                      (vector-set! done-vec i #t)
                                      q*))))))
       (define q/done (wait q)))
      (imperative-process-Q-empty? q/done)
      (andmap identity (vector->list done-vec))))
