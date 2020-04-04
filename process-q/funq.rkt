#lang at-exp racket

(provide (contract-out
          [empty-Q (and/c Q? empty-Q?)]
          [empty-Q? (Q? . -> . boolean?)]
          [rename make-Q Q (any/c ... . -> . Q?)]
          [enq (Q? any/c . -> .  (and/c Q? (not/c empty-Q?)))]
          [rename head Q-first ((and/c Q? (not/c empty-Q?)) . -> .  any/c)]
          [rename tail Q-rest ((and/c Q? (not/c empty-Q?)) . -> . Q?)]
          [rename size Q-size (Q? . -> . natural?)]))

(require syntax/parse/define)

;; Banker's Queue
;; based on Okasaki: https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf

;; Invariant: (stream-length front) >= (stream-length rear)
(struct Q (front front-count rear rear-count))

;; Why streams instead of lists?
;; They are equivalent except that the `append` in `maybe-rebalance` is
;; O(n) for lists, but O(1) for streams.
;; Since we're doing the reverse in `maybe-rebalance` anyway (which makes
;; the operation O(n)), this doesn't change the asymptotic complexity
;; but reduces the constant factor.
(define empty-Q (Q empty-stream 0 empty-stream 0))

(define (make-Q . vs)
  (Q (for/stream ([v (in-list vs)]) v)
     (length vs)
     empty-stream
     0))

(define (maybe-rebalance q)
  (match q
    [(Q f fc r rc)
     #:when (< fc rc)
     (define reversed-r (stream-fold (λ (reversed v)
                                       (stream-cons v reversed))
                                     empty-stream
                                     r))
     (Q (stream-append f reversed-r)
        (+ fc rc)
        empty-stream
        0)]
    [else q]))

(define (enq q v)
  (maybe-rebalance
   (struct-copy Q q
                [rear (stream-cons v (Q-rear q))]
                [rear-count (add1 (Q-rear-count q))])))

;; ctc: Q-front-count > 0
(define (head q)
  (stream-first (Q-front q)))

(define (tail q)
  (maybe-rebalance
   (struct-copy Q q
                [front (stream-rest (Q-front q))]
                [front-count (sub1 (Q-front-count q))])))

(define (size q)
  (+ (Q-front-count q) (Q-rear-count q)))

(define (empty-Q? q)
  (zero? (size q)))

(define (eQual? q1 q2)
  (match* {q1 q2}
    [{(Q f1 fc1 r1 rc1) (Q f2 fc2 r2 rc2)}
     #:when (and (= fc1 fc2)
                 (= rc1 rc2))
     (and (for/and ([v1 (in-stream f1)]
                    [v2 (in-stream f2)])
            (equal? v1 v2))
          (for/and ([v1 (in-stream r1)]
                    [v2 (in-stream r2)])
            (equal? v1 v2)))]
    [{_ _} #f]))

(module+ test
  (require ruinit)
  (test-begin
    #:name eQual?
    (eQual? empty-Q empty-Q)
    (eQual? (make-Q 1) (make-Q 1))
    (eQual? (make-Q 1 #f "y") (make-Q 1 #f "y"))
    (not (eQual? (make-Q 1 #f "y") (make-Q 1 "y" #f)))
    (eQual? (make-Q 1) (enq empty-Q 1)))
  (test-begin
    #:name ops
    (ignore (define q0 empty-Q)
            (define q3 (make-Q 1 2 3))
            (define q1 (enq q0 1))
            (define q2 (enq q1 2))
            (define q3* (enq q2 3)))
    (not (eQual? q0 q3))
    (eQual? q3 q3*)
    (test-= (head q2)
            1)
    (test-= (head q3)
            1)
    (test-= (head (tail q3))
            2)))
