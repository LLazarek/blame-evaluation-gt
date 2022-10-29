#lang at-exp racket

(provide sexp-diff
         (struct-out difference)
         (struct-out nothing)
         replace-name-in-sexp)

(require "util.rkt")

(struct difference (left right) #:transparent)
(struct nothing () #:transparent)

;; A B -> (or/c A (difference A B))
(define (compare left right)
  (if (equal? left right)
      left
      (difference left right)))

(define (=length? a b)
  (implies (and (list? a) (list? b))
           (= (length a) (length b))))


;; Dumb diff of s1 and s2, assuming both have the same length or at most one
;; element missing from one and not the other.
;; sexp? sexp? -> sexp?
(define (sexp-diff s1 s2)
  (let loop ([result empty]
             [s1-remaining s1]
             [s2-remaining s2])
    (cond [(=length? s1-remaining s2-remaining)
           (match* {s1-remaining s2-remaining}
             [{(cons head-1 tail-1)
               (cons head-2 tail-2)}
              #:when (=length? head-1 head-2)
              (loop (cons (loop empty head-1 head-2)
                          result)
                    tail-1
                    tail-2)]
             [{(list* first-1 second-1 tail-1)
               (list* first-2 second-2 tail-2)}
              #:when (and (equal? first-1 second-2)
                          (equal? second-1 first-2))
              (loop (append (list (compare second-1 second-2) ;; reverse order because `result`
                                  (compare first-1 first-2))  ;; is being built backwards!
                            result)
                    tail-1
                    tail-2)]
             [{(cons (? list? head-1) tail-1)
               (cons (? list? head-2) tail-2)}
              (loop (cons (compare head-1 head-2)
                          result)
                    tail-1
                    tail-2)]
             [{'() '()}
              (reverse result)]
             [{atom1 atom2}
              (compare   atom1  atom2)])]
          [else
           (define-values {s1* s2*} (mark-drop s1-remaining s2-remaining))
           (assert (=length? s1* s2*) #:name 'sexp-diff
                   "given two sexps that have more than one difference in length")
           (loop result
                 s1*
                 s2*)])))

;; Given two lists that are exactly the same, but one has at most a single element dropped,
;; return copies of the lists such that the dropped element now has the placeholder
;; `(nothing)`.
(define (mark-drop l1 l2)
  (assert (let ([l1-len (length l1)]
                [l2-len (length l2)])
            (or (= l1-len        l2-len)
                (= l1-len        (sub1 l2-len))
                (= (sub1 l1-len) l2-len)))
          #:name 'mark-drop
          @~a{given two lists whose length difference is not <= 1:
                    @~s[l1]
                    @~s[l2]})
  (match* {l1 l2}
    [{'() '()} (values l1 l2)]
    [{(cons one l1-rest)
      (cons two l2-rest)}
     #:when (equal? one two)
     (define-values {l1* l2*} (mark-drop l1-rest l2-rest))
     (values (cons one l1*)
             (cons two l2*))]
    [{(cons one l1-rest)
                (? list?)}
     #:when (equal? l1-rest l2)
     (values l1
             (cons (nothing) l2))]
    [{          (? list?)
      (cons two l2-rest)}
     #:when (equal? l1 l2-rest)
     (values (cons (nothing) l1)
             l2)]))

(module+ test
  (require ruinit)

  (define-test-syntax (test-equal?/2 actual-e expected1 expected2)
    #'(let-values ([{v1 v2} actual-e])
        (and/test (test-equal? v1 expected1)
                  (test-equal? v2 expected2))))

  (test-begin
    #:name mark-drop
    (test-equal?/2 (mark-drop empty empty)
                   empty empty)
    (test-equal?/2 (mark-drop '(1) empty)
                   '(1) (list (nothing)))
    (test-equal?/2 (mark-drop '(1 2) '(1))
                   '(1 2) (list 1 (nothing)))
    (test-equal?/2 (mark-drop '(2 1) '(1))
                   '(2 1) (list (nothing) 1))
    (test-equal?/2 (mark-drop '(1 2 3) '(1 3))
                   '(1 2 3) (list 1 (nothing) 3))
    (test-equal?/2 (mark-drop '(1 3) '(1 2 3))
                   (list 1 (nothing) 3) '(1 2 3)))

  (test-begin
    #:name sexp-diff
    (test-equal? (sexp-diff 1 1)
                 1)
    (test-equal? (sexp-diff 1 2)
                 (difference 1 2))
    (test-equal? (sexp-diff '(1 2 3) '(1 2 3))
                 '(1 2 3))
    (test-equal? (sexp-diff '(1 2 3) '(1 3 2))
                 (list 1 (difference 2 3) (difference 3 2)))
    (test-equal? (sexp-diff '(1 2 (3 4) 5)
                            '(1 2 (4 3) 5))
                 `(1 2 (,(difference 3 4) ,(difference 4 3)) 5))
    (test-equal? (sexp-diff '(1 2 (3 4) (5 (6)))
                            '(1 2 (4 3) (5 (6))))
                 `(1 2 (,(difference 3 4) ,(difference 4 3)) (5 (6))))
    (test-equal? (sexp-diff '((1 2 3))
                            '((1 3)))
                 `(,(difference '(1 2 3) '(1 3))))
    (test-equal? (sexp-diff '(1 2 3)
                            '(1 3))
                 `(1 ,(difference 2 (nothing)) 3))
    (test-equal? (sexp-diff '(-> A B Array)
                            '(-> B A Array))
                 `(-> ,(difference 'A 'B)
                      ,(difference 'B 'A)
                      Array))
    (test-equal? (sexp-diff '(-> (Vectorof Integer) (-> (Vectorof Integer) Float) Array)
                            '(-> (-> (Vectorof Integer) Float) (Vectorof Integer) Array))
                 `(-> ,(difference '(Vectorof Integer)
                                   '(-> (Vectorof Integer) Float))
                      ,(difference '(-> (Vectorof Integer) Float)
                                   '(Vectorof Integer))
                      Array))
    (test-equal? (sexp-diff '(case-> (-> (Listof Indexes) Indexes)
                                     (-> (Listof Indexes) (U #f #t 'permissive) Indexes))
                            '(case-> (-> (Listof Indexes) Indexes)
                                     (-> (U #f #t 'permissive) (Listof Indexes) Indexes)))
                 `(case-> (-> (Listof Indexes) Indexes)
                          (-> ,(difference '(Listof Indexes) '(U #f #t 'permissive))
                              ,(difference '(U #f #t 'permissive) '(Listof Indexes))
                              Indexes)))
    (test-equal? (sexp-diff '(-> Natural (-> String Number) (Listof String))
                            '(-> Natural Any (Listof String)))
                 `(-> Natural ,(difference '(-> String Number) 'Any) (Listof String)))
    (test-equal? (sexp-diff '(-> Natural (-> String Number) (Listof String))
                            '(-> Natural (-> String Number) Any))
                 `(-> Natural (-> String Number) ,(difference '(Listof String) 'Any)))))

;; sexp? symbol? symbol? -> sexp?
(define (replace-name-in-sexp sexp name new-name)
  (let loop ([s sexp])
    (match s
      [(== name)           new-name]
      [(list sub-exps ...) (map loop sub-exps)]
      [other               other])))

(module+ test
  (test-begin
    #:name replace-name-in-sexp
    (test-equal? (replace-name-in-sexp '() 'foo 'bar)
                 '())
    (test-equal? (replace-name-in-sexp '(foo) 'foo 'bar)
                 '(bar))
    (test-equal? (replace-name-in-sexp '(a b foo c) 'foo 'bar)
                 '(a b bar c))
    (test-equal? (replace-name-in-sexp '(a (b foo) c foo) 'foo 'bar)
                 '(a (b bar) c bar))))
