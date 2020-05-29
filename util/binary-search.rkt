#lang at-exp racket

(provide INDEX-SEARCH-RANGE
         lowest-upper-bound-binary-search
         (struct-out result)
         (struct-out try-go-lower)
         (struct-out go-lower)
         (struct-out go-higher)
         (struct-out exhausted))

(define INDEX-SEARCH-RANGE 20100)

(struct result (index value) #:prefab)
(struct try-go-lower (value) #:prefab)
;; (struct try-go-higher (value) #:prefab)
(struct go-lower () #:prefab)
(struct go-higher () #:prefab)
(struct exhausted () #:prefab)
;; Finds the lowest index for which `index-result` returns `try-go-lower?`
;; between `base-min` and `base-max`. For such an index where `index-result`
;; returns `(try-go-lower v)`, this function returns `(result index v)`.
;;
;; Diagramatically, the space of numbers looks like this:
;;  |------------|------------|----------|
;; min           a            b         max
;; And suppose that in this space, `index-result` produces these results:
;;  >>>>>>>>>>>>>~~~~~~~~~~~~~~<<<<<<<<<<<
;; min           a            b         max
;;     where `>` means `go-higher?`
;;           `~` means `try-go-lower?`
;;           `<` means `go-lower?`
;;
;; Then this function will find `a`.
;;
;; Edge cases:
;; If no such index can be found between `min` and `max` and `increase-max?`
;; is #f, returns `(result i (exhausted))`, where `i` is the highest index
;; for which `index-result` returns `go-higher?`.
;; Diagramatically, it finds `a` for:
;;  >>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<
;; min           a                      max
;;
;; If `index-result` only returns `go-lower?`, the resulting index will
;; instead be `(sub1 min)`.
;; Diagramatically, it finds `(sub1 min)` for:
;;  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;; min                                  max
;;
;; If `increase-max?` is #t and all results of `index-result` in `[min, max]`
;; are `go-higher?`, then the search is repeated on `[max, max+max]`.
;; And again if the same happens in that range.
;; If `index-result` never returns anything other than `go-higher?` on
;; `[0, +inf.0]` then this never terminates.
;; Diagramatically, it finds a` for:
;;  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~~~~<<<<<<<<<<<<<<<<<<<<
;; min                                  max        a                    2*max
(define ((lowest-upper-bound-binary-search index-result)
         #:min [base-min 0]
         #:max [base-max INDEX-SEARCH-RANGE]
         #:default-value [default-end-value (exhausted)]
         #:increase-max? [increase-max? #f])
  (let search ([min base-min]
               [max base-max]
               [lowest-end-value-found (result +inf.0 default-end-value)])
    (cond [(and (> min max)
                increase-max?
                (zero? (modulo max base-max)))
           (search min
                   (+ max base-max)
                   lowest-end-value-found)]
          [(> min max)
           (match lowest-end-value-found
             [(result +inf.0 v)
              (result max v)]
             [else lowest-end-value-found])]
          [else
           (define index-to-try (+ min (quotient (- max min) 2)))
           (define lowest-index (result-index lowest-end-value-found))
           (match (index-result index-to-try)
             [(go-lower)
              (search min
                      (sub1 index-to-try)
                      lowest-end-value-found)]
             [(go-higher)
              (search (add1 index-to-try)
                      max
                      lowest-end-value-found)]
             [(try-go-lower value)
              (search min
                      (sub1 index-to-try)
                      (if (< index-to-try lowest-index)
                          (result index-to-try value)
                          lowest-end-value-found))]
             #;[(try-go-higher value)
                (search (add1 index-to-try)
                        max
                        (if (< index-to-try lowest-index)
                            (result index-to-try value)
                            lowest-end-value-found))])])))

(module+ test
  (require ruinit)
  (test-begin
    #:name lowest-upper-bound-binary-search
    (ignore (define search
              (lowest-upper-bound-binary-search
               (match-lambda
                 [(? (</c 40)) (go-higher)]
                 [(? (and/c (>=/c 40) (<=/c 60)) n) (try-go-lower (- n))]
                 [else (go-lower)]))))
    (test-match (search)
                (result 40 -40))
    (test-match (search #:min 70)
                (result 69 (exhausted)))
    (test-match (search #:min 70
                        #:default-value 'foobar)
                (result 69 'foobar))
    (test-match (search #:max 30
                        #:increase-max? #f)
                (result 30 (exhausted)))
    (test-match (search #:max 30
                        #:increase-max? #t)
                (result 40 -40))

    ;; Simulate a program that has no mutants!
    (ignore (define search (lowest-upper-bound-binary-search
                            (match-lambda [else (go-lower)]))))
    (test-match (search)
                (result -1 (exhausted)))

    ;; The inverse...
    (ignore (define search (lowest-upper-bound-binary-search
                            (match-lambda [else (go-higher)]))))
    (test-match (search #:max 100)
                (result 100 (exhausted)))


    ;; Simulate a program that passes on all mutants, and has 50 mutants. Then
    ;; we want index 49, and the result should be `exhausted?`.
    (ignore (define search (lowest-upper-bound-binary-search
                            (match-lambda
                              [(? (</c 50)) (go-higher)]
                              [else (go-lower)]))))
    (test-match (search)
                (result 49 (exhausted)))

    ;; Simulate a program that diverges on the first mutant, and only have 50
    ;; mutants. Then we want index 0.
    (ignore (define search (lowest-upper-bound-binary-search
                            (match-lambda
                              [(? (>/c 50)) (go-lower)]
                              [i (try-go-lower (~a i))]))))
    (test-match (search)
                (result 0 "0"))

    ;; Simulate a program that diverges on the third mutant up to the tenth,
    ;; then runs out mutants. Then we want index 2.
    (ignore (define search (lowest-upper-bound-binary-search
                            (match-lambda
                              [(? (>/c 9)) (go-lower)]
                              [(? (between/c 2 9) i) (try-go-lower (~a i))]
                              [(? (</c 2)) (go-higher)]))))
    (test-match (search)
                (result 2 "2"))))
