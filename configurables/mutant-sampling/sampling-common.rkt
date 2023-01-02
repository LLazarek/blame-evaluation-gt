#lang racket

(provide (contract-out
          [distribute-sample-size-to-categories
           (->i ([sample-size natural?]
                 [max-sample-size-by-category (hash/c any/c natural?)])
                [result (hash/c any/c natural?)]
                #:post {sample-size max-sample-size-by-category result}
                (and (set=? (hash-keys max-sample-size-by-category)
                            (hash-keys result))
                     (= sample-size
                        (apply + (hash-values result)))))]

          [sample-indices-by-category
           (->i ([all-indices-by-category (hash/c any/c (listof natural?))]
                 [sample-size natural?])
                (#:exclude [excluded-indices (listof natural?)]
                 #:replacement? [replacement? boolean?])
                [result (hash/c any/c (listof natural?))]
                #:post {all-indices-by-category sample-size excluded-indices result}
                (and (set=? (hash-keys all-indices-by-category)
                            (hash-keys result))
                     (= sample-size
                        (apply + (map length (hash-values result))))
                     (set-empty? (set-intersect excluded-indices
                                                (flatten (hash-values result))))))])
         summaries-db->benchmark-info)

(require racket/random)

(define (distribute-sample-size-to-categories sample-size
                                              max-sample-size-by-category)
  (define categories (hash-keys max-sample-size-by-category))
  (let redistribute
      ([distributed (for/hash ([category (in-list categories)])
                      (values category 0))]
       [space-left max-sample-size-by-category]
       [remaining-to-distribute sample-size])
    (define remaining-categories
      (for/list ([{category left} (in-hash space-left)]
                 #:when (> left 0))
        category))
    (cond [(or (<= remaining-to-distribute 0)
               (empty? remaining-categories))
           distributed]
          [(<= remaining-to-distribute (length remaining-categories))
           (for/fold ([distributed distributed])
                     ([i (in-range remaining-to-distribute)]
                      [category (in-list remaining-categories)])
             (hash-update distributed category add1))]
          [else
           (define distribution-per-category
             (quotient remaining-to-distribute (length remaining-categories)))
           (define distributed-this-time
             (for/hash ([category (in-list remaining-categories)])
               (define distributed-to-category
                 (if (<= distribution-per-category (hash-ref space-left category))
                     distribution-per-category
                     (hash-ref space-left category)))
               (values category
                       distributed-to-category)))
           (define new-distributed
             (for/hash ([{category previously-distributed} (in-hash distributed)])
               (values category
                       (+ previously-distributed
                          (hash-ref distributed-this-time category 0)))))
           (define new-space-left
             (for/hash ([{category previously-left} (in-hash space-left)])
               (values category
                       (- previously-left
                          (hash-ref distributed-this-time category 0)))))
           (define total-distributed-this-time
             (apply + (hash-values distributed-this-time)))
           (define new-remaining-to-distribute
             (- remaining-to-distribute total-distributed-this-time))
           (redistribute new-distributed
                         new-space-left
                         new-remaining-to-distribute)])))

(module+ test
  (require ruinit)
  (test-begin
    #:name distribute-sample-size-to-categories
    (test-match (distribute-sample-size-to-categories 100
                                                      (make-immutable-hash
                                                       (map (λ (name) (cons name 15))
                                                            (build-list 10 values))))
                (hash-table [(? (between/c 0 9)) 10] ___))
    (test-match (distribute-sample-size-to-categories 100
                                                      (make-immutable-hash
                                                       (map (λ (name) (cons name 5))
                                                            (build-list 10 values))))
                (hash-table [(? (between/c 0 9)) 5] ___))
    (test-equal? (distribute-sample-size-to-categories 100
                                                       (hash 0 15
                                                             1 50
                                                             2 30
                                                             3 20))
                 (hash 0 15
                       1 35
                       2 30
                       3 20))
    (test-equal? (distribute-sample-size-to-categories 2
                                                       (hash 0 15
                                                             1 50
                                                             2 30
                                                             3 20))
                 (hash 0 1
                       1 1
                       2 0
                       3 0))))

(define (sample-indices-by-category all-indices-by-category
                                    sample-size
                                    #:exclude [excluded-indices empty]
                                    #:replacement? [replacement? #f])
  (define valid-indices-by-category
    (for/hash ([{category indices} (in-dict all-indices-by-category)])
      (values category
              (set-subtract indices
                            excluded-indices))))
  (define max-sample-size-by-category
    (for/hash ([{category indices} (in-hash valid-indices-by-category)])
      (values category (length indices))))
  (define samples-by-category
    (distribute-sample-size-to-categories sample-size
                                          max-sample-size-by-category))
  (define sampled-indices-by-category
    (for/hash ([{category samples} (in-hash samples-by-category)])
      (define indices-for-category
        (hash-ref valid-indices-by-category category))
      (define sampled-indices (random-sample indices-for-category samples
                                             #:replacement? replacement?))
      (values category sampled-indices)))
  sampled-indices-by-category)

;; (or/c db:path-to-db? #f) ; one of these two must not be #f
;; (or/c db:path-to-db? #f)
;; ->
;; (values (listof string?) (string? -> benchmark-summary?))
(define (summaries-db->benchmark-info mutant-summaries-db-path
                                      benchmark-summaries-db-path)
  (match* {mutant-summaries-db-path benchmark-summaries-db-path}
    [{(? path-string?) #f}
     (define summaries-db (db:get mutant-summaries-db-path))
     (values (db:keys summaries-db)
             (λ (bench-name)
               (define module-summaries (db:read summaries-db bench-name))
               (when benchmarks-dir
                 (define bench (read-benchmark (build-path benchmarks-dir
                                                           bench-name)))
                 (for ([{module-name summary} (in-hash module-summaries)])
                   (sanity-check-summary! summary
                                          module-name
                                          bench)))
               (module-summaries->benchmark-summary module-summaries)))]
    [{#f (? path-string?)}
     (define bench-summaries-db (db:get benchmark-summaries-db-path))
     (values (db:keys bench-summaries-db)
             (λ (bench) (db:read bench-summaries-db bench)))]))
