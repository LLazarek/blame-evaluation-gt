#lang at-exp racket

(provide sample-some-trails
         sample-some-trail-comparisons)

(require racket/runtime-path
         racket/random
         "read-data.rkt"
         "bt-length-distributions.rkt"
         "bt-ids.rkt"
         bex/runner/mutation-runner-data
         bex/experiment/blame-trail-data)

(define-runtime-path default-summaries-db "../../../experiment-data/dbs/code-mutations-natural-biased/type-err-summaries.rktdb")

(define (sample-some-trails mode-data-dir
                            #:summaries-db-path [summaries-db-path default-summaries-db]
                            #:min-length [min-length 2]
                            #:samples [samples 10]
                            #:start-outcome [start-outcome-ok? #f]
                            #:middle-outcomes [mid-outcome-ok? #f]
                            #:end-outcome [end-outcome-ok? #f])
  (define mutant-mutators (read-mutants-by-mutator summaries-db-path))
  (define bts-by-mutator (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir
                                                                             mutant-mutators))
  (define all-bts (append* (hash-values bts-by-mutator)))
  (define filtered-bts (filter (λ (bt)
                                 (and (or (false? start-outcome-ok?)
                                          (start-outcome-ok?
                                           (run-status-outcome
                                            (mutant-summary-run-status
                                             (last (blame-trail-mutant-summaries bt))))))
                                      (or (false? mid-outcome-ok?)
                                          (match (blame-trail-mutant-summaries bt)
                                            [(list _
                                                   (? (compose1 mid-outcome-ok?
                                                                run-status-outcome
                                                                mutant-summary-run-status)) ...
                                                   _) #t]
                                            [(list _ ..2) #f]
                                            [else #t]))
                                      (or (false? end-outcome-ok?)
                                          (end-outcome-ok?
                                           (run-status-outcome
                                            (mutant-summary-run-status
                                             (first (blame-trail-mutant-summaries bt))))))
                                      (or (false? min-length)
                                          (>= (bt-length bt #t) min-length))))
                               all-bts))
  (random-sample filtered-bts
                 (min samples
                      (length filtered-bts))
                 #:replacement? #f))

(define (sample-some-trail-comparisons mode-data-dir-1
                                       mode-data-dir-2
                                       #:where bts-ok?
                                       #:summaries-db-path [summaries-db-path default-summaries-db]
                                       #:samples [samples 10])

  (define mutant-mutators (read-mutants-by-mutator summaries-db-path))
  (define (read-bts-by-id mode-data-dir)
    (bts-by-id
     (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir
                                                         mutant-mutators)))
  (define bts-by-id-1 (read-bts-by-id mode-data-dir-1))
  (define bts-by-id-2 (read-bts-by-id mode-data-dir-2))
  (define all-bt-pairs
    (for/list ([{id bt} (in-hash bts-by-id-1)])
      (list bt (hash-ref bts-by-id-2 id))))
  (define filtered-bts (filter (λ (pair) (apply bts-ok? pair))
                               all-bt-pairs))
  (random-sample filtered-bts
                 (min samples
                      (length filtered-bts))
                 #:replacement? #f))


(module+ db
  (require sawzall
           threading
           data-frame
           "aggregate-data.rkt")
  (sawzall-show-formatter (λ (v) (pretty-format v 60)))
  (define data (read-blame-trail-db->df "/home/lukas/github_sync/grad/projects/blame-gradual-typing/src/experiment-data/results/type-api-mutations/data.sqlite"))
  (define comparison-points
    (~> data
        (create [mutant (mutant) (pretty-format mutant)])
        #;(where [success] (false? success))
        #;(where [mode] (equal? mode "transient-newest"))
        ((λ (data*)
           (inner-join (~> data*
                           (where [mode] (equal? mode "transient-newest"))
                           (rename "success" "transient-newest-success"
                                   "mutant-summaries" "transient-newest-summaries"))
                       (~> data*
                           (where [mode] (equal? mode "transient-oldest"))
                           (rename "success" "transient-oldest-success"
                                   "mutant-summaries" "transient-oldest-summaries"))
                       "mutant"
                       "id"))
         _)
        (where [transient-newest-success
                transient-oldest-success]
               (and transient-newest-success
                    (not transient-oldest-success)))
        #;(where transient-newest-summaries
               (match* transient-newest-summaries
                 [(list* (struct* mutant-summary ([run-status (struct* run-status ([outcome 'blamed]
                                                                                   [blamed '()]))]))
                         _)
                  #t]
                 [else #f]))))
  (pretty-print-columns 80)
  (show (reorder comparison-points (cons "id" (λ (a b) (random-ref '(#t #f))))) everything))
