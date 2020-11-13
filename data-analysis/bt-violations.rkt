#lang at-exp rscript

(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         (only-in pict vc-append text)
         pict-util/file
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../experiment/blame-trail-data.rkt"
         (prefix-in db: "../db/db.rkt")
         "../configurables/configurables.rkt"
         "../runner/mutation-runner-data.rkt"

         "read-data.rkt")

(define pict? any/c)
(define (hash-with-all-active-mutator-names? h)
  (and (hash? h)
       (set=? (hash-keys h)
              (configured:active-mutator-names))))

(define (raw-bt-violation-distribution-plot-for key
                                                blame-trail-map
                                                #:dump-to [dump-to #f])
  (define trails (hash-ref blame-trail-map key))
  (define total-trail-count (length trails))
  (define-values {bt-satisfied bt-failed} (partition bt-satisfied? trails))
  (when (output-port? dump-to)
    (pretty-write (hash 'satisfied bt-satisfied
                        'failed bt-failed)
                  dump-to))
  (define bt-satisfied-count (length bt-satisfied))
  (define bt-satisfied-% (if (zero? total-trail-count) 0 (/ bt-satisfied-count total-trail-count)))
  (plot-pict (discrete-histogram (list (list "✓" bt-satisfied-%)
                                       (list "✗" (- 1 bt-satisfied-%))))
             #:y-max 1
             #:x-label "Trail satisfies BT?"
             #:y-label (~a "Percent (out of " total-trail-count ")")
             #:title key))

(define (blame-reliability-plot-for key
                                    blame-trail-map
                                    #:dump-to [dump-to #f])
  (define breakdown (blame-reliability-breakdown-for key blame-trail-map))
  (define total-mutant-count (for/sum ([mutant-trails (hash-values breakdown)])
                               (length mutant-trails)))
  (when (output-port? dump-to)
    (pretty-write breakdown dump-to))
  (plot-pict (stacked-histogram
              (list
               (list key
                     (for/list ([category (in-list '("always" "sometimes" "never"))])
                       (define trail-count (length (hash-ref breakdown category)))
                       (/ trail-count total-mutant-count))))
              #:colors '("green" "yellow" "red"))
             #:y-max 1
             ;; #:x-label "Mutants"
             #:y-label (~a "Percent (out of " total-mutant-count " mutants)")))

;; string? (hash/c string? (listof blame-trail?))
;; ->
;; (hash/c "always"    (listof blame-trail?)
;;         "sometimes" ^
;;         "never"     ^)
(define (blame-reliability-breakdown-for key
                                         blame-trail-map)
  (define trails (hash-ref blame-trail-map key))
  (define total-trail-count (length trails))
  (define trails-grouped-by-mutant (group-by blame-trail-mutant-id trails))

  (define (categorize-trail-set-reliability trail-set)
    (define bt-success-count (count bt-satisfied? trail-set))
    (match* {bt-success-count (length trail-set)}
      [{     0  (not 0)} "never"]
      [{(not 0)      0}  "always"]
      [{(not 0) (not 0)} "sometimes"]))

  (for/fold ([breakdown (hash "always" empty
                              "sometimes" empty
                              "never" empty)])
            ([mutant-trails (in-list trails-grouped-by-mutant)])
    (define mutant-category (categorize-trail-set-reliability mutant-trails))
    (hash-update breakdown
                 mutant-category
                 (add-to-list mutant-trails))))

(define (bt-satisfied? bt)
  (define end-mutant-summary (first (blame-trail-mutant-summaries bt)))
  (match end-mutant-summary
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome 'type-error]
                                          [blamed blamed]))
                     config)
     (and (equal? (hash-ref config mutated-mod-name) 'types)
          (list? blamed)
          (member mutated-mod-name blamed))]
    [else #f]))

(define (add-missing-active-mutators blame-trails-by-mutator/across-all-benchmarks)
  (for/fold ([data+missing blame-trails-by-mutator/across-all-benchmarks])
            ([mutator-name (in-list (configured:active-mutator-names))])
    (hash-update data+missing
                 mutator-name
                 values
                 empty)))
(define ((add-to-list v) l) (cons v l))

(main
 #:arguments {[(hash-table ['data-dir data-dir]
                           ['out-dir  out-dir]
                           ['name     name]
                           ['config   config-path]
                           ['dump-path dump-path]
                           ['by breakdown-dimension]
                           ['raw-counts? raw-counts?])
               args]
              #:once-each
              [("-d" "--data-dir")
               'data-dir
               ("Path to the directory containing data sub-directories for each"
                "mode.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-s" "--mutant-summaries")
               'summaries-db
               ("Path to the db containing summaries of the mutants in the data."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect ["path"
                          (set-parameter mutation-analysis-summaries-db)
                          (mutation-analysis-summaries-db)]]
              [("-o" "--out-dir")
               'out-dir
               ("Directory in which to place plots."
                "Default: .")
               #:collect ["path" take-latest "."]]
              [("-n" "--name")
               'name
               ("Name for the plots. This becomes the title of the plots,"
                "as well as a prefix of the plot file names.")
               #:collect ["name" take-latest ""]]
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-D" "--dump-data")
               'dump-path
               "Dump data for generating the plot to the given file."
               #:collect ["path" take-latest #f]]
              [("-b" "--by")
               'by
               "Break down the data by either mutator or benchmark. Default: mutator"
               #:collect ["mutator or benchmark" take-latest "mutator"]]
              [("-r" "--raw-counts")
               'raw-counts?
               ("Plot raw counts of blame trail violations instead of the statistically-meaningful"
                "Always/Sometimes/Never categorizations of mutants.")
               #:record]}
 #:check [(member breakdown-dimension '("mutator" "benchmark"))
          @~a{Invalid argument to --by: @breakdown-dimension}]

 (install-configuration! config-path)

 (define mutant-mutators
   (read-mutants-by-mutator (mutation-analysis-summaries-db)))

 (define blame-trails-by-mutator/across-all-benchmarks
   (add-missing-active-mutators
    (read-blame-trails-by-mutator/across-all-benchmarks data-dir mutant-mutators)))

 (define all-mutator-names (mutator-names blame-trails-by-mutator/across-all-benchmarks))

 (define dump-port (and dump-path
                        (open-output-file dump-path #:exists 'replace)))
 (define distributions
   (match breakdown-dimension
     ["mutator"
      (for/hash ([mutator (in-list all-mutator-names)])
        (when dump-port (newline dump-port) (displayln mutator dump-port))
        (define plot
          (if raw-counts?
              (raw-bt-violation-distribution-plot-for mutator
                                                      blame-trails-by-mutator/across-all-benchmarks
                                                      #:dump-to dump-port)
              (blame-reliability-plot-for mutator
                                          blame-trails-by-mutator/across-all-benchmarks
                                          #:dump-to dump-port)))
        (values mutator plot))]
     ["benchmark"
      (define blame-trails-by-benchmark/across-all-mutators
        (for*/fold ([bts-by-benchmark (hash)])
                   ([{mutator bts} (in-hash blame-trails-by-mutator/across-all-benchmarks)]
                    [bt (in-list bts)])
          (define benchmark (mutant-benchmark (blame-trail-mutant-id bt)))
          (hash-update bts-by-benchmark
                       benchmark
                       (add-to-list bt)
                       empty)))
      (for/hash ([benchmark (in-hash-keys blame-trails-by-benchmark/across-all-mutators)])
        (when dump-port (newline dump-port) (displayln benchmark dump-port))
        (define plot
          (if raw-counts?
              (raw-bt-violation-distribution-plot-for benchmark
                                                      blame-trails-by-benchmark/across-all-mutators
                                                      #:dump-to dump-port)
              (blame-reliability-plot-for benchmark
                                          blame-trails-by-benchmark/across-all-mutators
                                          #:dump-to dump-port)))
        (values benchmark plot))]))
 (when dump-port (close-output-port dump-port))

 (make-directory* out-dir)
 (define (write-distributions-image! distributions name)
   (define distributions/sorted
     (map cdr (sort (hash->list distributions) string<? #:key car)))
   (define all-together
     (table/fill-missing distributions/sorted
                         #:columns 3
                         #:column-spacing 5
                         #:row-spacing 5))
   (define all-together+title
     (vc-append 20
                (text name)
                all-together))
   (pict->png! all-together+title (build-path out-dir (~a name '- breakdown-dimension ".png"))))

 (write-distributions-image! distributions name))
