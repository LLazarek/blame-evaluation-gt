#lang at-exp rscript

(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         pict
         pict-util/file
         bex/mutation-analysis/mutation-analysis-summaries
         bex/experiment/blame-trail-data
         (prefix-in db: bex/db/db)
         bex/configurables/configurables
         bex/runner/mutation-runner

         "read-data.rkt")

;; r/b-error := runtime/blame-error

;; -> (or/c "N/A" nonnegative-rational?)
(define (%-mutants-with-at-least-one-r/b-error mutator data)
  (define trails (hash-ref data mutator))
  (define mutant-has-r/b-error?
    (for/fold ([mutant-has-r/b-error? (hash)])
              ([trail (in-list trails)])
      (hash-update mutant-has-r/b-error?
                   ;; lltodo: wiw: this needs to include the benchmark name so that we don't combine
                   ;; modules across benchmarks that happen to have the same name
                   (list (blame-trail-summary-mutated-module-name trail)
                         (blame-trail-summary-mutation-index trail))
                   (λ (x) (or x
                              (trail-has-mutant-with-r/b-error? trail)))
                   #f)))
  (define r/b-errors (hash-values mutant-has-r/b-error?))
  (if (empty? r/b-errors)
      (values 0 "N/A")
      (values (/ (count identity r/b-errors) (length r/b-errors))
              #f)))

(define (trail-has-mutant-with-r/b-error? trail)
  (define mutant-has-r/b-error?
    (match-lambda [(struct* mutant-summary
                            ([run-status (struct* run-status
                                                  ([outcome (or 'runtime-error
                                                                'blamed)]))]))
                   #t]
                  [else #f]))
  (ormap mutant-has-r/b-error? (blame-trail-summary-mutants trail)))

(main
 #:arguments {[(hash-table ['data-dir data-dir]
                           ['out-dir  out-dir]
                           ['name     name]
                           ['config   config-path]
                           _ ...)
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
               #:mandatory]}

 (install-configuration! config-path)

 (define mutant-mutators
   (read-mutants-by-mutator (mutation-analysis-summaries-db)))

 (define data (read-blame-trails-by-mutator/across-all-benchmarks data-dir mutant-mutators))

 (define all-mutator-names (mutator-names data))

 (define-values {r/b-error-%s-by-mutator annotations}
   (for/lists {mutator-bars annotations}
              ([mutator (in-list all-mutator-names)])
     (define-values {% annotation}
       (%-mutants-with-at-least-one-r/b-error mutator data))
     (values (list mutator %)
             (list mutator annotation))))

 (define bar-offset 1)
 (define label-y 0)
 (define annotation-points
   (for/list ([annotation (in-list annotations)]
              [i (in-naturals)]
              #:when (second annotation))
     (point-label (list label-y (+ (* i bar-offset) 0.5))
                  (second annotation)
                  #:point-size 0
                  #:color "gray")))
 (define plot
   (plot-pict (list (discrete-histogram r/b-error-%s-by-mutator
                                        #:invert? #t)
                    annotation-points)
              #:x-min 0
              #:y-min 0
              #:x-max 1
              #:y-label #f
              #:x-label #f
              #:title "Percentage of mutants with dynamic errors"))

 (make-directory* out-dir)
 (pict->png! plot (build-path out-dir (~a name '- "mutant-dynamic-errors.png"))))
