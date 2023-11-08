#lang at-exp rscript

(provide db-setup-script)

(require (only-in "../experiment-info.rkt"
                  experiment-benchmarks
                  benchmarks-dir)
         file/glob
         syntax/parse/define)

(define-runtime-paths
  [mutation-analysis-dir "../../mutation-analysis"]
  [scratch-dir "../../../../tmp/scratch"])

(define cpus (make-parameter 5))

(begin-for-syntax
  (require syntax/parse
           racket/format)
  (define-syntax-class cli-arg
    #:commit
    (pattern minus-i
             #:when (equal? (syntax->datum #'minus-i) -i)
             #:with parsed #''|-i|)
    (pattern (_ ...)
             #:with parsed this-syntax)
    (pattern {~and plain-thing {~or* :id :number :str}}
             #:with parsed #''plain-thing)))

(define-for-syntax (path-stx->string stx)
  (datum->syntax stx (~a (syntax->datum stx))))

(define racket-path (find-system-path 'exec-file))
(define-simple-macro (define/racket-runner head
                       {~alt {~seq #:helper helper-def}
                             {~optional {~seq #:pre-flags [pre-flag:cli-arg ...]}}} ...
                       rel-script-path
                       .
                       {~and ({~or* #:result script-arg:cli-arg} ...)
                             ({~alt {~optional {~seq #:result result-path:cli-arg}}
                                    _} ...)})
  #:with rel-script-path-string (path-stx->string #'rel-script-path)
  (begin
    (define-runtime-path script-path rel-script-path-string)
    (define head
      helper-def ...
      (unless (system @~a{
                          @racket-path @(string-join (map ~a (flatten (list {~? {~@ pre-flag.parsed ...}})))) @;
                          @script-path @(string-join (map ~a (flatten (list {~? script-arg.parsed} ...))))
                          })
        (raise-user-error 'setup-all-dbs
                          (~a (basename rel-script-path-string) " exited w/ non-0 status")))
      {~? result-path.parsed (void)})))

(define/racket-runner (rebuild!)
  ../../util/project-raco.rkt -c)

(define (analyze-mutation/all-benchmarks! outdir mutation-analysis-config check-for-any-error?)
  (define analyses-outdir (build-path outdir "mutation-analyses"))
  (make-directory* analyses-outdir)
  (for/list ([bench (in-list experiment-benchmarks)])
    (analyze-mutation! bench analyses-outdir mutation-analysis-config check-for-any-error?)))

(define/racket-runner (analyze-mutation! bench-name outdir mutation-analysis-config check-for-any-error?)
  #:helper (define (outpath suffix)
             (build-path outdir (~a bench-name '- suffix)))
  #:pre-flags [-O "debug@mutation-analysis" -W "warning@mutation-analysis"]
  ../../mutation-analysis/analyze-mutation.rkt
  -b (build-path benchmarks-dir bench-name)
  -n (~a (cpus))
  -e (outpath 'errs.log)
  -l #:result (outpath 'progress.log)
  -c (~a mutation-analysis-config)
  -o (outpath 'data)
  (if check-for-any-error?
      "-a"
      '())
  >> (outpath 'debug.log)
  2> (outpath 'errs.log))

(define/racket-runner (summarize-mutation-analyses! outdir progress-logs)
  ../../mutation-analysis/summarize-mutation-analyses.rkt
  -o #:result (build-path outdir "type-err-summaries.rktdb")
  (values progress-logs))

(define/racket-runner (filter-mutants-for-dynamic-errors! outdir
                                                          type-err-summaries.rktdb
                                                          experiment-config
                                                          lattice-config-id
                                                          interestingness-filter?)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@mutant-dynamic-errors"]
  ../../mutation-analysis/filter-mutants-for-dynamic-errors.rkt
  -s (~a type-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "dyn-err-summaries-progress.log")
  -d (~a scratch-dir)
  -j (~a (cpus))
  -c (~a experiment-config)
  -L (~a lattice-config-id)
  (if interestingness-filter?
      "-I"
      empty)
  -o #:result (outpath "dyn-err-summaries.rktdb"))

(define (plot-mutation-analysis-results! outdir
                                         mutation-analysis-config)
  (define full-outdir (simple-form-path outdir))
  (parameterize ([current-directory mutation-analysis-dir])
    (plot-mutant-attrition!/assuming-right-dir full-outdir mutation-analysis-config)))

(define/racket-runner (plot-mutant-attrition!/assuming-right-dir outdir mutation-analysis-config)
  ../../../bex-data-analysis/mutation/plot-new-mutation-analyses.rkt
  -c (~a mutation-analysis-config)
  --type attrition
  -d (build-path outdir "dyn-err-summaries.rktdb")
  -o (build-path outdir "mutant-attrition.pdf")
  (glob (~a (build-path outdir "mutation-analyses" "*-debug.log"))))

;; only relevant for the bltym experiment
(define/racket-runner (plot-type-mutation-categories! outdir mutation-analysis-config)
  ../../../bex-data-analysis/mutation/categorize-mutants.rkt
  -c (~a mutation-analysis-config)
  -s (build-path outdir "dyn-err-summaries.rktdb")
  -o (build-path outdir "mutant-population.pdf"))

(define/racket-runner (find-interesting-scenarios! outdir
                                                   dyn-err-summaries.rktdb
                                                   dyn-err-analysis-config)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@find-scenarios"]
  ../../mutation-analysis/find-interesting-scenarios.rkt
  -s (~a dyn-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "interesting-scenarios-progress.log")
  -d (~a scratch-dir)
  -j (~a (cpus))
  -c (~a dyn-err-analysis-config)
  -o #:result (outpath "interesting-scenarios.rktdb"))

(define/racket-runner (summarize-interesting-mutants! outdir
                                                      interesting-scenarios.rktdb
                                                      dyn-err-summaries.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  ../../mutation-analysis/summarize-interesting-mutants-from-scenarios.rkt
  -i (~a interesting-scenarios.rktdb)
  -s (~a dyn-err-summaries.rktdb)
  -o #:result (outpath "interesting-mutant-summaries.rktdb"))

(define/racket-runner (sample-mutants! outdir
                                       experiment-config-from-which-to-get-mutator/mutant-info
                                       [number-of-mutants #f]
                                       #:from-interesting [interesting-mutants.rktdb #f]
                                       #:from-dyn-err     [dyn-err-summaries.rktdb #f])
  ../../configurables/mutant-sampling/generate-samples-within-mutators.rkt
  -c (~a experiment-config-from-which-to-get-mutator/mutant-info)
  (cond [interesting-mutants.rktdb
         `(-S ,interesting-mutants.rktdb)]
        [dyn-err-summaries.rktdb
         `(-s ,dyn-err-summaries.rktdb)]
        [else
         (error 'sample-mutants!
                "Missing #:from-* keyword specificying from which set to sample mutants.")])
  -b (~a benchmarks-dir)
  (if number-of-mutants
      (list "-n" (~a number-of-mutants))
      empty)
  -o #:result (build-path outdir "mutant-samples.rktdb"))

(define/racket-runner (select-bt-roots! outdir
                                        experiment-config-from-which-to-get-mutator/mutant-info
                                        mutant-samples.rktdb
                                        [interesting-scenarios.rktdb #f])
  #:helper (define (outpath name)
             (build-path outdir name))
  ../../configurables/bt-root-sampling/pre-select-bt-roots.rkt
  -d (~a mutant-samples.rktdb)
  -c (~a experiment-config-from-which-to-get-mutator/mutant-info)
  (if interesting-scenarios.rktdb
      (list "-i" interesting-scenarios.rktdb)
      empty)
  -o (outpath "pre-selected-bt-roots.rktdb"))

(define (pre-compute-benchmark-results-for-erasure/all-benchmarks! outdir
                                                                   mutant-samples.rktdb
                                                                   pre-compute-config)
  (for ([bench (in-list experiment-benchmarks)])
    (pre-compute-benchmark-results-for-erasure! bench
                                                outdir
                                                mutant-samples.rktdb
                                                pre-compute-config)))

(define/racket-runner (pre-compute-benchmark-results-for-erasure! bench-name
                                                                  outdir
                                                                  mutant-samples.rktdb
                                                                  pre-compute-config)
  ../../configurables/benchmark-runner/pre-compute-benchmark-results.rkt
  -c (~a pre-compute-config)
  -m (~a mutant-samples.rktdb)
  -o (build-path outdir "pre-computed-mutant-results.rktdb")
  -l (build-path outdir "pre-computed-mutant-results-progress.log")
  -j (~a (cpus))
  -d (~a scratch-dir)
  (build-path benchmarks-dir bench-name))

(define (setup-all-dbs! mutation-analysis:check-for-any-error?
                        search-for-interesting-scenarios?
                        mutants-to-sample-per-benchmark/or-all
                        outdir
                        viz-mode
                        plot-type-mutation-categories?

                        mutation-analysis-config
                        experiment-config-with-which-analyze-mutants-dynamic-errors
                        dynamic-error-filtering-lattice-config-id
                        dynamic-error-interestingness-filter?
                        pre-compute-config)
  (match-define-values {viz-only? no-viz?}
                       (match viz-mode
                         ['normal   (values #f #f)]
                         ['viz-only (values #t #f)]
                         ['no-viz   (values #f #t)]))
  (define mutants-to-sample-per-benchmark
    (match mutants-to-sample-per-benchmark/or-all
      ['all 100000000] ;; hack!
      [n n]))
  (define (viz!)
    (displayln "Plotting results...")
    (plot-mutation-analysis-results! outdir mutation-analysis-config)
    (when plot-type-mutation-categories?
      (plot-type-mutation-categories! outdir mutation-analysis-config)))
  (cond [viz-only? (viz!)]
        [else
         (rebuild!)
         (displayln "Analyzing mutation...")
         (define progress-logs
           (analyze-mutation/all-benchmarks! outdir
                                             mutation-analysis-config
                                             mutation-analysis:check-for-any-error?))
         (displayln "Summarizing mutation analysis...")
         (define type-err-summaries.rktdb (summarize-mutation-analyses! outdir progress-logs))
         (displayln "Filtering mutants for dynamic errors...")
         (define dyn-err-summaries.rktdb
           (filter-mutants-for-dynamic-errors! outdir
                                               type-err-summaries.rktdb
                                               experiment-config-with-which-analyze-mutants-dynamic-errors
                                               dynamic-error-filtering-lattice-config-id
                                               dynamic-error-interestingness-filter?))

         (unless no-viz? (viz!))

         (define-values {mutant-samples.rktdb interesting-scenarios.rktdb}
           (cond [search-for-interesting-scenarios?
                  (displayln "Searching for interesting scenarios...")
                  (define interesting-scenarios.rktdb
                    (find-interesting-scenarios! outdir
                                                 dyn-err-summaries.rktdb
                                                 experiment-config-with-which-analyze-mutants-dynamic-errors))
                  (displayln "Summarizing interesting mutants from scenarios...")
                  (define interesting-mutants.rktdb
                    (summarize-interesting-mutants! outdir
                                                    interesting-scenarios.rktdb
                                                    dyn-err-summaries.rktdb))
                  (displayln "Sampling interesting mutants...")
                  (define mutant-samples.rktdb
                    (sample-mutants! outdir
                                     mutation-analysis-config
                                     mutants-to-sample-per-benchmark
                                     #:from-interesting interesting-mutants.rktdb))
                  (values mutant-samples.rktdb interesting-scenarios.rktdb)]
                 [else
                  (displayln "Sampling mutants...")
                  (define mutant-samples.rktdb
                    (sample-mutants! outdir
                                     mutation-analysis-config
                                     mutants-to-sample-per-benchmark
                                     #:from-dyn-err dyn-err-summaries.rktdb))
                  (values mutant-samples.rktdb
                          #f)]))
         (displayln "Selecting BT roots for mutant samples...")
         (select-bt-roots! outdir
                           mutation-analysis-config
                           mutant-samples.rktdb
                           interesting-scenarios.rktdb)
         (when pre-compute-config
           (displayln "Pre-computing mutant results for erasure...")
           (pre-compute-benchmark-results-for-erasure/all-benchmarks! outdir
                                                                      mutant-samples.rktdb
                                                                      pre-compute-config))
         (displayln "DB set up complete.")]))

(define-simple-macro (db-setup-script
                      ;; each of these configs should be relative paths from here
                      #:mutation-analysis-config mutation-analysis-config-rel-path
                      #:mutation-analysis-error-type mutation-analysis-error-type
                      #:analyze-type-mutation-categories? analyze-type-mutation-categories?
                      #:experiment-config-with-which-analyze-mutants-dynamic-errors experiment-config-with-which-analyze-mutants-dynamic-errors-rel-path
                      #:dynamic-error-filtering-lattice-config-id dynamic-error-filtering-lattice-config-id
                      #:dynamic-error-interestingness-filter? dynamic-error-interestingness-filter?
                      #:search-for-interesting-scenarios? search-for-interesting-scenarios?
                      #:mutants-to-sample-per-benchmark mutants-to-sample-per-benchmark
                      {~or* {~seq #:pre-compute-config pre-compute-config-rel-path}
                            #:no-erasure-mode})
  #:with [{~optional pre-compute-config-id}] (if (attribute pre-compute-config-rel-path)
                                                 #'[pre-compute-config]
                                                 #'[])
  (begin
    (define-runtime-paths
      [mutation-analysis-config mutation-analysis-config-rel-path]
      [experiment-config-with-which-analyze-mutants-dynamic-errors
       experiment-config-with-which-analyze-mutants-dynamic-errors-rel-path]
      {~? [pre-compute-config-id pre-compute-config-rel-path]})
    (main
     #:arguments ([(hash-table ['no-viz? no-viz?]
                               ['viz-only? viz-only?]
                               _ (... ...))
                   (list outdir)]
                  #:once-each
                  [("-j" "--cpus")
                   'cpus
                   ("How many CPUs to use when possible."
                    @~a{Default: @(cpus)})
                   #:collect {"N" (set-parameter cpus string->number) (cpus)}]
                  [("-v" "--no-viz")
                   'no-viz?
                   "Skip generating visualizations."
                   #:record
                   #:conflicts '(viz-only?)]
                  [("-V" "--viz-only")
                   'viz-only?
                   "Only generate visualizations."
                   #:record
                   #:conflicts '(no-viz?)]
                  #:args [outdir])
     #:check [(natural? (cpus))
              @~a{CPUs must be a natural number.}]

     (file-stream-buffer-mode (current-output-port) 'line)

     (setup-all-dbs! (equal? mutation-analysis-error-type 'any-error)
                     search-for-interesting-scenarios?
                     mutants-to-sample-per-benchmark
                     outdir
                     (cond [no-viz? 'no-viz]
                           [viz-only? 'viz-only]
                           [else 'normal])
                     analyze-type-mutation-categories?

                     mutation-analysis-config
                     experiment-config-with-which-analyze-mutants-dynamic-errors
                     dynamic-error-filtering-lattice-config-id
                     dynamic-error-interestingness-filter?
                     {~? pre-compute-config-id #f}))))

