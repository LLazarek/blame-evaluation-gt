#lang at-exp rscript

(require (only-in "experiment-info.rkt"
                  experiment-benchmarks
                  benchmarks-dir)
         file/glob
         syntax/parse/define)

(define cpus (make-parameter 5))
(define-runtime-paths
  [mutation-analysis-config "../configurables/blutil-configs/mutation-code-mistake-analysis.rkt"]
  [mutation-analysis-dir "../mutation-analysis"]
  [TR-config "../configurables/blutil-configs/blame.rkt"]
  [scratch-dir "../../tmp/scratch"]
  [pre-compute-config "../configurables/configs/erasure-pre-compute-benchmark-results.rkt"])

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
  ../util/project-raco.rkt -c)

(define (analyze-mutation/all-benchmarks! outdir)
  (define analyses-outdir (build-path outdir "mutation-analyses"))
  (make-directory* analyses-outdir)
  (for/list ([bench (in-list experiment-benchmarks)])
    (analyze-mutation! bench analyses-outdir)))

(define/racket-runner (analyze-mutation! bench-name outdir)
  #:helper (define (outpath suffix)
             (build-path outdir (~a bench-name '- suffix)))
  #:pre-flags [-O "debug@mutation-analysis" -W "warning@mutation-analysis"]
  ../mutation-analysis/analyze-mutation.rkt
  -b (build-path benchmarks-dir bench-name)
  -n (~a (cpus))
  -e (outpath 'errs.log)
  -l #:result (outpath 'progress.log)
  -c (~a mutation-analysis-config)
  -o (outpath 'data)
  >> (outpath 'debug.log)
  2> (outpath 'errs.log))

(define/racket-runner (summarize-mutation-analyses! outdir progress-logs)
  ../mutation-analysis/summarize-mutation-analyses.rkt
  -o #:result (build-path outdir "type-err-summaries.rktdb")
  (values progress-logs))

(define/racket-runner (filter-mutants-for-dynamic-errors! outdir
                                                          type-err-summaries.rktdb
                                                          filtering-mode)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@mutant-dynamic-errors"]
  ../mutation-analysis/filter-mutants-for-dynamic-errors.rkt
  -s (~a type-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "dyn-err-summaries-progress.log")
  -d (~a scratch-dir)
  -j (~a (cpus))
  -m (~a filtering-mode)
  -o #:result (outpath "dyn-err-summaries.rktdb"))

(define (plot-mutation-analysis-results! outdir)
  (define full-outdir (simple-form-path outdir))
  (parameterize ([current-directory mutation-analysis-dir])
    (plot-mutant-attrition!/assuming-right-dir full-outdir))
  (plot-mutant-population! outdir))

(define/racket-runner (plot-mutant-attrition!/assuming-right-dir outdir)
  ../../bex-data-analysis/mutation/plot-new-mutation-analyses.rkt
  -c (~a TR-config)
  --type attrition
  -d (build-path outdir "dyn-err-summaries.rktdb")
  -o (build-path outdir "mutant-attrition.pdf")
  (glob (~a (build-path outdir "mutation-analyses" "*-debug.log"))))

(define/racket-runner (plot-mutant-population! outdir)
  ../../bex-data-analysis/mutation/categorize-mutants.rkt
  -c (~a TR-config)
  -s (build-path outdir "dyn-err-summaries.rktdb")
  -o (build-path outdir "mutant-population.pdf"))

(define/racket-runner (find-interesting-scenarios! outdir dyn-err-summaries.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@find-scenarios"]
  ../mutation-analysis/find-interesting-scenarios.rkt
  -s (~a dyn-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "interesting-scenarios-progress.log")
  -d (~a scratch-dir)
  -j (~a (cpus))
  -o #:result (outpath "interesting-scenarios.rktdb"))

(define/racket-runner (summarize-interesting-mutants! outdir
                                                      interesting-scenarios.rktdb
                                                      dyn-err-summaries.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  ../mutation-analysis/summarize-interesting-mutants-from-scenarios.rkt
  -i (~a interesting-scenarios.rktdb)
  -s (~a dyn-err-summaries.rktdb)
  -o #:result (outpath "interesting-mutant-summaries.rktdb"))

(define/racket-runner (sample-mutants! outdir
                                       number-of-mutants
                                       #:from-interesting [interesting-mutants.rktdb #f]
                                       #:from-dyn-err     [dyn-err-summaries.rktdb #f])
  ../configurables/mutant-sampling/generate-samples-within-mutators.rkt
  -c (~a TR-config)
  (cond [interesting-mutants.rktdb
         `(-S ,interesting-mutants.rktdb)]
        [dyn-err-summaries.rktdb
         `(-s ,dyn-err-summaries.rktdb)]
        [else
         (error 'sample-mutants!
                "Missing #:from-* keyword specificying from which set to sample mutants.")])
  -b (~a benchmarks-dir)
  -n (~a number-of-mutants)
  -o #:result (build-path outdir "mutant-samples.rktdb"))

(define/racket-runner (select-bt-roots! outdir mutant-samples.rktdb [interesting-scenarios.rktdb #f])
  #:helper (define (outpath name)
             (build-path outdir name))
  ../configurables/bt-root-sampling/pre-select-bt-roots.rkt
  -d (~a mutant-samples.rktdb)
  -c (~a TR-config)
  (if interesting-scenarios.rktdb
      (list "-i" interesting-scenarios.rktdb)
      empty)
  -o (outpath "pre-selected-bt-roots.rktdb"))

(define (pre-compute-benchmark-results-for-erasure/all-benchmarks! outdir mutant-samples.rktdb)
  (for ([bench (in-list experiment-benchmarks)])
    (pre-compute-benchmark-results-for-erasure! bench outdir mutant-samples.rktdb)))

(define/racket-runner (pre-compute-benchmark-results-for-erasure! bench-name
                                                                  outdir
                                                                  mutant-samples.rktdb)
  ../configurables/benchmark-runner/pre-compute-benchmark-results.rkt
  -c (~a pre-compute-config)
  -m (~a mutant-samples.rktdb)
  -o (build-path outdir "pre-computed-mutant-results.rktdb")
  (build-path benchmarks-dir bench-name))

 (main
 #:arguments ([(hash-table ['no-viz? no-viz?]
                           ['viz-only? viz-only?]
                           ['interesting-scenario-search? interesting-scenario-search?]
                           ['dyn-err-filtering-mode dyn-err-filtering-mode]
                           ['no-erasure? no-erasure?]
                           _ ...)
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
              [("-d" "--dyn-err-filtering-mode")
               'dyn-err-filtering-mode
               ("Mode for filtering dynamic errors."
                "See mutation-analysis/filter-mutants-for-dynamic-errors.rkt"
                "Default: erasure-any")
               #:collect {"name" take-latest "erasure-any"}]
              [("-i" "--search-for-interesting-scenarios")
               'interesting-scenario-search?
               "Instead of considering interesting all scenarios in the lattice of interesting mutants, run mutation-analysis/find-interesting-scenarios.rkt to find them."
               #:record]
              [("-n" "--no-erasure")
               'no-erasure?
               "Do not precompute benchmark results for erasure."
               #:record]
              #:args [outdir])
 #:check [(natural? (cpus))
          @~a{CPUs must be a natural number.}]

 (file-stream-buffer-mode (current-output-port) 'line)

 (define (viz!)
   (displayln "Plotting results...")
   (plot-mutation-analysis-results! outdir))
 (cond [viz-only? (viz!)]
       [else
        (rebuild!)
        (displayln "Analyzing mutation...")
        (define progress-logs (analyze-mutation/all-benchmarks! outdir))
        (displayln "Summarizing mutation analysis...")
        (define type-err-summaries.rktdb (summarize-mutation-analyses! outdir progress-logs))
        (displayln "Filtering mutants for dynamic errors...")
        (define dyn-err-summaries.rktdb  (filter-mutants-for-dynamic-errors! outdir
                                                                             type-err-summaries.rktdb
                                                                             dyn-err-filtering-mode))

        (unless no-viz? (viz!))

        (define-values {mutant-samples.rktdb interesting-scenarios.rktdb}
          (cond [interesting-scenario-search?
                 (displayln "Searching for interesting scenarios...")
                 (define interesting-scenarios.rktdb
                   (find-interesting-scenarios! outdir
                                                dyn-err-summaries.rktdb))
                 (displayln "Summarizing interesting mutants from scenarios...")
                 (define interesting-mutants.rktdb
                   (summarize-interesting-mutants! outdir
                                                   interesting-scenarios.rktdb
                                                   dyn-err-summaries.rktdb))
                 (displayln "Sampling interesting mutants...")
                 (define mutant-samples.rktdb
                   (sample-mutants! outdir 1000
                                    #:from-interesting interesting-mutants.rktdb))
                 (values mutant-samples.rktdb interesting-scenarios.rktdb)]
                [else
                 (displayln "Sampling mutants...")
                 (define mutant-samples.rktdb
                   (sample-mutants! outdir 1000
                                    #:from-dyn-err dyn-err-summaries.rktdb))
                 (values mutant-samples.rktdb
                         #f)]))
        (displayln "Selecting BT roots for mutant samples...")
        (select-bt-roots! outdir mutant-samples.rktdb interesting-scenarios.rktdb)
        (displayln "Pre-computing mutant results for erasure...")
        (unless no-erasure?
          (pre-compute-benchmark-results-for-erasure/all-benchmarks! outdir mutant-samples.rktdb))
        (displayln "DB set up complete.")]))
