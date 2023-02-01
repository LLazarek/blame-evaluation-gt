#lang at-exp rscript

(require (only-in "experiment-info.rkt"
                  experiment-benchmarks
                  benchmarks-dir)
         file/glob
         syntax/parse/define)

(define cpus 5)
(define-runtime-paths
  [mutation-analysis-config "../configurables/configs/mutation-type-error-analysis.rkt"]
  [mutation-analysis-dir "../mutation-analysis"]
  [TR-config "../configurables/configs/TR.rkt"]
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
  -n (~a cpus)
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

(define/racket-runner (filter-mutants-for-dynamic-errors! outdir type-err-summaries.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@mutant-dynamic-errors"]
  ../mutation-analysis/filter-mutants-for-dynamic-errors.rkt
  -s (~a type-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "dyn-err-summaries-progress.log")
  -d (~a scratch-dir)
  -j (~a cpus)
  -m natural-bot
  -o #:result (outpath "dyn-err-summaries.rktdb"))

(define (plot-mutation-analysis-results! outdir)
  (define full-outdir (simple-form-path outdir))
  (parameterize ([current-directory mutation-analysis-dir])
    (plot-mutation-analysis-results!/assuming-right-dir full-outdir)))

(define/racket-runner (plot-mutation-analysis-results!/assuming-right-dir outdir)
  ../mutation-analysis/plot-new-mutation-analyses.rkt
  -c (~a TR-config)
  --type attrition
  -d (build-path outdir "dyn-err-summaries.rktdb")
  -o (build-path outdir "mutant-attrition.png")
  (glob (~a (build-path outdir "mutation-analyses" "*-debug.log"))))

(define/racket-runner (find-interesting-scenarios! outdir dyn-err-summaries.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  #:pre-flags [-O "info@find-scenarios"]
  ../mutation-analysis/find-interesting-scenarios.rkt
  -s (~a dyn-err-summaries.rktdb)
  -b (~a benchmarks-dir)
  -l (outpath "interesting-scenarios-progress.log")
  -d (~a scratch-dir)
  -j (~a cpus)
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

(define/racket-runner (sample-mutants! outdir interesting-mutants.rktdb)
  ../configurables/mutant-sampling/generate-samples-within-mutators.rkt
  -c (~a TR-config)
  ;; -s (build-path outdir "dyn-err-summaries.rktdb")
  -S (~a interesting-mutants.rktdb)
  -b (~a benchmarks-dir)
  -n 10000 ;; just a big number. We want all of the mutants right now.
  -o #:result (build-path outdir "mutant-samples.rktdb"))

(define/racket-runner (select-bt-roots! outdir mutant-samples.rktdb interesting-scenarios.rktdb)
  #:helper (define (outpath name)
             (build-path outdir name))
  ../configurables/bt-root-sampling/pre-select-bt-roots.rkt
  -d (~a mutant-samples.rktdb)
  -c (~a TR-config)
  -i (~a interesting-scenarios.rktdb)
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
 #:arguments ([(hash-table)
               (list outdir)]
              #:args [outdir])
 (rebuild!)
 (displayln "Analyzing mutation...")
 (define progress-logs (analyze-mutation/all-benchmarks! outdir))
 (displayln "Summarizing mutation analysis...")
 (define type-err-summaries.rktdb (summarize-mutation-analyses! outdir progress-logs))
 (displayln "Filtering mutants for dynamic errors...")
 (define dyn-err-summaries.rktdb  (filter-mutants-for-dynamic-errors! outdir type-err-summaries.rktdb))
 (displayln "Plotting results...")
 (plot-mutation-analysis-results! outdir)

 ;; todo: reify the alternative flow here where mutants are sampled without info
 ;; about interesting scenarios? (ie assuming all scenarios in the lattice are
 ;; interesting)
 (displayln "Searching for interesting scenarios...")
 (define interesting-scenarios.rktdb (find-interesting-scenarios! outdir
                                                                  dyn-err-summaries.rktdb))
 (displayln "Summarizing interesting mutants from scenarios...")
 (define interesting-mutants.rktdb (summarize-interesting-mutants! outdir
                                                                   interesting-scenarios.rktdb
                                                                   dyn-err-summaries.rktdb))
 (displayln "Sampling interesting mutants...")
 (define mutant-samples.rktdb (sample-mutants! outdir interesting-mutants.rktdb))
 (displayln "Selecting BT roots for mutant samples...")
 (select-bt-roots! outdir mutant-samples.rktdb interesting-scenarios.rktdb)
 (displayln "Pre-computing mutant results for erasure...")
 (pre-compute-benchmark-results-for-erasure/all-benchmarks! outdir mutant-samples.rktdb)
 (displayln "DB set up complete."))
