#lang at-exp rscript

(provide blame-reliability-plot-for
         blame-reliability-bar-for
         raw-bt-violation-distribution-plot-for)

(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         (only-in pict vc-append text)
         pict-util/file
         bex/mutation-analysis/mutation-analysis-summaries
         bex/experiment/blame-trail-data
         bex/configurables/configurables
         bex/runner/mutation-runner-data

         "plot-common.rkt"
         "read-data.rkt")

(define (raw-bt-violation-distribution-plot-for key
                                                blame-trail-map
                                                #:dump-to [dump-to #f])
  (define trails (hash-ref blame-trail-map key))
  (define total-trail-count (length trails))
  (define-values {bt-satisfied bt-failed} (partition satisfies-BT-hypothesis? trails))
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

(define (blame-reliability-bar-for key
                                   blame-trail-map
                                   #:dump-to [dump-to #f]
                                   #:colors [colors '("green" "yellow" "red")]
                                   #:with-x-tick? [with-x-tick? #f])
  (define breakdown (blame-reliability-breakdown-for key blame-trail-map))
  (define total-mutant-count (for/sum ([mutant-trails (in-hash-values breakdown)])
                               (length mutant-trails)))
  (when (output-port? dump-to)
    (pretty-write breakdown dump-to))
  (stacked-histogram
   (list
    (list key
          (for/list ([category (in-list '("always" "sometimes" "never"))])
            (define trail-count (length (hash-ref breakdown category)))
            (/ trail-count total-mutant-count))))
   #:colors colors
   #:add-ticks? with-x-tick?))

(define (blame-reliability-plot-for key
                                    blame-trail-map
                                    #:dump-to [dump-to #f]
                                    #:colors [colors '("green" "yellow" "red")])
  (plot-pict (blame-reliability-bar-for key
                                        blame-trail-map
                                        #:dump-to dump-to
                                        #:colors colors)
             #:y-max 1
             ;; #:x-label "Mutants"
             #:y-label (~a "Percent of mutants")
             #:x-label #f
             #:title key))

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
                "benchmark of a given mode.")
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

 (define table
   (make-distributions-table (if raw-counts?
                                 raw-bt-violation-distribution-plot-for
                                 blame-reliability-plot-for)
                             #:breakdown-by breakdown-dimension
                             #:summaries-db (mutation-analysis-summaries-db)
                             #:data-directory data-dir
                             #:dump-to-file dump-path))

 (make-directory* out-dir)
 (define with-title
   (vc-append 20
              (text name)
              table))
 (pict->png! with-title (build-path out-dir (~a name '- breakdown-dimension ".png"))))
