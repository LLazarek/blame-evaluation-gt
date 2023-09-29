#lang at-exp rscript

(require complot
         sawzall
         data-frame
         threading
         bex/configurables/configurables
         (prefix-in db: bex/db/db)
         bex/mutation-analysis/mutation-analysis-summaries)

(define current-mutation-types (make-parameter #f))

;; (listof path-to-existant-file?) -> (data-frame/columns/c "mutator" "benchmark" "type-error" "non-type-error" "total")
(define (read-log-files-for-type-error-data log-files)
  (define benchmark-success-hashes ; (hash/c benchmark-name? success+failure-hash?)
    (for/hash ([log-file (in-list log-files)])
      (values (path->benchmark-name log-file)
              (log->data log-file))))
  (~> (for*/data-frame {mutator benchmark type-error non-type-error}
        ([mutator (in-list (current-mutation-types))]
         [{benchmark success-data-hash} (in-hash benchmark-success-hashes)])
        (values mutator
                benchmark
                (hash-ref (hash-ref success-data-hash 'success)
                          mutator
                          0)
                (hash-ref (hash-ref success-data-hash 'fail)
                          mutator
                          0)))
      (create [total {type-error non-type-error} (+ type-error non-type-error)])))

(define (path->benchmark-name path)
  (match (basename path)
    [(regexp @regexp{^(.+)-debug.log$}
             (list _ name))
     name]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

(define (log->data path)
  (match (system/string @~a{
                            awk '/^#hash/{buf=""; a=1} a{buf=buf $0 ORS} END{printf "%s", buf}' @;
                            '@path'
                            })
    [(regexp @regexp{#hash.+$}
             (list data-hash-str))
     (hash-update
      (hash-update (call-with-input-string data-hash-str read)
                   'fail
                   values
                   (hash))
      'success
      values
      (hash))]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

;; db:db-path?
;; ->
;; (data-frame/columns/c "mutator" "benchmark" "dynamic-error")
(define (read-dyn-error-data-from-db db-path)
  (define db (db:get db-path))
  (define benchmarks (db:keys db))
  (for*/data-frame {mutator benchmark dynamic-error}
    ([benchmark (in-list benchmarks)]
     [summaries-by-module (in-value (db:read db benchmark))]
     [benchmark-summary
      (in-value (benchmark-summary-mutants-by-mutator
                 (module-summaries->benchmark-summary summaries-by-module)))]
     [mutator (in-list (current-mutation-types))])
    (define dyn-err-mutant-count (length (hash-ref benchmark-summary (~a mutator) empty)))
    (values mutator
            benchmark
            dyn-err-mutant-count)))

(define (rename-mutator mutator-name)
  (hash-ref (hash "constant-swap" "constant"
                  "begin-result-deletion" "deletion"
                  "top-level-id-swap" "top-level-id"
                  "imported-id-swap" "imported-id"
                  "method-id-swap" "method-id"
                  "field-id-swap" "field-id"
                  "position-swap" "position"
                  "nested-list-construction-swap" "list"
                  "class:initializer-swap" "class:init"
                  "class:publicity" "class:public"
                  "class:super-new" "class:super"
                  "class:parent-swap" "class:parent"
                  "arithmetic-op-swap" "arithmetic"
                  "boolean-op-swap" "boolean"
                  "negate-conditional" "negate-cond"
                  "force-conditional" "force-cond")
            mutator-name
            mutator-name))

(main
 #:arguments {[(hash-table ['config config-path]
                           ['outfile outfile]
                           ['plot-type plot-type]
                           ['dyn-err-summaries-db dyn-err-db-path])
               log-files]
              #:once-each
              [("-c" "--config")
               'config
               "Config from which to obtain active mutator names."
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--outfile")
               'outfile
               ("Filename to output plot table."
                "Default: <plot-type>.png (see -t/--type)")
               #:collect ["path" take-latest #f]]
              [("-t" "--type")
               'plot-type
               ("Which type of plot to produce for the data. Options below; default is attrition."
                "  attrition : plot the count of mutants produced by each mutator, broken down by 1) the total count, 2) of those, the ones that produce type errors, and 3) of those, the ones that produce dynamic errors")
               #:collect {"type" take-latest "attrition"}
               #:mandatory]
              [("-d" "--dyn-err-summaries-db")
               'dyn-err-summaries-db
               ("Read data about dynamic errors from the given summaries db.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              #:args log-files}

 #:check [(not (empty? log-files))
          @~a{Must provide at least one log file to plot.}]
 #:check [(path-to-existant-file? config-path)
          @~a{@config-path does not exist}]
 #:check [(db:path-to-db? dyn-err-db-path)
          @~a{@dyn-err-db-path does not look like a db}]

 (install-configuration! config-path)

 (current-mutation-types
  (map string->symbol (configured:active-mutator-names)))

 (define outpath (or outfile (~a plot-type ".png")))
 (define mutator-type-error-data
   (read-log-files-for-type-error-data log-files))
 (define mutator-dyn-error-data
   (read-dyn-error-data-from-db dyn-err-db-path))
 (match plot-type
   ["attrition"
    (define (sum vec) (apply + (vector->list vec)))
    (define data
      (~> (full-join mutator-type-error-data
                     mutator-dyn-error-data
                     "mutator"
                     "benchmark")
          (group-with "mutator")
          (aggregate [type-error {type-error} (sum type-error)]
                     [non-type-error {non-type-error} (sum non-type-error)]
                     [dynamic-error {dynamic-error} (sum dynamic-error)])
          (create [type-error-but-no-dyn {type-error dynamic-error} (- type-error dynamic-error)])
          (pivot-longer ["non-type-error" "type-error-but-no-dyn" "dynamic-error"]
                        #:names-to "category"
                        #:values-to "count")))
    (define the-plot
      (add-to (plot data)
              (x-axis)
              (y-axis #:tick-lines? #t)
              (stacked-bars #:x "mutator"
                            #:y "count"
                            #:group-by "category"
                            #:group-ordering (by-vector (vector "dynamic-error"
                                                                "type-error-but-no-dyn"
                                                                "non-type-error"))
                            #:colors (match-lambda [3 (list "black" "gray" '(242 242 242))]
                                                   [4 (list "black" "gray" '(242 242 242) "red")])
                            #:auto-label? #f)
              (legend)))
    (local-require (only-in plot
                            plot-x-tick-label-anchor
                            plot-x-tick-label-angle))
    (parameterize ([plot-x-tick-label-anchor 'top-right]
                   [plot-x-tick-label-angle  30])
      (void (render the-plot outpath
                    #:width 1700
                    #:height 1000)))]
   [other (raise-user-error 'cli @~a{No plot type named @~s[other]})]))

(module test racket)
