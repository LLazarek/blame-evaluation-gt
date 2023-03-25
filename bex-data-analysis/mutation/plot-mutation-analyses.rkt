#lang at-exp rscript

(require plot
         plot/utils
         plot-util/quick/infer
         plot-util/histogram
         pict
         (except-in pict-util line)
         pict-util/file
         bex/configurables/configurables
         (prefix-in db: bex/db/db)
         bex/mutation-analysis/mutation-analysis-summaries
         bex/util/for)

(define current-mutation-types (make-parameter #f))

(define PLOT-WIDTH 700)
(define SUCCESS-COUNTS-LIMIT 50)

(struct annotated (data points) #:prefab)
(struct serializable-point-label (point label size color) #:prefab)

(define (serializable-point-label->point-label spl)
  (match-define (serializable-point-label p l s c) spl)
  (point-label p l #:point-size s #:color c))

(define (make-serializable-point-label p l #:point-size s #:color c)
  (serializable-point-label p l s c))


(define (read-data-files log-files
                         #:data-type data-type)
  (match data-type
    [(or 'successful-population-count 'successful-population-heatmap)
     (define benchmark-success-hashes
       (for/hash ([log-file (in-list log-files)])
         (values (path->benchmark-name log-file)
                 (hash-ref (log->data log-file) 'success (hash)))))
     (for/list ([mutator (in-list (current-mutation-types))])
       (define success-counts-per-benchmark
         (for/list ([{benchmark success-data-hash} (in-hash benchmark-success-hashes)])
           (list benchmark (hash-ref success-data-hash mutator 0))))
       (list mutator
             success-counts-per-benchmark))]
    [(or 'total-counts 'success-ratios)
     (for/hash ([log (in-list log-files)])
       (define-values {data
                       total-mutant-count
                       total-success-count
                       plot-annotations}
         (read-data-from-log log
                             #:data-type data-type))
       (define title (~a (path->benchmark-name log) " "
                         "("
                         total-success-count " / " total-mutant-count " type error"
                         ")"
                         ;; " ("
                         ;; (round (* 100 (/ total-success-count total-mutant-count)))
                         ;; "%)"
                         ))
       (values title (annotated data plot-annotations)))]))

(define (path->benchmark-name path)
  (match (basename path)
    [(regexp @regexp{^(.+).log$}
             (list _ name))
     name]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

(define (log->data path)
  (match (file->string path)
    [(regexp @regexp{#hash.+$}
             (list data-hash-str))
     (call-with-input-string data-hash-str read)]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

;; db:db-path? -> (listof (list/c mutator-name?
;;                                (listof (list/c benchmark-name?
;;                                                natural?))))
(define (read-data-from-summaries db-path)
  (define db (db:get db-path))
  (define benchmarks (db:keys db))
  (define data-dict
    (for/fold ([data empty])
              ([benchmark (in-list benchmarks)])
      (define summaries-by-module
        (db:read db benchmark))
      (define benchmark-summary
        (benchmark-summary-mutants-by-mutator
         (module-summaries->benchmark-summary summaries-by-module)))
      (for/fold ([data data])
                ([mutator (in-list (map ~a (current-mutation-types)))])
        (define ill-typed-mutant-count (length (hash-ref benchmark-summary mutator empty)))
        (dict-update data
                     mutator
                     (λ (benchmark-counts)
                       (append benchmark-counts
                               (list (list benchmark ill-typed-mutant-count))))
                     empty))))
  (dict-map data-dict list))

(define (read-data-from-log path #:data-type data-type)
  (define hit+miss-data
    (log->data path))
  (define all-hits
    (hash-ref hit+miss-data 'success (hash)))
  (define all-misses
    (hash-ref hit+miss-data 'fail (hash)))
  (define total-count (hash-ref hit+miss-data 'total 0))
  (define ratios
    (for/list ([type (in-list (current-mutation-types))])
      (define ratio
        (match* {(hash-ref all-hits type 0)
                 (hash-ref all-misses type 0)
                 data-type}
          [{0 0 _} 0]
          [{hits misses 'success-ratios}
           (/ hits (+ hits misses))]
          [{hits misses 'total-counts}
           (match total-count
             [0 0]
             [total (/ (+ hits misses) total)])]))
      (cons type ratio)))
  (define annotations
    (data-annotations all-hits all-misses))
  (when (equal? data-type 'total-counts)
    (define sum (apply + (dict-values ratios)))
    (unless (or (< (abs (- sum 1)) 0.001)
                (zero? sum) ;; there are no mutants at all
                )
      (raise-user-error
       'plot-mutation-analyses
       @~a{
           The sum of mutation operator occurrence ratios is not 1 @;
           in log @path !
           This probably means that there are missing mutation types, @;
           or otherwise there is a bug.
           The mutation types I looked for are:
           @(pretty-format (dict-keys ratios))
           })))
  (values ratios
          total-count
          (apply + (hash-values all-hits))
          annotations))

(define (data-annotations all-hits all-misses)
  (define bar-offset 1)
  (define label-y 0)
  (for/list ([type (in-list (current-mutation-types))]
             [i (in-naturals)]
             #:when (match* {(hash-ref all-hits type 0)
                             (hash-ref all-misses type 0)}
                      [{0 0} #t]
                      [{_ _} #f]))
    (make-serializable-point-label (list label-y (+ (* i bar-offset) 0.5))
                                   "N/A"
                                   #:point-size 0
                                   #:color "gray")))


;; plot-y-far-tick-label-anchor
(define (make-plotter add-ticks? #:extra [extra-renderer-trees empty])
  (define colored-discrete-histogram
    (discrete-histogram/colors
     (map ->brush-color (build-list (length (current-mutation-types)) values))))
  (simple-inferred-plotter (curry colored-discrete-histogram
                                  #:invert? #t
                                  #:add-ticks? add-ticks?)
                           #:plot plot-pict
                           #:extra extra-renderer-trees
                           ))

(define labels-width 100)
#;(define plot-bars-pict
        (simple-inferred-plotter (curry discrete-histogram
                                        #:add-ticks? #t)
                                 #:plot (make-angled-label-plotter plot-pict)))

(define (plot-benchmark-table all-data
                              plot-type
                              columns
                              #:title [title #f]
                              #:x-max [x-max 1])
  (define label-offset
    (inexact->exact
     (round
      (let ([x (length (current-mutation-types))])
        (+ (* -8.33333333333 x)
             136.833333333)
        #;(+ 18
           (* (exp 8.358)
              (expt x -1.905))))))
    #;(match (length (current-mutation-types))
        [13 30]
        [10 55]))
  (define picts
    (flatten
     (for/list ([{benchmark data+annotations} (in-hash all-data)]
                [i (in-naturals)])
       (define draw-labels? (zero? (modulo i columns)))
       (match-define (annotated data points) data+annotations)
       (define plot-bars-pict (make-plotter draw-labels?
                                            #:extra (map serializable-point-label->point-label
                                                         points)))
       (define pict
         (parameterize ([plot-width (- PLOT-WIDTH (if draw-labels? 0 labels-width))]
                        [plot-height (+ 400 (if draw-labels? label-offset 0))])
           (vc-append
            (plot-bars-pict data
                            #:title benchmark
                            #:y-label #f
                            #:x-label #f
                            #:x-max x-max)
            (if draw-labels? (blank 0) (blank 0 label-offset)))))
       pict)))
  (define table (table/fill-missing picts
                                    #:columns columns
                                    #:column-spacing 10
                                    #:row-spacing 20))
  (fill-background
   (if title
       (vc-append 10 (text title '(bold) 20) table)
       table)))

;; (listof real?) (listof (list/c real? color/c)) -> (listof color/c)
(define (cutoff-bucket-colors numbers bucket-colors)
  (define (color-for n)
    (for/first* ([bucket (in-list bucket-colors)])
                (and (< n (first bucket))
                     (second bucket))))
  (map color-for numbers))

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
 #:arguments {[flags log-files]
              #:once-each
              [("-c" "--config")
               'config
               "Config from which to obtain active mutator names."
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--outfile")
               'outfile
               ("Filename to output plot table."
                "Default: mutation-analyses.png")
               #:collect ["path" take-latest "mutation-analyses.png"]]
              [("-C" "--columns")
               'cols
               ("Number of columns in which to arrange plots."
                "Default: 4")
               #:collect ["count" take-latest "4"]]
              [("-t" "--type")
               'plot-type
               ("Which type of plot to produce for the data. Options below; default is population."
                "  population : plot the breakdown of entire mutant population by mutators"
                "  success-ratios : plot the ratio of successful mutants produced by each mutator"
                "  success-counts : plot the count of successful mutants produced by each mutator"
                "  successful-population-count : plot the number of successful mutants produced by each mutator, aggregating over benchmarks"
                "  successful-population-heatmap : plot a heatmap-like stacked bar chart color-coding the number of successful mutants produced by each mutator, aggregating over benchmarks")
               #:collect {"type" take-latest "population"}]
              [("-s" "--save")
               'save-data-file
               ("Save processed data into this file, or read it from the file"
                "if it already exists.")
               #:collect ["path" take-latest #f]]
              [("-d" "--summaries-db")
               'summaries-db
               ("Read data from the given summaries db instead of log files. This only works"
                "with the following plot types (see -t): successful-population-count")
               #:collect ["path" take-latest #f]]
              #:args log-files}

 #:check [(or (not (empty? log-files))
              (hash-ref flags 'summaries-db))
          @~a{Must provide at least one log file to plot.}]

 (install-configuration! (hash-ref flags 'config))

 (current-mutation-types
  (map string->symbol (configured:active-mutator-names)))

 (define plot-type
   (match (hash-ref flags 'plot-type)
     ["population" 'total-counts]
     ["success-ratios" 'success-ratios]
     ["success-counts" 'success-counts]
     ["successful-population-count" 'successful-population-count]
     ["successful-population-heatmap" 'successful-population-heatmap]))
 (define columns (string->number (hash-ref flags 'cols)))
 (define all-data
   (match (hash-ref flags 'save-data-file)
     [(? path-to-existant-file? path)
      (file->value path)]
     [maybe-save-path
      (define data
        (match* {log-files (hash-ref flags 'summaries-db)}
          [{'() (and (not #f) db-path)}
           (unless (member plot-type '(successful-population-count
                                       successful-population-heatmap
                                       success-counts))
             (raise-user-error 'plot-mutation-analyses
                               "-d can only be used with plot types successful-population-*"))
           (read-data-from-summaries db-path)]
          [{(? cons?) #f}
           (read-data-files
            log-files
            #:data-type plot-type)]
          [{_ _} (raise-user-error 'plot-mutation-analyses
                                   "-d/--summaries-db and positional args are mutually exclusive")]))
      (when maybe-save-path
        (write-to-file data maybe-save-path))
      data]))
 (match plot-type
   ['success-counts
    (define ((add-to-list v) l) (cons v l))
    (define breakdown-by-benchmark
      (for*/fold ([breakdown-by-benchmark (hash)])
                 ([{mutator mutator-counts-by-benchmark} (in-dict all-data)]
                  [{benchmark count} (in-dict (first mutator-counts-by-benchmark))])
        (hash-update breakdown-by-benchmark
                     benchmark
                     (add-to-list (cons mutator (first count)))
                     empty)))
    (define (sort+rename-mutators mutator-counts)
      (define ordering '("constant"
                         "deletion"
                         "position"
                         "list"
                         "top-level-id"
                         "imported-id"
                         "method-id"
                         "field-id"
                         "class:init"
                         "class:parent"
                         "class:public"
                         "class:super"
                         "arithmetic"
                         "boolean"
                         "negate-cond"
                         "force-cond"))
      (define renamed (dict-map mutator-counts
                                (λ (orig-name count)
                                  (cons (rename-mutator orig-name) count))))
      (sort renamed
            >
            #:key (match-lambda [(cons name _) (index-of ordering name)])
            #:cache-keys? #t))
    (define sorted+renamed+annotations
      (for/hash ([{benchmark mutator-counts} (in-hash breakdown-by-benchmark)])
        (define sorted+renamed (sort+rename-mutators mutator-counts))
        (values benchmark (annotated sorted+renamed empty))))
    (pict->png!
     (plot-benchmark-table sorted+renamed+annotations
                           #f
                           columns
                           ;; #:title "Mutants with interesting debugging scenarios, per operator"
                           #:x-max 50)
     (hash-ref flags 'outfile))]
   ['successful-population-count
    (plot-new-window? #t)
    (stacked-histogram-colors
     '((240 163 255)
       (0 117 220)
       (153 63 0)
       (43 206 72)
       (128 128 128)
       (148 255 181)
       (157 204 0)
       (94 241 242)
       (116 10 255)
       (255 164 5)
       (0 51 128)
       (76 0 92)
       (0 92 49)
       (255 204 153)
       (143 124 0)
       (194 0 136)
       (255 168 187)
       (66 102 0)
       (255 0 16)
       (0 153 143)
       (224 255 102)
       (153 0 0)
       (255 255 128)
       (255 255 0)
       (255 80 5)))
    (stacked-histogram-line-colors (stacked-histogram-colors))
    (pict->png!
     (plot-pict
      (grouped-category-stacked-histogram all-data
                                          #:label-groups? #f
                                          #:common-legend? #t)
      #:legend-anchor 'top-right
      #:title @~a{Ill-typed mutant population breakdown by mutator}
      #:y-label (match plot-type
                  ['successful-population-count
                   "count"]
                  ['successful-population-ratios
                   "ratio"])
      #:x-label "Mutator"
      #:y-max (match plot-type
                ['successful-population-count
                 #f]
                ['successful-population-ratios
                 1])
      #:y-min (match plot-type
                ['successful-population-count
                 #f]
                ['successful-population-ratios
                 0])
      #:width 1700
      #:height 1000)
     (hash-ref flags 'outfile))]
   ['successful-population-heatmap
    (define bucket-colors
      '((1 "gray")
        (10 "red")
        (50 "orange")
        (100 "cyan")
        (+inf.0 "green")))
    (plot-y-ticks no-ticks)
    (define picts
      (for/list ([mutator-stack (in-list all-data)])
        (define benchmark-successful-mutant-count-pairs
          (second mutator-stack))
        (define ones (map (const 1) benchmark-successful-mutant-count-pairs))
        (define colors
          (cutoff-bucket-colors (map second benchmark-successful-mutant-count-pairs)
                                bucket-colors))
        (plot-pict
         (list (stacked-histogram (list (list (first mutator-stack)
                                              ones))
                                  #:colors colors)
               (for/list ([benchmark-name
                           (sequence-map first
                                         benchmark-successful-mutant-count-pairs)]
                          [i (in-naturals)])
                 (point-label (list 0.1 (+ i 0.5)) benchmark-name
                              #:point-size 0)))
         #:x-label #f
         #:y-label #f)))
    (define the-plot-pict
      (fill-background
       (vc-append
        10
        (text (apply ~a
                     "Successful mutant heatmap; "
                     (add-between
                      (for/list ([bucket (in-list bucket-colors)])
                        (~a (second bucket) ": <" (first bucket)))
                      ", "))
              '(bold)
              30)
        (table/fill-missing picts
                            #:columns columns
                            #:column-spacing 10
                            #:row-spacing 0))))
    (pict->png!
     the-plot-pict
     (hash-ref flags 'outfile))]
   [(or 'total-counts 'success-ratios)
    (pict->png!
     (plot-benchmark-table all-data
                           plot-type
                           columns
                           #:title
                           (match plot-type
                             ['total-counts
                              "Proportion of all mutants created by operator"]
                             ['success-ratios
                              "Ratio of mutants causing type errors, per operator"]
                             ['success-counts
                              "Mutants with interesting debugging scenarios, per operator"]))
     (hash-ref flags 'outfile))]))

(module test racket)
