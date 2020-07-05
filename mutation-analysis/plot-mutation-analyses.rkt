#lang at-exp rscript

(require plot/no-gui
         plot/utils
         plot-util/quick/infer
         plot-util/histogram
         pict
         (except-in pict-util line)
         pict-util/file
         "../configurables/configurables.rkt")

(define current-mutation-types (make-parameter #f))

(define PLOT-WIDTH 500)

(struct annotated (data points))
(define (read-data-files log-files
                         #:data-type data-type)
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
    (values title (annotated data plot-annotations))))

(define (path->benchmark-name path)
  (match (basename path)
    [(regexp @regexp{^(.+).log$}
             (list _ name))
     name]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

(define (read-data-from-log path #:data-type data-type)
  (match (file->string path)
    [(regexp @regexp{#hash.+$}
             (list data-hash-str))
     (define hit+miss-data
       (call-with-input-string data-hash-str read))
     (define all-hits
       (hash-ref hit+miss-data 'success))
     (define all-misses
       (hash-ref hit+miss-data 'fail))
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
              (match (hash-ref hit+miss-data 'total)
                [0 0]
                [total (/ (+ hits misses) total)])]))
         (cons type ratio)))
     (define annotations
       (data-annotations all-hits all-misses))
     (when (equal? data-type 'total-counts)
       (define sum (apply + (dict-values ratios)))
       (unless (< (abs (- sum 1)) 0.001)
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
             (hash-ref hit+miss-data 'total)
             (apply + (hash-values all-hits))
             annotations)]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))

(define (data-annotations all-hits all-misses)
  (define bar-offset 1)
  (define label-y 0)
  (for/list ([type (in-list (current-mutation-types))]
             [i (in-naturals)]
             #:when (match* {(hash-ref all-hits type 0)
                             (hash-ref all-misses type 0)}
                      [{0 0} #t]
                      [{_ _} #f]))
    (point-label (list label-y (+ (* i bar-offset) 0.5))
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

(define labels-width 190)
#;(define plot-bars-pict
        (simple-inferred-plotter (curry discrete-histogram
                                        #:add-ticks? #t)
                                 #:plot (make-angled-label-plotter plot-pict)))

(main
 #:arguments {[flags log-files]
              #:once-each
              [("-c" "--config")
               'config
               "Config from which to obtain active mutator names."
               #:collect ["path"
                          (set-parameter current-configuration-path)
                          #f]
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
              [("--population")
               'total-counts
               ("Plot the breakdown of mutation type counts for each benchmark"
                "instead of mutation type success ratios.")
               #:record]
              #:args log-files}

 #:check [(not (empty? log-files))
          @~a{Must provide at least one log file to plot.}]

 (current-mutation-types
  (map string->symbol
       (load-configured (current-configuration-path)
                        "mutation"
                        'active-mutator-names)))

 (define label-offset
   (match (length (current-mutation-types))
     [13 30]
     [10 55]))

 (define plot-type (if (hash-ref flags 'total-counts)
                       'total-counts
                       'success-ratios))
 (define columns (string->number (hash-ref flags 'cols)))
 (define picts
   (flatten
    (for/list ([{benchmark data+annotations} (in-hash (read-data-files
                                                       log-files
                                                       #:data-type plot-type))]
               [i (in-naturals)])
      (define draw-labels? (zero? (modulo i columns)))
      (match-define (annotated data points) data+annotations)
      (define plot-bars-pict (make-plotter draw-labels?
                                           #:extra points))
      (define pict
        (parameterize ([plot-width (- PLOT-WIDTH (if draw-labels? 0 labels-width))]
                       [plot-height (+ 400 (if draw-labels? label-offset 0))])
          (vc-append
           (plot-bars-pict data
                          #:title benchmark
                          #:y-label #f
                          #:x-label #f
                          #:x-max 1)
           (if draw-labels? (blank 0) (blank 0 label-offset)))))
      pict)))
 (define together
   (fill-background
    (vc-append
     10
     (text (match plot-type
             ['total-counts "Proportion of all mutants created by operator"]
             ['success-ratios "Ratio of mutants causing type errors, per operator"])
           '(bold)
           30)
     (table/fill-missing picts
                         #:columns columns
                         #:column-spacing 10
                         #:row-spacing 0))))
 (pict->png! together (hash-ref flags 'outfile)))

(module test racket)
