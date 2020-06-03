#lang at-exp rscript

(require plot/no-gui
         plot/utils
         plot-util/quick/infer
         plot-util/histogram
         pict
         (except-in pict-util line)
         pict-util/file)

(define all-mutation-types
  '(arithmetic-op-swap
    boolean-op-swap
    class:publicity
    class:super-new
    data-accessor-swap
    constant-swap
    begin-result-deletion
    negate-conditional
    class:parent-swap
    class:initializer-swap
    position-swap
    class:add-extra-method
    top-level-id-swap))

(define PLOT-WIDTH 500)

(define (read-data-files log-files
                         #:data-type data-type)
  (for/hash ([log (in-list log-files)])
    (define-values {data total-mutant-count total-success-count}
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
    (values title data)))

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
       (for/hash ([type (in-list all-mutation-types)])
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
         (values type ratio)))
     (when (equal? data-type 'total-counts)
       (define sum (apply + (hash-values ratios)))
       (unless (< (abs (- sum 1)) 0.001)
         (raise-user-error
          'plot-mutation-analyses
          @~a{
              The sum of mutation operator occurrence ratios is not 1!
              This probably means that there are missing mutation types,
              or otherwise there is a bug.
              })))
     (values ratios
             (hash-ref hit+miss-data 'total)
             (apply + (hash-values all-hits)))]
    [else
     (raise-user-error 'plot-mutation-analyses
                       @~a{Given file that doesn't look like a log: @path})]))


;; plot-y-far-tick-label-anchor
(define (make-plotter add-ticks?)
  (define colored-discrete-histogram
    (discrete-histogram/colors
     (map ->brush-color (build-list (length all-mutation-types) values))))
  (simple-inferred-plotter (curry colored-discrete-histogram
                                  #:invert? #t
                                  #:add-ticks? add-ticks?)
                           #:plot plot-pict
                           ;; #:extra (list (y-tick-lines))
                           ))

(define labels-width 190)
#;(define plot-bars-pict
        (simple-inferred-plotter (curry discrete-histogram
                                        #:add-ticks? #t)
                                 #:plot (make-angled-label-plotter plot-pict)))

(main
 #:arguments {[flags log-files]
              #:once-each
              [("-o" "--outfile")
               'outfile
               ("Filename to output plot table."
                "Default: mutation-analyses.png")
               #:collect ["path" take-latest "mutation-analyses.png"]]
              [("-c" "--columns")
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

 (define plot-type (if (hash-ref flags 'total-counts)
                       'total-counts
                       'success-ratios))
 (define columns (string->number (hash-ref flags 'cols)))
 (define picts
   (flatten
    (for/list ([{benchmark data} (in-hash (read-data-files
                                           log-files
                                           #:data-type plot-type))]
               [i (in-naturals)])
      (define draw-labels? (zero? (modulo i columns)))
      (define plot-bars-pict (make-plotter draw-labels?))
      (define pict
        (parameterize ([plot-width (- PLOT-WIDTH (if draw-labels? 0 labels-width))]
                       [plot-height (+ 400 (if draw-labels? 15 0))])
          (vc-append
           (plot-bars-pict data
                          #:title benchmark
                          #:y-label #f
                          #:x-label #f
                          #:x-max 1)
           (if draw-labels? (blank 0) (blank 0 10)))))
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
