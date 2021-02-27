#lang at-exp rscript

(define (rename-mode mode-name)
  (hash-ref (hash "TR" "Natural"
                  "TR-stack-first" "Natural exceptions"

                  "transient-newest" "Transient last blame"
                  "transient-oldest" "Transient first blame"
                  "transient-stack-first"  "Transient exceptions"
                  "transient-all"  "Transient all blame"

                  "erasure-stack-first" "Erasure"

                  "null" "Null")
            mode-name))
(define rename-benchmark values)

;; (usefulness-table bt-lengths-table avo-matrix)
(define to-generate '(bt-lengths-table avo-matrix))




(require plot
         (except-in pict-util line)
         (except-in pict pict?)
         pict-util/file
         "../configurables/configurables.rkt"

         "plot-common.rkt"
         "read-data.rkt"

         "bt-violations.rkt"
         "bt-length-distributions.rkt"
         "adds-value-over.rkt"
         "experiment-info.rkt"
         "stratified-proportion-estimation.rkt")

(define-runtime-paths
  [data-dirs "../../experiment-data/results/code-mutations"]
  [dyn-err-summaries-db-path "../dbs/code-mutations/dyn-err-summaries.rktdb"]
  [TR-config "../configurables/configs/TR.rkt"]
  [outdir "./figures"]
  [data-cache "./data-cache"])

(install-configuration! TR-config)

(make-directory* outdir)



(define mutant-mutators
  (read-mutants-by-mutator dyn-err-summaries-db-path))
(define get-bts-by-mutator-for-mode
  (simple-memoize
   (λ (mode-name)
     (define mode-data-dir (build-path data-dirs mode-name))
     (add-missing-active-mutators
      (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir
                                                          mutant-mutators)))))

(define (bt->id bt)
  (list (blame-trail-mutant-id bt)
        (blame-trail-trail-id bt)))

(define success-color '(153 225 187))
(define failure-color '(255 153 153))

(when (member 'usefulness-table to-generate)
  (define usefulness-table
    (let ([modes (remove "null" modes)])
      (define (make-usefulness-table-cell-plot key bts-by-key
                                               #:dump-to [dump-to #f])
        (define bar (blame-reliability-bar-for key
                                               bts-by-key
                                               #:dump-to dump-to
                                               #:colors '("green" "yellow" "red")))
        (parameterize ([plot-x-ticks no-ticks]
                       [plot-y-ticks no-ticks]
                       [plot-y-far-ticks no-ticks])
          (plot-pict bar
                     #:x-label #f
                     #:y-label #f
                     #:title #f)))

      (define distributions-plots/by-mode
        (for/hash ([mode (in-list modes)])
          (values mode
                  (make-distributions-plots make-usefulness-table-cell-plot
                                            #:breakdown-by "benchmark"
                                            #:summaries-db dyn-err-summaries-db-path
                                            #:data-directory (build-path data-dirs mode)))))

      (define column-labels (map rename-benchmark benchmarks))
      (define row-labels (map rename-mode modes))
      (define plots
        (for*/list ([mode (in-list modes)]
                    [benchmark (in-list benchmarks)])
          (scale (hash-ref (hash-ref distributions-plots/by-mode
                                     mode)
                           benchmark)
                 0.5
                 0.5)))

      (define column-spacing 5)
      (define row-spacing 15)
      (define plain-table
        (table/fill-missing plots
                            #:columns (length column-labels)
                            #:column-spacing column-spacing
                            #:row-spacing row-spacing))

      (define a-cell (first plots))
      (define cell-width-spacer (blank (pict-width a-cell) 1))
      (define cell-height-spacer (blank 1 (pict-height a-cell)))
      (define (make-text str)
        (match str
          [(regexp #rx"([^ ]+) (.+)" (list _ first-word other-words))
           (vc-append (text first-word null 40)
                      (text other-words null 40))]
          [else (text str
                      null
                      40)]))
      (define column-label-picts
        (apply hc-append
               column-spacing
               (for/list ([col (in-list column-labels)])
                 (cc-superimpose (make-text col)
                                 cell-width-spacer))))
      (define row-label-picts
        (apply vr-append
               row-spacing
               (for/list ([row (in-list row-labels)])
                 (cc-superimpose (make-text row)
                                 cell-height-spacer))))
      (hb-append
       (+ column-spacing 5)
       row-label-picts
       (vl-append column-label-picts
                  plain-table))))

  (pict->png! usefulness-table (build-path outdir "usefulness-table.png")))

(when (member 'bt-lengths-table to-generate)
  (define bt-lengths-table
    (let ()
      (define bt-length-distribution-for-mode
        (simple-memoize
         #:on-disk (build-path data-cache "bt-length-distributions.rktd")
         (λ (mode-name)
           (define bts-by-mutator/across-all-benchmarks
             (get-bts-by-mutator-for-mode mode-name))
           (bt-length-distributions-for
            "yes"
            (hash "yes"
                  (append* (hash-values bts-by-mutator/across-all-benchmarks)))
            #:normalize? #t
            #:partition-by-success? #t))))

      (define (make-length-table-cell-plot mode-name)
        (define length-distribution (bt-length-distribution-for-mode mode-name))
        (define histogram
          (stacked-histogram length-distribution
                             #:colors (list success-color failure-color)))
        (parameterize ([plot-x-ticks (ticks (linear-ticks-layout #:number 1)
                                            (linear-ticks-format))]
                       [plot-font-size 20]
                       ;; [plot-y-ticks no-ticks]
                       [plot-y-far-ticks no-ticks])
          (plot-pict histogram
                     #:y-min 0
                     #:y-max 1

                     #:x-label #f
                     #:y-label #f
                     #:title #f
                     #:width (if (equal? mode-name "null")
                                 (* 2 (plot-width))
                                 (plot-width))
                     #:x-max (if (equal? mode-name "null") #f 8))))

      (define distributions-plots/by-mode
        (for/hash ([mode (in-list modes)])
          (values mode (make-length-table-cell-plot mode))))

      (define (make-text str)
        (match str
          [(regexp #rx"([^ ]+) (.+)" (list _ first-word other-words))
           (vc-append (text first-word null 40)
                      (text other-words null 40))]
          [else (text str
                      null
                      40)]))
      (define modes/ordered '("null"
                              "TR" "transient-newest" "transient-oldest"
                              "TR-stack-first" "transient-stack-first" "erasure-stack-first"))
      (define plot-labels
        (for/list ([mode (in-list modes/ordered)])
          (make-text (rename-mode mode))))
      (define uniform-label-filler
        (ghost
         (for/fold ([pict (blank 0 0)])
                   ([plot-label (in-list plot-labels)])
           (cc-superimpose pict
                           plot-label))))
      (define plots
        (for/list ([mode (in-list (rest modes/ordered))]
                   [label (in-list (rest plot-labels))])
          (vc-append 10
                     (cb-superimpose label uniform-label-filler)
                     (hash-ref distributions-plots/by-mode
                               mode))))

      (define column-spacing 10)
      (define row-spacing 50)
      (define plain-table
        (vc-append (/ row-spacing 2)
                   (vc-append 10
                              (cb-superimpose (first plot-labels) uniform-label-filler)
                              (hash-ref distributions-plots/by-mode "null"))
                   (table/fill-missing plots
                            #:columns 3
                            #:column-spacing column-spacing
                            #:row-spacing row-spacing)))
      plain-table))
  (pict->png! bt-lengths-table (build-path outdir "bt-lengths-table.png")))


(define/contract (two-sided-histogram data
                                      #:top-color [top-color (rectangle-color)]
                                      #:bot-color [bot-color (rectangle-color)]
                                      #:gap [gap (discrete-histogram-gap)]
                                      #:skip [skip (discrete-histogram-skip)])
  ({(listof (list/c any/c real? real?))}
   {#:top-color any/c
    #:bot-color any/c
    #:gap any/c
    #:skip any/c}
   . ->* .
   list?)
  (list (discrete-histogram (map (match-lambda [(list name top _) (list name top)])
                                 data)
                            #:color top-color
                            #:gap gap
                            #:skip skip)
        ;; lltodo: this is a workaround for a bug with discrete-histogram
        ;; This doesn't work:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2) (b -4)))
                         (x-axis))
                   #:y-min -5)
        ;; while this does:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2)) #:x-min 0)
                         (discrete-histogram '((b -4)) #:x-min 1)
                         (x-axis))
                   #:y-min -5)
        (for/list ([bar-sides (in-list data)]
                   [i (in-naturals)])
          (match-define (list name _ bottom) bar-sides)
          (discrete-histogram `((,name ,(- bottom)))
                              #:x-min i
                              #:color bot-color
                              #:gap gap
                              #:skip skip))))

(when (member 'avo-bars to-generate)
  (define avo-bars
    (let ([modes (remove "null" modes)])
      (define make-bt-by-benchmark+mutant-getter-for-mode
        (simple-memoize
         (λ (mode-name)
           (define bts-by-mutator (get-bts-by-mutator-for-mode mode-name))
           (λ (benchmark mutator)
             (for/hash/fold ([bt (in-list (hash-ref bts-by-mutator mutator))]
                             #:when (equal? (mutant-benchmark (blame-trail-mutant-id bt))
                                            benchmark))
                            #:combine cons
                            #:default empty
                            (values (blame-trail-mutant-id bt)
                                    bt))))))

      (define direct-avo-%
        (simple-memoize
         #:on-disk (build-path data-cache "direct-avo-percents.rktd")
         (λ (top-mode bottom-mode dump-to)
           (displayln @~a{@top-mode vs @bottom-mode})
           (define bottom-bts-by-mutator (get-bts-by-mutator-for-mode bottom-mode))
           (define bottom-bts-by-id
             (for*/hash ([bts (in-hash-values bottom-bts-by-mutator)]
                         [bt (in-list bts)])
               (values (bt->id bt) bt)))
           (define get-top-bts-by-benchmark+mutant
             (make-bt-by-benchmark+mutant-getter-for-mode top-mode))
           (define (get-bts-by-benchmark+mutant benchmark mutator)
             (define top-bts-by-mutant (get-top-bts-by-benchmark+mutant benchmark mutator))
             (for/hash ([{mutant top-bts} (in-hash top-bts-by-mutant)])
               (values mutant
                       (for/list ([top-bt (in-list top-bts)])
                         (define corresponding-bottom-bt (hash-ref bottom-bts-by-id (bt->id top-bt)))
                         (list top-bt corresponding-bottom-bt)))))
           (define estimate
             (strata-proportion-estimate (match-lambda
                                           [(list top-bt bottom-bt)
                                            (and (satisfies-BT-hypothesis? top-bt)
                                                 (not (satisfies-BT-hypothesis? bottom-bt)))])
                                         get-bts-by-benchmark+mutant))
           (define p (hash-ref estimate 'proportion-estimate))
           (define error-margin (variance->margin-of-error (hash-ref estimate 'variance)
                                                           1.96))
           (displayln error-margin)
           (list p error-margin))))

      (define (direct-avo-comparisons top-mode other-modes)
        (define comparison-data
          (for/list ([other-mode (in-list other-modes)]
                     [index (in-naturals)])
            (match-define (list top/other top/other-error-margin) (direct-avo-% top-mode other-mode #f))
            (match-define (list other/top other/top-error-margin) (direct-avo-% other-mode top-mode #f))
            (list (rename-mode other-mode) top/other other/top)))
        (parameterize ([plot-x-tick-label-angle 40]
                       [plot-x-tick-label-anchor 'top-right])
          (plot-pict (two-sided-histogram comparison-data
                                          #:top-color success-color
                                          #:bot-color failure-color)
                     #:title (rename-mode top-mode)
                     #:y-label @~a{% of scenarios} ; where @top-mode is more useful than mode X (above) | vice versa (below)
                     #:x-label #f
                     #:y-min -0.4
                     #:y-max 0.4)))

      (define plots
        (for*/list ([top (in-list modes)])
          (direct-avo-comparisons top modes)))

      (apply vc-append
             20
             plots)))
  (pict->png! avo-bars (build-path outdir "avo-bars.png"))
  (void))

(when (member 'success-bars to-generate)
  (define success-bars
    (let ([modes (remove "null" modes)])
      (define (all-bts-for-mode mode-name)
        (define bts-by-mutator (get-bts-by-mutator-for-mode mode-name))
        (for/fold ([all-bts empty])
                  ([{_ bts} (in-hash bts-by-mutator)])
          (append bts all-bts)))
      (define mode-success-%
        (simple-memoize
         #:on-disk (build-path data-cache "success-percents.rktd")
         (λ (mode-name)
           (define bts (all-bts-for-mode mode-name))
           (define success-count (count satisfies-BT-hypothesis? bts))
           (/ success-count (length bts)))))

      (parameterize ([plot-x-tick-label-angle 40]
                     [plot-x-tick-label-anchor 'top-right])
        (plot-pict (discrete-histogram (for/list ([mode-name (in-list modes)])
                                         (list (rename-mode mode-name) (mode-success-% mode-name)))
                                       #:color success-color)
                   #:y-max 1
                   #:y-min 0
                   #:y-label "% of scenarios"
                   #:x-label #f))))
  (pict->png! success-bars (build-path outdir "success-bars.png"))
  (void))

