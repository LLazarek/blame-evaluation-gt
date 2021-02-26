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
  [outdir "./figures"])

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
      (define (make-length-table-cell-plot mode-name)
        (define bts-by-mutator/across-all-benchmarks
          (get-bts-by-mutator-for-mode mode-name))
        (define histogram
          (bt-length-distribution-histogram-for
           "yes"
           (hash "yes"
                 (append* (hash-values bts-by-mutator/across-all-benchmarks)))
           #:normalize? #t
           #:color-by-success? #t
           #:colors '((153 225 187) (255 153 153))))
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
      (define row-spacing 25)
      (define plain-table
        (vc-append row-spacing
                   (vc-append 10
                              (cb-superimpose (first plot-labels) uniform-label-filler)
                              (hash-ref distributions-plots/by-mode "null"))
                   (table/fill-missing plots
                            #:columns 3
                            #:column-spacing column-spacing
                            #:row-spacing row-spacing)))
      plain-table))
  (pict->png! bt-lengths-table (build-path outdir "bt-lengths-table.png")))


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
      (define (bt->id bt)
        (list (blame-trail-mutant-id bt)
              (blame-trail-trail-id bt)))

      (define direct-avo-%
        (simple-memoize
         #:on-disk "direct-avo-percents.rktd"
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

      (define (direct-avo-bar-for top-mode bottom-mode
                                  #:dump-to [dump-to #f])
        (match-define (list p error-margin) (direct-avo-% top-mode bottom-mode dump-to))
        (list (discrete-histogram (list (list "hide me" p))
                                  #:add-ticks? #f)
              (point-label (list 0.5 p)
                           (~a (~r (* 100 p) #:precision 2)
                               "%"
                               ;; " +/- "
                               ;; (~r (* 100 error-margin) #:precision 1)
                               )
                           #:point-size 0
                           #:anchor 'bottom
                           #:size 60)))



      (define (make-avo-matrix-cell-plot top-mode bottom-mode)
        (define direct-avo-bar (direct-avo-bar-for top-mode bottom-mode))
        (parameterize ([plot-x-ticks no-ticks]
                       [plot-y-ticks no-ticks]
                       [plot-y-far-ticks no-ticks])
          (scale (plot-pict direct-avo-bar
                            #:y-min 0
                            #:y-max 1

                            #:x-label #f
                            #:y-label #f
                            #:title #f)
                 0.5
                 0.5)))


      (define filler-blank
        (frame (cc-superimpose
                (ghost (make-avo-matrix-cell-plot (first modes) (first modes)))
                (text "×" null 70))
               #:color "gray"))
      (define plots
        (for*/list ([top (in-list modes)]
                    [bottom (in-list modes)])
          (if (equal? top bottom)
              filler-blank
              (make-avo-matrix-cell-plot top bottom))))
      (define column-labels (map rename-mode modes))
      (define row-labels column-labels)

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
        (apply hb-append
               column-spacing
               (for/list ([col (in-list column-labels)])
                 (cc-superimpose (let ([rotated (rotate (make-text col) (* pi 1/4))])
                                   (inset/clip rotated
                                               (/ (- (pict-width a-cell)
                                                     (pict-width rotated))
                                                  2)))
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
  (pict->png! avo-bars (build-path outdir "avo-bars.png"))
  (void))
