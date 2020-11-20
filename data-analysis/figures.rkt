#lang at-exp rscript

(define (rename-mode mode-name)
  (hash-ref (hash "TR" "Natural"
                  "TR-stack-first" "Natural exceptions"

                  "transient-newest" "Transient last blame"
                  "transient-oldest" "Transient first blame"
                  "transient-stack-first"  "Transient exceptions"

                  "erasure-stack-first" "Erasure"

                  "null" "Null")
            mode-name))
(define rename-benchmark values)

;; (usefulness-table bt-lengths-table avo-matrix)
(define to-generate '(bt-lengths-table))




(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         (except-in pict pict?)
         pict-util/file
         (prefix-in db: "../db/db.rkt")
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../experiment/blame-trail-data.rkt"
         "../configurables/configurables.rkt"
         "../runner/mutation-runner-data.rkt"

         "plot-common.rkt"
         "read-data.rkt"

         "bt-violations.rkt"
         "bt-length-distributions.rkt"
         "adds-value-over.rkt")

(define-runtime-paths
  [data-dirs "../../experiment-data/results/code-mutations"]
  [dyn-err-summaries-db-path "../dbs/code-mutations/dyn-err-summaries.rktdb"]
  [TR-config "../configurables/configs/TR.rkt"]
  [outdir "./figures"])

(install-configuration! TR-config)

(make-directory* outdir)

(define modes
  '("null" "TR" "TR-stack-first" "transient-newest" "transient-oldest" "transient-stack-first" "erasure-stack-first"))
(define benchmarks
  '("acquire"
    "gregor"
    "kcfa"
    "quadT"
    "quadU"
    "snake"
    "suffixtree"
    "synth"
    "take5"
    "tetris"))

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
      (define (make-length-table-cell-plot key bts-by-key
                                           #:dump-to [dump-to #f])
        (define histogram
          (bt-length-distribution-histogram-for key
                                                bts-by-key
                                                #:dump-to dump-to
                                                #:normalize? #t
                                                #:color-by-success? #t))
        (parameterize ([plot-y-ticks no-ticks]
                       [plot-y-far-ticks no-ticks])
          (plot-pict histogram
                     #:y-min 0
                     #:y-max 1

                     #:x-label #f
                     #:y-label #f
                     #:title #f)))

      (define distributions-plots/by-mode
        (for/hash ([mode (in-list modes)])
          (values mode
                  (make-distributions-plots make-length-table-cell-plot
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
                 1
                 1)))

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
           (vc-append (text first-word null 60)
                      (text other-words null 60))]
          [else (text str
                      null
                      60)]))
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
  (pict->png! bt-lengths-table (build-path outdir "bt-lengths-table.png")))

(when (member 'avo-matrix to-generate)
  (define avo-matrix
    (let ([modes (remove "null" modes)])
      (define (make-avo-matrix-cell-plot top-bts-by-mutator bottom-bts-by-mutator)
        (define avo-%
          (adds-value-over->% (adds-value-over top-bts-by-mutator bottom-bts-by-mutator)))
        (parameterize ([plot-x-ticks no-ticks]
                       [plot-y-ticks no-ticks]
                       [plot-y-far-ticks no-ticks])
          (scale (plot-pict (list (discrete-histogram (list (list "" avo-%)))
                                  (point-label (list 0.5 avo-%)
                                               (~r avo-% #:precision 3)
                                               #:point-size 0
                                               #:anchor 'bottom
                                               #:size 50))
                            #:y-min 0
                            #:y-max 1

                            #:x-label #f
                            #:y-label #f
                            #:title #f)
                 0.6
                 0.6)))

      (define mutant-mutators
        (read-mutants-by-mutator dyn-err-summaries-db-path))
      (define mode-bts-by-mutator
        (simple-memoize
         (λ (mode-name)
           (define mode-data-dir (build-path data-dirs mode-name))
           (add-missing-active-mutators
            (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir mutant-mutators)))))
      (define filler-blank
        (cc-superimpose
         (ghost (make-avo-matrix-cell-plot (mode-bts-by-mutator (first modes))
                                           (mode-bts-by-mutator (first modes))))
         (text "×" null 40)))
      (define plots
        (for*/list ([top (in-list modes)]
                    [bottom (in-list modes)])
          (if (equal? top bottom)
              filler-blank
              (make-avo-matrix-cell-plot (mode-bts-by-mutator top)
                                         (mode-bts-by-mutator bottom)))))
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
                 (cc-superimpose (rotate (make-text col) (* pi 1/4))
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
  (pict->png! avo-matrix (build-path outdir "avo-matrix.png")))
