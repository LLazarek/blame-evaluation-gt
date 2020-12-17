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
  '("null" "TR" "TR-stack-first" "transient-newest" "transient-oldest" "transient-all" "transient-stack-first" "erasure-stack-first"))
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

(define mutators (configured:active-mutator-names))

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
                                 (plot-width)))))

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

(when (member 'avo-matrix to-generate)
  (define avo-matrix
    (let ([modes (remove "null" modes)])
      (define (stderr->variance stderr)
        (sqr stderr))
      (define (sqrt* n)
        (when (negative? n) (displayln @~a{sqrt of negative: @n}))
        (sqrt n))
      (define (variance->stderr variance)
        (sqrt* variance))
      (define (variance->margin-of-error variance confidence-z-value)
        (* (variance->stderr variance) confidence-z-value))

      ;; proportion-estimate/c :=
      ;; (hashof 'proportion-estimate (real-in 0 1)
      ;;         'variance            real?
      ;;         'sample-size         natural?
      ;;         'population-size     natural?)

      ;; (listof any/c) (any/c . -> . boolean?) natural? [(real-in 0 1) real?]
      ;; ->
      ;; proportion-estimate/c
      ;; estimate? := (real-in 0 1)
      ;; variance? := real?
      (define (estimate-proportion sample
                                   predicate
                                   population-size
                                   [confidence 95/100]
                                   [confidence-z-value 1.96])
        (define sample-size (length sample))
        (define proportion-estimate (/ (count predicate sample) sample-size))
        ;; variance of a proportion estimate within a sample from
        ;; https://stattrek.com/survey-research/stratified-sampling-analysis.aspx
        (define variance (/ (* sample-size proportion-estimate (- 1 proportion-estimate))
                            (sub1 sample-size)))
        (hash 'proportion-estimate proportion-estimate
              'variance variance
              'sample-size sample-size
              'population-size population-size))

      (define max-bt-population-sizes-by-benchmark
        ;; computed in sizes.rkt: possible-interesting-scenario-counts/by-benchmark
        #hash(("acquire" . 256)
              ("gregor" . 4096)
              ("kcfa" . 64)
              ("quadT" . 8192)
              ("quadU" . 8192)
              ("snake" . 128)
              ("suffixtree" . 32)
              ("synth" . 512)
              ("take5" . 128)
              ("tetris" . 256)))
      (define number-of-interesting-mutants-available-by-benchmark+mutator
        ;; computed in sizes.rkt: mutants-with-interesting-scenarios-count/by-benchmark+mutator
        #hash((("synth" "top-level-id-swap") . 40)
              (("kcfa" "constant-swap") . 2)
              (("tetris" "top-level-id-swap") . 47)
              (("synth" "begin-result-deletion") . 3)
              (("suffixtree" "constant-swap") . 31)
              (("gregor" "force-conditional") . 0)
              (("kcfa" "position-swap") . 15)
              (("suffixtree" "position-swap") . 127)
              (("suffixtree" "boolean-op-swap") . 1)
              (("take5" "method-id-swap") . 20)
              (("synth" "imported-id-swap") . 10)
              (("snake" "top-level-id-swap") . 7)
              (("acquire" "arithmetic-op-swap") . 0)
              (("tetris" "imported-id-swap") . 28)
              (("quadT" "arithmetic-op-swap") . 2)
              (("quadU" "arithmetic-op-swap") . 2)
              (("gregor" "negate-conditional") . 0)
              (("kcfa" "top-level-id-swap") . 41)
              (("suffixtree" "top-level-id-swap") . 367)
              (("suffixtree" "begin-result-deletion") . 11)
              (("quadT" "class:super-new") . 0)
              (("kcfa" "imported-id-swap") . 20)
              (("acquire" "class:super-new") . 0)
              (("acquire" "class:initializer-swap") . 0)
              (("acquire" "field-id-swap") . 0)
              (("suffixtree" "imported-id-swap") . 349)
              (("take5" "arithmetic-op-swap") . 0)
              (("quadU" "class:super-new") . 0)
              (("quadT" "class:parent-swap") . 0)
              (("acquire" "boolean-op-swap") . 5)
              (("quadT" "boolean-op-swap") . 9)
              (("acquire" "constant-swap") . 90)
              (("acquire" "class:parent-swap") . 0)
              (("synth" "force-conditional") . 0)
              (("quadT" "position-swap") . 430)
              (("quadU" "boolean-op-swap") . 8)
              (("quadT" "constant-swap") . 177)
              (("kcfa" "nested-list-construction-swap") . 0)
              (("quadU" "position-swap") . 403)
              (("quadU" "constant-swap") . 173)
              (("acquire" "position-swap") . 64)
              (("quadU" "class:parent-swap") . 0)
              (("take5" "class:super-new") . 4)
              (("take5" "class:initializer-swap") . 1)
              (("take5" "field-id-swap") . 6)
              (("quadU" "begin-result-deletion") . 5)
              (("acquire" "top-level-id-swap") . 2551)
              (("acquire" "begin-result-deletion") . 1)
              (("kcfa" "force-conditional") . 0)
              (("quadT" "class:publicity") . 0)
              (("take5" "position-swap") . 22)
              (("suffixtree" "force-conditional") . 4)
              (("take5" "constant-swap") . 16)
              (("tetris" "negate-conditional") . 0)
              (("take5" "class:parent-swap") . 2)
              (("quadT" "begin-result-deletion") . 7)
              (("acquire" "class:publicity") . 2)
              (("take5" "boolean-op-swap") . 0)
              (("synth" "negate-conditional") . 2)
              (("quadT" "top-level-id-swap") . 3285)
              (("quadU" "class:publicity") . 0)
              (("quadU" "top-level-id-swap") . 3243)
              (("quadU" "imported-id-swap") . 1729)
              (("gregor" "arithmetic-op-swap") . 4)
              (("acquire" "imported-id-swap") . 296)
              (("snake" "negate-conditional") . 1)
              (("quadT" "imported-id-swap") . 1514)
              (("quadT" "nested-list-construction-swap") . 9)
              (("take5" "class:publicity") . 2)
              (("suffixtree" "negate-conditional") . 5)
              (("take5" "top-level-id-swap") . 28)
              (("quadU" "nested-list-construction-swap") . 11)
              (("acquire" "nested-list-construction-swap") . 2)
              (("kcfa" "negate-conditional") . 0)
              (("take5" "begin-result-deletion") . 2)
              (("quadT" "force-conditional") . 26)
              (("gregor" "boolean-op-swap") . 0)
              (("acquire" "force-conditional") . 1)
              (("gregor" "position-swap") . 32)
              (("quadU" "force-conditional") . 16)
              (("gregor" "constant-swap") . 690)
              (("gregor" "top-level-id-swap") . 293)
              (("quadU" "negate-conditional") . 23)
              (("gregor" "begin-result-deletion") . 1)
              (("acquire" "negate-conditional") . 2)
              (("take5" "force-conditional") . 1)
              (("quadT" "negate-conditional") . 36)
              (("gregor" "imported-id-swap") . 79)
              (("suffixtree" "arithmetic-op-swap") . 2)
              (("take5" "negate-conditional") . 2)
              (("synth" "position-swap") . 40)
              (("synth" "constant-swap") . 104)
              (("tetris" "position-swap") . 24)
              (("gregor" "nested-list-construction-swap") . 0)
              (("tetris" "constant-swap") . 140)
              (("synth" "boolean-op-swap") . 1)
              (("quadT" "method-id-swap") . 1)
              (("quadU" "method-id-swap") . 2)
              (("acquire" "method-id-swap") . 14)
              (("snake" "position-swap") . 14)
              (("snake" "constant-swap") . 12)
              (("take5" "imported-id-swap") . 4)
              (("synth" "arithmetic-op-swap") . 4)))

      (define (possible-interesting-mutants benchmark mutator)
        (hash-ref number-of-interesting-mutants-available-by-benchmark+mutator
                  (list benchmark mutator)
                  0 ;; if it's not there, then there were no mutants in the first place for that mutator
                  ))
      ;; (blame-trail? . -> . boolean?)
      ;; (benchmark? mutator . -> . (hash/c mutant? blame-trail?))
      ;; . -> .
      ;; proportion-estimate/c
      (define (strata-proportion-estimate predicate bts-by-mutant-for)
        (define benchmark-bucket-estimates
          (for/list ([benchmark (in-list benchmarks)])
            (define mutator-bucket-estimates
              (for/list ([mutator (in-list mutators)])
                (define bts-by-mutant (bts-by-mutant-for benchmark mutator))
                (define mutant-bucket-estimates
                  (for/list ([{mutant bts} (in-hash bts-by-mutant)])
                    (define max-bt-population-size
                      (hash-ref max-bt-population-sizes-by-benchmark
                                (mutant-benchmark mutant)))
                    (estimate-proportion bts
                                         predicate
                                         (max max-bt-population-size
                                              96))))
                (combine-subgroup-estimates mutant-bucket-estimates
                                            #;(possible-interesting-mutants benchmark mutator))))
            (combine-subgroup-estimates mutator-bucket-estimates
                                        #;16)))
        (combine-subgroup-estimates benchmark-bucket-estimates
                                    #;10))

      (define (combine-subgroup-estimates estimates
                                          #;total-group-population-size)
        ;; Based entirely on:
        ;; https://stattrek.com/survey-research/stratified-sampling-analysis.aspx
        (cond [(empty? estimates)
               #f]
              [else
               (define non-empty-subgroup-estimates (filter-not false? estimates))
               (define total-group-population-size
                 (for/sum ([subgroup-estimate (in-list non-empty-subgroup-estimates)])
                   (hash-ref subgroup-estimate
                             'population-size)))
               (define total-group-sample-size
                 (for/sum ([subgroup-estimate (in-list non-empty-subgroup-estimates)])
                   (hash-ref subgroup-estimate
                             'sample-size)))
               (unless (>= total-group-population-size total-group-sample-size)
                 (error 'combine @~a{@total-group-population-size < @total-group-sample-size}))
               (define group-proportion-estimate
                 (for/sum ([estimate (in-list non-empty-subgroup-estimates)])
                   (* (/ (hash-ref estimate 'sample-size)
                         total-group-sample-size)
                      (hash-ref estimate 'proportion-estimate))))
               (define group-estimate-stderr
                 (* (/ 1 total-group-population-size)
                    (sqrt* (for/sum ([estimate (in-list non-empty-subgroup-estimates)])
                            (* (sqr (hash-ref estimate 'population-size))
                               (- 1 (/ (hash-ref estimate 'sample-size)
                                       (hash-ref estimate 'population-size)))
                               (/ (hash-ref estimate 'variance)
                                  (hash-ref estimate 'sample-size)))))))
               (define group-variance (stderr->variance group-estimate-stderr))
               (hash 'sample-size (length non-empty-subgroup-estimates)
                     'proportion-estimate group-proportion-estimate
                     'variance group-variance
                     'population-size total-group-population-size)]))




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

      (define (direct-avo-bar-for top-mode bottom-mode
                                  #:dump-to [dump-to #f])
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
  (pict->png! avo-matrix (build-path outdir "avo-matrix.png"))
  (void))
