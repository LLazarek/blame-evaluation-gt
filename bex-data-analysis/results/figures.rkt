#lang at-exp rscript

(provide mutant-mutators
         get-bts-by-mutator-for-mode
         get-bts-by-mutator-for-mode-backdoor

         generate-figure:bt-lengths-table
         generate-figure:bt-length-comparisons
         generate-figure:avo-bars
         generate-figure:success-bars

         data-cache
         use-disk-data-cache?
         collect-max-error-margin?)

(define (rename-mode mode-name)
  (hash-ref (hash "TR" "Natural blame"
                  "TR-stack-first" "Natural exceptions"

                  "transient-newest" "Transient last blame"
                  "transient-oldest" "Transient first blame"
                  "transient-stack-first"  "Transient exceptions"
                  "transient-all"  "Transient all blame"

                  "erasure-stack-first" "Erasure"

                  "TR-null" "Random")
            mode-name
            mode-name))
(define rename-benchmark values)

;; (bt-lengths-table bt-length-comparisons avo-bars blame-vs-exns-venn success-bars detection-bars client-side-success-bars)
(define to-generate '(bt-lengths-table #;bt-length-comparisons avo-bars #;blame-vs-exns-venn success-bars
                      detection-bars client-side-success-bars))
(plot-font-size 14)
(define (plot-title-size) (inexact->exact (truncate (* 1.5 (plot-font-size)))))

;; How to render percentages: within [0,1] or [0,100]
(define (~% %) (* % 100))

;; As the figures are written below, this will only take effect if the figures
;; are *not* pulling data from a cache on disk ...
(define collect-max-error-margin? (make-parameter #t))
;; ... i.e. this is not #t
(define use-disk-data-cache? (make-parameter #f))


(define max-error-margin (box 0))
(define (record-error-margin! e)
  (when (> e (unbox max-error-margin))
    (set-box! max-error-margin e)))

(require plot
         (except-in pict-util line)
         (except-in pict pict?)
         pict-util/file
         bex/configurables/configurables
         bex/util/for

         "plot-common.rkt"
         "read-data.rkt"

         "bt-violations.rkt"
         "bt-length-distributions.rkt"
         "adds-value-over.rkt"
         "experiment-info.rkt"
         "stratified-proportion-estimation.rkt"
         "bt-ids.rkt")

(define-runtime-paths
  [data-dirs "../../../experiment-data/results/type-api-mutations"]
  [mutant-summaries-db-path "../../bex/dbs/type-api-mutations/type-err-summaries.rktdb"]
  [TR-config "../../bex/configurables/configs/TR.rkt"]
  [outdir "../../../experiment-data/results/type-api-mutations"]
  [data-cache "./data-cache"]
  [venn-template "venn-template.svg"])

(define plot-name-prefix (basename data-dirs))

;; for getting mutator names
(install-configuration! TR-config)


(define mutant-mutators
  (read-mutants-by-mutator mutant-summaries-db-path))
(define get-bts-by-mutator-for-mode
  (simple-memoize
   (λ (mode-name)
     (cond [(and (get-bts-by-mutator-for-mode-backdoor)
                 ((get-bts-by-mutator-for-mode-backdoor) mode-name)) => values]
           [else
            (define mode-data-dir (build-path data-dirs mode-name))
            (add-missing-active-mutators
             (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir
                                                                 mutant-mutators))]))))
(define get-bts-by-mutator-for-mode-backdoor (make-parameter #f))

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


(define/contract (bt-wise-strata-proportion-estimate mode-names
                                                     predicate)
  (->i ([mode-names (non-empty-listof string?)]
        [predicate {mode-names}
                   ((and/c (non-empty-listof blame-trail?)
                           (λ (l) (= (length l) (length mode-names))))
                    . -> .
                    boolean?)])
       [result hash?])

  (define (mode->bts-by-id mode)
    (define bts-by-mutator (get-bts-by-mutator-for-mode mode))
    (bts-by-id bts-by-mutator))

  (match-define (cons top-mode other-modes) mode-names)
  (define other-modes-bts-by-id (map mode->bts-by-id other-modes))

  (define get-top-bts-by-benchmark+mutant
    (make-bt-by-benchmark+mutant-getter-for-mode top-mode))
  (define get-bts-by-benchmark+mutant
    (simple-memoize
     (λ (benchmark mutator)
       (define top-bts-by-mutant (get-top-bts-by-benchmark+mutant benchmark mutator))
       (for/hash ([{mutant top-bts} (in-hash top-bts-by-mutant)])
         (values mutant
                 (for/list ([top-bt (in-list top-bts)])
                   (define top-id (bt->id top-bt))
                   (cons top-bt
                         (map (λ (bts-by-id) (hash-ref bts-by-id top-id))
                              other-modes-bts-by-id))))))))
  (strata-proportion-estimate predicate
                              get-bts-by-benchmark+mutant))


(define success-color '(153 225 187))
(define failure-color '(255 153 153))

(define/contract (add-multiline-labels picts
                                       labels
                                       label->lines
                                       #:spacing [label-gap 10]
                                       #:style [style null]
                                       #:size [size 12])
  ({(listof pict?) (listof any/c) (any/c . -> . (listof string?))}
   {#:spacing real?
    #:style any/c
    #:size natural?}
   . ->* .
   (listof pict?))

  (define (text* str) (text str style size))
  (define label-picts
    (for/list ([label (in-list labels)])
      (apply vc-append
             (map text* (label->lines label)))))
  (define uniform-label-filler (ghost (foldl cc-superimpose (blank 0) label-picts)))
  (for/list ([pict (in-list picts)]
             [label-pict (in-list label-picts)])
    (vc-append label-gap
               (cb-superimpose label-pict uniform-label-filler)
               pict)))

(define (round-up-to-next-multiple n of)
  (define x (/ n of))
  (if (integer? x)
      (+ n of)
      (* (inexact->exact (ceiling x)) of)))

(define (generate-figure:bt-lengths-table modes/ordered)
  (define mode->max-trail-length
    (match-lambda ["TR-null" 10]
                  [else 4]))
  (define bt-length-distribution-for-mode
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "bt-length-distributions.rktd"))
     (λ (mode-name)
       (define bt-length/memo (simple-memoize bt-length))
       (define (estimate-length-proportion length successful?)
         (displayln @~a{Computing @mode-name @length @successful?})
         (define estimate
           (bt-wise-strata-proportion-estimate
            (list mode-name)
            (match-lambda [(list bt)
                           (and (= (bt-length/memo bt #t) length)
                                (if successful?
                                    (satisfies-BT-hypothesis? bt)
                                    (not (satisfies-BT-hypothesis? bt))))])))
         (when (collect-max-error-margin?)
           (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                            1.96)))
         (displayln @~a{
                        @"  "-> @(exact->inexact (~% (hash-ref estimate 'proportion-estimate)))%

                        })
         (hash-ref estimate 'proportion-estimate))
       (for/list ([length (in-range (add1 (mode->max-trail-length mode-name)))])
         (list length
               (list (estimate-length-proportion length #t)
                     (estimate-length-proportion length #f)))))))

  (define (make-length-table-cell-plot mode-name)
    (define length-distribution (bt-length-distribution-for-mode mode-name))
    (define histogram
      (stacked-histogram (map (match-lambda [`(,len (,%1 ,%2))
                                             (list len (list (~% %1) (~% %2)))])
                              length-distribution)
                         #:colors (list success-color failure-color)
                         #:line-colors (list "black" "black")))
    (parameterize ([plot-x-ticks (ticks (linear-ticks-layout #:number 1)
                                        (linear-ticks-format))]
                   [plot-y-far-ticks no-ticks])
      (plot-pict histogram
                 #:y-min (~% 0)
                 #:y-max (~% 1)

                 #:x-label #f
                 #:y-label "% of trails with length"
                 #:title #f
                 #:width (if (equal? mode-name "TR-null")
                             (* 2 (plot-width))
                             (plot-width))
                 #:x-max (if (equal? mode-name "TR-null") #f (add1 (mode->max-trail-length "TR"))))))

  (define plots/ordered
    (for/list ([mode (in-list modes/ordered)])
      (make-length-table-cell-plot mode)))
  (define (split-mode-name name)
    (define split (string-split name))
    (match split
      [(list* first-word other-words)
       (list first-word (string-join other-words))]
      [other-split other-split]))
  (define labeled-plots/ordered
    (add-multiline-labels plots/ordered
                          (map rename-mode modes/ordered)
                          split-mode-name
                          #:style (or (plot-font-face) (plot-font-family))
                          #:size (plot-title-size)))

  (define column-spacing 30)
  (define row-spacing 50)
  (define plain-table
    (vc-append (/ row-spacing 2)
               (first labeled-plots/ordered)
               (table/fill-missing (rest labeled-plots/ordered)
                                   #:columns 3
                                   #:column-spacing column-spacing
                                   #:row-spacing row-spacing)))
  plain-table)

(define (generate-figure:bt-length-comparisons mode-comparisons)
  (define length-differences '(-inf.0 -3 -2 -1 0 1 2 3 +inf.0))
  (define direct-bt-length-comparison-distribution
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "direct-effort-comparison-distributions.rktd"))
     (λ (top-mode bottom-mode dump-to)
       (define bt-length/memo (simple-memoize bt-length))
       (define (estimate-Δ-proportion Δ)
         (displayln @~a{Computing @top-mode vs @bottom-mode @Δ})
         (define estimate
           (bt-wise-strata-proportion-estimate
            (list top-mode bottom-mode)
            (match-lambda [(list top-bt bot-bt)
                           #:when (equal? Δ -inf.0)
                           (and (satisfies-BT-hypothesis? top-bt)
                                (not (satisfies-BT-hypothesis? bot-bt)))]
                          [(list top-bt bot-bt)
                           #:when (equal? Δ +inf.0)
                           (and (not (satisfies-BT-hypothesis? top-bt))
                                (satisfies-BT-hypothesis? bot-bt))]
                          [(list top-bt bot-bt)
                           (and (satisfies-BT-hypothesis? top-bt)
                                (satisfies-BT-hypothesis? bot-bt)
                                (= (- (bt-length/memo top-bt #t)
                                      (bt-length/memo bot-bt #t))
                                   Δ))])))
         (when (collect-max-error-margin?)
           (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                            1.96)))
         (displayln @~a{
                        @"  "-> @(exact->inexact (~% (hash-ref estimate 'proportion-estimate)))%

                        })
         (hash-ref estimate 'proportion-estimate))
       (for/list ([Δ (in-list length-differences)])
         (list Δ
               (estimate-Δ-proportion Δ))))))

  (define Δ-distributions
    (for/list ([comparison (in-list mode-comparisons)])
      (match-define (list top-mode other-mode) comparison)
      (direct-bt-length-comparison-distribution top-mode other-mode #f)))

  (define biggest-% (argmax values
                            (for*/list ([Δ-distribution (in-list Δ-distributions)]
                                        [% (in-dict-values Δ-distribution)])
                              (first %))))
  (define y-max% (/ (round-up-to-next-multiple (* 100 biggest-%) 5) 100))

  (define (direct-bt-length-comparison Δ-distribution)
    (define compression-zone-bottom 0.55)
    (define compression-zone-top 0.8)
    ;; lltodo: there seems to be another bug with discrete-histogram here
    ;; this doesn't work at all:
    #;(plot (list (discrete-histogram '((1 2) (2 10))
                                      #:color "red")
                  (discrete-histogram '((0 20))
                                      #:color "gray")
                  (discrete-histogram '((-1 8) (-2 5))
                                      #:color "green")))
    ;; Actually, there just need to be placeholders for every x-value that I want
    ;; to show up in the final plot, in every group (see below)
    (parameterize (#;[plot-y-transform (axis-transform-compose
                                      (axis-transform-compose
                                       (collapse-transform (~% compression-zone-bottom)
                                                           (~% compression-zone-top))
                                       (stretch-transform (~% 0)
                                                          (~% compression-zone-bottom)
                                                          2))
                                      (stretch-transform (~% compression-zone-top)
                                                         (~% 1)
                                                         1/3))]
                   [plot-y-ticks (ticks-add (plot-y-ticks)
                                            (map ~% (append (range 0 y-max% 0.05) (list y-max%))))])
      (plot-pict (list
                  (for/list ([filter (in-list (list (</c 0) (=/c 0) (>/c 0)))]
                             [color (in-list (list success-color
                                                   '(242 242 242)
                                                   failure-color))])
                    (define group-data (for/list ([{Δ %l} (in-dict Δ-distribution)])
                                         (define display-Δ
                                           (match Δ
                                             [-inf.0 "-∞"]
                                             [+inf.0 "+∞"]
                                             [v v]))
                                         (list display-Δ
                                               ;; Need a placeholder for every Δ, even if
                                               ;; it's not in this group
                                               (if (filter Δ) (~% (first %l)) 0))))
                    (discrete-histogram group-data
                                        #:color color))
                  #;(x-axis (~% compression-zone-bottom) #:ticks? #f #:alpha 0.7))
                 #:title #f
                 #:y-label "% of mutually-successful scenarios"
                 #:x-label @~a{trail length difference}
                 #:y-min (~% 0)
                 #:y-max (~% y-max%)
                 #:x-min 0
                 #:x-max (length Δ-distribution))))

  (define-values {plots labels/lines}
    (for/lists {plots labels/lines}
               ([comparison (in-list mode-comparisons)]
                [Δ-distribution (in-list Δ-distributions)])
      (match-define (list top-mode other-mode) comparison)
      (values (direct-bt-length-comparison Δ-distribution)
              (list (rename-mode top-mode) "vs" (rename-mode other-mode)))))
  (define (text* str)
    (text str
          (or (plot-font-face) (plot-font-family))
          (plot-title-size)))
  (define (make-vs-aligned-pict parts)
    (define (multiline-mode-name mode)
      (match (string-split mode)
        [(list first more ..1) (list first (string-join more))]
        [single             single]))
    (define (vs-table left-top right-top
                      left-bot right-bot)
      (define left-col (vl-append 5 (text* left-top) (text* left-bot)))
      (define right-col (vl-append 5 (text* right-top) (text* right-bot)))
      (define center-col (text* "vs"))
      (define padded-left-col
        (rc-superimpose left-col
                        (ghost right-col)))
      (define padded-right-col
        (lc-superimpose right-col
                        (ghost left-col)))
      (ht-append 20 padded-left-col center-col padded-right-col))
    (match-define (list left "vs" right) parts)
    (define left-lines (multiline-mode-name left))
    (define right-lines (multiline-mode-name right))
    (match* {left-lines right-lines}
      [{(list single) (list top bottom)}
       (vs-table single top
                 ""     bottom)]
      [{(list left-top left-bot) (list right-top right-bot)}
       (vs-table left-top right-top
                 left-bot right-bot)]))
  (define (pad-to-width pict w)
    (define difference (- w (pict-width pict)))
    (blank-pad pict
               #:left (/ difference 2.0)
               #:right (/ difference 2.0)))
  (define labeled-plots
    (for/list ([plot (in-list plots)]
               [label-parts (in-list labels/lines)])
      (vr-append 10
                 (blank-pad (pad-to-width (make-vs-aligned-pict label-parts)
                                          300)
                            #:right 8)
                 plot)))
  (table/fill-missing labeled-plots
                      #:columns 3
                      #:column-spacing 15
                      #:row-spacing 50))

(define (generate-figure:avo-bars modes/ordered bottom-modes)
  ;; (define y-max/min-% 0.4)
  (define direct-avo-%
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "direct-avo-percents.rktd"))
     (λ (top-mode bottom-mode dump-to)
       (displayln @~a{@top-mode vs @bottom-mode})
       (define estimate
         (bt-wise-strata-proportion-estimate
          (list top-mode bottom-mode)
          (match-lambda
            [(list top-bt bottom-bt)
             (and (satisfies-BT-hypothesis? top-bt)
                  (not (satisfies-BT-hypothesis? bottom-bt)))])))
       (define p (hash-ref estimate 'proportion-estimate))
       (define error-margin (variance->margin-of-error (hash-ref estimate 'variance)
                                                       1.96))
       (record-error-margin! error-margin)
       (list p error-margin))))

  (define (direct-avo-comparisons top-mode other-modes)
    (define comparison-data
      (for/list ([other-mode (in-list other-modes)]
                 [index (in-naturals)])
        (match-define (list top/other top/other-error-margin) (direct-avo-% top-mode other-mode #f))
        (match-define (list other/top other/top-error-margin) (direct-avo-% other-mode top-mode #f))
        (list (rename-mode other-mode) (~% top/other) (~% other/top))))
    comparison-data)

  (define direct-avo-comparisons-data
    (for/list ([top (in-list modes/ordered)])
      (cons top (direct-avo-comparisons top bottom-modes))))

  (define biggest-%
    (argmax abs
            (for*/list ([comparisons (in-dict-values direct-avo-comparisons-data)]
                        [comparison (in-list comparisons)])
              (second comparison))))
  (define y-max/min-% (/ (round-up-to-next-multiple (abs biggest-%) 5) 100))

  (define (direct-avo-comparisons-plot top-mode comparison-data)
    (parameterize ([plot-x-tick-label-angle 40]
                   [plot-x-tick-label-anchor 'top-right]
                   [plot-y-ticks (absolute-value-format (linear-ticks #:number 15))]
                   [plot-font-size 16])
      (plot-pict (two-sided-histogram comparison-data
                                      #:top-color success-color
                                      #:bot-color failure-color)
                 #:title (rename-mode top-mode)
                 #:y-label @~a{% of scenarios less useful    % of scenarios more useful}
                 #:x-label #f
                 #:y-min (~% (- y-max/min-%))
                 #:y-max (~% y-max/min-%)
                 #:height (* 2 (plot-height))
                 #:width (* 1.3 (plot-width)))))

  (define plots
    (for/list ([{top comparison-data} (in-dict direct-avo-comparisons-data)])
      (direct-avo-comparisons-plot top comparison-data)))

  #;(apply vc-append
           20
           plots)
  (table/fill-missing plots
                      #:columns 3
                      #:column-spacing 10
                      #:row-spacing 80))

(define (generate-figure:success-bars modes/ordered)
  (define mode-success-%
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "success-percents-prop-estimate.rktd"))
     (λ (mode-name)
       (define estimate (bt-wise-strata-proportion-estimate
                         (list mode-name)
                         (match-lambda [(list bt) (satisfies-BT-hypothesis? bt)])))
       (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                        1.96))
       (hash-ref estimate 'proportion-estimate))))

  (parameterize ([plot-x-tick-label-angle 40]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-y-ticks (linear-ticks #:number 10)]
                 [plot-y-far-ticks no-ticks])
    (plot-pict (discrete-histogram (for/list ([mode-name (in-list modes/ordered)])
                                     (list (rename-mode mode-name) (~% (mode-success-% mode-name))))
                                   #:color success-color)
               #:y-max (~% 1)
               #:y-min (~% 0)
               #:y-label "% of scenarios successful"
               #:x-label #f
               #:width (* 1.25 (plot-width))
               #:height (* 1.25 (plot-height)))))

(define (generate-figure:detection-bars modes/ordered)
  (local-require bex/experiment/blame-trail-data
                 bex/runner/mutation-runner-data)
  (define undetected?
    (match-lambda
      [(struct* blame-trail
                ([mutant-summaries
                  (cons (mutant-summary _
                                        (struct* run-status ([outcome outcome]))
                                        _)
                        _)]))
       (equal? outcome 'completed)]))
  (define mode-detection-%
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "detection-percents-prop-estimate.rktd"))
     (λ (mode-name)
       (define estimate (bt-wise-strata-proportion-estimate
                         (list mode-name)
                         (match-lambda [(list bt) (undetected? bt)])))
       (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                        1.96))
       (hash-ref estimate 'proportion-estimate))))

  (parameterize ([plot-x-tick-label-angle 40]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-y-ticks (linear-ticks #:number 10)]
                 [plot-y-far-ticks no-ticks])
    (plot-pict (discrete-histogram (for/list ([mode-name (in-list modes/ordered)])
                                     (list (rename-mode mode-name)
                                           (~% (- 1 (mode-detection-% mode-name)))))
                                   #:color success-color)
               #:y-max (~% 1)
               #:y-min (~% 0)
               #:y-label "% of scenarios producing error"
               #:x-label #f
               #:width (* 1.25 (plot-width))
               #:height (* 1.25 (plot-height)))))
#;(define (generate-figure:detection-bars modes/ordered)
  (local-require complot
                 data-frame
                 sawzall
                 threading
                 racket/hash
                 "aggregate-data.rkt"
                 bex/experiment/blame-trail-data
                 bex/runner/mutation-runner-data)

  (define undetected?
    (match-lambda
      [(cons (mutant-summary _
                             (struct* run-status ([outcome outcome]))
                             _)
             _)
       (equal? outcome 'completed)]))
  (define ((order-like an-ordering) a b)
    (< (or (index-of an-ordering a) +inf.0)
       (or (index-of an-ordering b) +inf.0)))

  (define data (read-blame-trail-db->df (build-path data-dirs "data.sqlite")))
  (define detection-bars-data (~> data
                                  (group-with "mode")
                                  (aggregate [detection-rate
                                              (mutant-summaries)
                                              (for/fold ([missed 0]
                                                         [total 0]
                                                         #:result (~% (/ (- total missed) total)))
                                                        ([trail-summaries (in-vector mutant-summaries)])
                                                (values (if (undetected? trail-summaries)
                                                            (add1 missed)
                                                            missed)
                                                        (add1 total)))])))
  (render (add-to (plot detection-bars-data)
                  (x-axis)
                  (y-axis #:max (~% 1) #:min (~% 0) #:label "% of scenarios")
                  (bars #:x "mode" #:y "detection-rate"
                        #:color success-color
                        #:bar-ordering (order-like modes/ordered)))
          #:width (* 1.25 (plot-width))
          #:height (* 1.25 (plot-height))))

(define (generate-figure:client-side-success-bars modes/ordered)
  (local-require bex/experiment/blame-trail-data
                 bex/runner/mutation-runner-data)
  (define client-mods-by-benchmark
    (hash "acquire" '(main.rkt player.rkt strategy.rkt)
          "gregor" '(main.rkt)
          "kcfa" '(main.rkt ui.rkt)
          "quadT" '(main.rkt quad-main.rkt)
          "quadU" '(main.rkt quad-main.rkt)
          "snake" '(main.rkt handlers.rkt)
          "synth" '(main.rkt sequencer.rkt mixer.rkt synth.rkt drum.rkt)
          "take5" '(main.rkt player.rkt)
          "tetris" '(main.rkt world.rkt)
          "suffixtree" '(main.rkt lcs.rkt)
          "sieve" '(main.rkt)))
  (define ((server-side? mutant) mod-name)
    (not (member mod-name
                 (map ~a
                      (hash-ref client-mods-by-benchmark
                                (mutant-benchmark mutant))))))
  (define (hash-diff-keys a b) ; assuming a and b have the same keys
    (for/list ([{ka va} (in-hash a)]
               #:unless (equal? (hash-ref b ka) va))
      ka))
  (define (configs->typed-module-sequence config-seq)
    (match config-seq
      [(list a b _ ...)
       (for/list ([before-config (in-list config-seq)]
                  [after-config  (in-list (rest config-seq))])
         (first (hash-diff-keys before-config after-config)))]
      [else empty]))
  (define types-server-side?
    (match-lambda
      [(struct* blame-trail
                ([mutant-summaries
                  (list (mutant-summary _ _ configs)
                        ...)]
                 [mutant-id mutant]))
       (unless (subset? (map ~a (hash-ref client-mods-by-benchmark (mutant-benchmark mutant)))
                        (hash-keys (first configs)))
         (error 'assert
                @~a{
                    bad client mod name(s) for @(mutant-benchmark mutant):
                    @~s[(map ~a (hash-ref client-mods-by-benchmark (mutant-benchmark mutant)))]
                    is not a subset of
                    @~s[(hash-keys (first configs))]
                    }))
       (define modules-typed (configs->typed-module-sequence (reverse configs)))
       (ormap (server-side? mutant)
              modules-typed)]))
  (define mode-success-%
    (simple-memoize
     #:on-disk (and (use-disk-data-cache?)
                    (build-path data-cache "client-side-success-percents-prop-estimate.rktd"))
     (λ (mode-name)
       (define estimate (bt-wise-strata-proportion-estimate
                         (list mode-name)
                         (match-lambda [(list bt) (and (satisfies-BT-hypothesis? bt)
                                                       (not (types-server-side? bt)))])))
       (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                        1.96))
       (hash-ref estimate 'proportion-estimate))))

  (parameterize ([plot-x-tick-label-angle 40]
                 [plot-x-tick-label-anchor 'top-right]
                 [plot-y-ticks (linear-ticks #:number 10)]
                 [plot-y-far-ticks no-ticks])
    (plot-pict (discrete-histogram (for/list ([mode-name (in-list modes/ordered)])
                                     (list (rename-mode mode-name)
                                           (~% (mode-success-% mode-name))))
                                   #:color success-color)
               #:y-max (~% 1)
               #:y-min (~% 0)
               #:y-label "% of scenarios successful"
               #:x-label #f
               #:width (* 1.25 (plot-width))
               #:height (* 1.25 (plot-height)))))

(module+ main
  (make-directory* outdir)
  (define (pict->figure-pdf! pict name)
    (pict->pdf! pict (build-path outdir (~a plot-name-prefix "-" name ".pdf"))))

  (when (member 'bt-lengths-table to-generate)
    (define bt-lengths-table
      (let ()
        (define modes/ordered '("TR-null"
                                "TR" "transient-newest" "transient-oldest"
                                "TR-stack-first" "transient-stack-first" "erasure-stack-first"))
        (generate-figure:bt-lengths-table modes/ordered)))
    (pict->figure-pdf! bt-lengths-table "bt-lengths-table")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for bt-lengths-table: @(unbox max-error-margin)})
      (set-box! max-error-margin 0)))

  (when (member 'bt-length-comparisons to-generate)
    (define bt-length-comparisons
      (let ()
        (define mode-comparisons '(["TR" "TR-stack-first"]
                                   ["TR" "transient-newest"]
                                   ["TR" "transient-oldest"]

                                   ["transient-newest" "transient-oldest"]
                                   ["transient-newest" "transient-stack-first"]
                                   ["transient-oldest" "transient-stack-first"]))
        (generate-figure:bt-length-comparisons mode-comparisons)))
    (pict->figure-pdf! bt-length-comparisons "bt-length-comparisons")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for bt-lengths-comparisons: @(unbox max-error-margin)})
      (set-box! max-error-margin 0)))


  (when (member 'avo-bars to-generate)
    (define avo-bars
      (let ()
        (define modes/ordered '("TR" "transient-newest" "transient-oldest"
                                     "TR-stack-first" "transient-stack-first" "erasure-stack-first"))
        (generate-figure:avo-bars modes/ordered
                                  (remove "TR-null" modes))))
    (pict->figure-pdf! avo-bars "avo-bars")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for avo-bars: @(unbox max-error-margin)})
      (set-box! max-error-margin 0))
    (void))

  (when (member 'success-bars to-generate)
    (define success-bars
      (let ()
        (define modes/ordered '("TR"
                                "TR-stack-first"
                                "transient-newest"
                                "transient-oldest"
                                "transient-stack-first"
                                "erasure-stack-first"
                                #;"TR-null" ; decided to remove it from this plot
                                ))
        (generate-figure:success-bars modes/ordered)))
    (pict->figure-pdf! success-bars "success-bars")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for success-bars: @(unbox max-error-margin)})
      (set-box! max-error-margin 0))
    (void))

  (when (member 'blame-vs-exns-venn to-generate)
    (struct utility-comparison (all-3-%

                                top-only-%
                                top-bot-%
                                top-erasure-%

                                bot-only-%
                                bot-erasure-%

                                erasure-only-%)
      #:prefab)
    (define venn-%s
      (simple-memoize
       #:on-disk (and (use-disk-data-cache?)
                      (build-path data-cache "blame-vs-exns-venn-data-prop-estimate.rktd"))
       (λ (top-mode bottom-mode)
         (define (trio-% trio-predicate)
           (define estimate (bt-wise-strata-proportion-estimate (list top-mode
                                                                      bottom-mode
                                                                      "erasure-stack-first")
                                                                trio-predicate))
           (record-error-margin! (variance->margin-of-error (hash-ref estimate 'variance)
                                                            1.96))
           (hash-ref estimate 'proportion-estimate))
         (define all-3-succeed-%
           (trio-% (match-lambda [(list (? satisfies-BT-hypothesis?)
                                        (? satisfies-BT-hypothesis?)
                                        (? satisfies-BT-hypothesis?)) #t]
                                 [_ #f])))

         (define top-only-%
           (trio-% (match-lambda [(list (? satisfies-BT-hypothesis?)
                                        (not (? satisfies-BT-hypothesis?))
                                        (not (? satisfies-BT-hypothesis?))) #t]
                                 [_ #f])))
         (define top-bot-%
           (trio-% (match-lambda [(list (? satisfies-BT-hypothesis?)
                                        (? satisfies-BT-hypothesis?)
                                        (not (? satisfies-BT-hypothesis?))) #t]
                                 [_ #f])))
         (define top-erasure-%
           (trio-% (match-lambda [(list (? satisfies-BT-hypothesis?)
                                        (not (? satisfies-BT-hypothesis?))
                                        (? satisfies-BT-hypothesis?)) #t]
                                 [_ #f])))

         (define bot-only-%
           (trio-% (match-lambda [(list (not (? satisfies-BT-hypothesis?))
                                        (? satisfies-BT-hypothesis?)
                                        (not (? satisfies-BT-hypothesis?))) #t]
                                 [_ #f])))
         (define bot-erasure-%
           (trio-% (match-lambda [(list (not (? satisfies-BT-hypothesis?))
                                        (? satisfies-BT-hypothesis?)
                                        (? satisfies-BT-hypothesis?)) #t]
                                 [_ #f])))

         (define erasure-only-%
           (trio-% (match-lambda [(list (not (? satisfies-BT-hypothesis?))
                                        (not (? satisfies-BT-hypothesis?))
                                        (? satisfies-BT-hypothesis?)) #t]
                                 [_ #f])))

         (utility-comparison all-3-succeed-%
                             top-only-%
                             top-bot-%
                             top-erasure-%
                             bot-only-%
                             bot-erasure-%
                             erasure-only-%))))

    (for ([comparison (in-list '(["TR" "TR-stack-first"]
                                 ["transient-newest" "transient-stack-first"]
                                 ["transient-oldest" "transient-stack-first"]))])
      (match-define (list top bot) comparison)
      (define (->str %) (~r (* % 100) #:precision 1))
      (match-define (utility-comparison (app ->str all-3-%)

                                        (app ->str top-only-%)
                                        (app ->str top-bot-%)
                                        (app ->str top-erasure-%)

                                        (app ->str bot-only-%)
                                        (app ->str bot-erasure-%)

                                        (app ->str erasure-only-%))
        (apply venn-%s comparison))
      (displayln
       @~a{
           Erasure                                @(rename-mode top)

           @erasure-only-%          @top-erasure-%               @top-only-%



           @all-3-%


           @bot-erasure-%                @top-bot-%



           @bot-only-%
           @(rename-mode bot)
           })
      (newline)
      (newline)
      (displayln "--------------------------------------------------")

      (define venn-outfile (build-path outdir @~a{@|top|-@|bot|-venn.svg}))
      (copy-file venn-template venn-outfile #t)
      (define (template-fill! field value)
        (replace-in-file! venn-outfile
                          (~a "&lt;" field "&gt;")
                          value))
      (template-fill! "top-name" (rename-mode top))
      (template-fill! "bot-name" (rename-mode bot))
      (for ([pattern (in-list     '(all
                                    top-only top-bot top-erasure
                                    bot-only bot-erasure
                                    erasure-only))]
            [%       (in-list (list all-3-%
                                    top-only-% top-bot-% top-erasure-%
                                    bot-only-% bot-erasure-%
                                    erasure-only-%))])
        (template-fill! pattern %))
      (system @~a{convert '@venn-outfile' '@(path-replace-extension venn-outfile ".pdf")'})
      (delete-file venn-outfile))

    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for venns: @(unbox max-error-margin)})
      (set-box! max-error-margin 0))
    (void))

  (when (member 'detection-bars to-generate)
    (define detection-bars
      (let ()
        (define modes/ordered '("TR"
                                "TR-stack-first"
                                "transient-newest"
                                "transient-oldest"
                                "transient-stack-first"
                                "erasure-stack-first"
                                #;"TR-null" ; decided to remove it from this plot
                                ))
        (generate-figure:detection-bars modes/ordered)))
    (pict->figure-pdf! detection-bars "detection-bars")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for detection-bars: @(unbox max-error-margin)})
      (set-box! max-error-margin 0))
    (void))

  (when (member 'client-side-success-bars to-generate)
    (define success-bars
      (let ()
        (define modes/ordered '("TR"
                                "TR-stack-first"
                                "transient-newest"
                                "transient-oldest"
                                "transient-stack-first"
                                "erasure-stack-first"
                                #;"TR-null" ; decided to remove it from this plot
                                ))
        (generate-figure:client-side-success-bars modes/ordered)))
    (pict->figure-pdf! success-bars "client-side-success-bars")
    (when (collect-max-error-margin?)
      (displayln @~a{Max error margin for client-side-success-bars: @(unbox max-error-margin)})
      (set-box! max-error-margin 0))
    (void)))
