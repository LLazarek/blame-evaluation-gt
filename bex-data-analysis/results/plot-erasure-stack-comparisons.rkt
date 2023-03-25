#lang at-exp rscript

(require "read-data.rkt"
         "figures.rkt"
         bex/experiment/blame-trail-data
         bex/runner/mutation-runner-data)

(define (erasure-context-bt->filtered-version a-bt)
  (match a-bt
    [(blame-trail mutant-id
                  trail-id
                  mode-name
                  summaries)
     (define filtered-summaries (bt-summaries->filtered-version summaries))
     (blame-trail mutant-id
                  trail-id
                  mode-name
                  filtered-summaries)]))

(define (bt-summaries->filtered-version summaries)
  (define (mutant-summary->successor summary)
    (match summary
      [(mutant-summary id
                       (and status
                            (struct* run-status ([mutated-module mutated-mod]
                                                 [context-stack (? list? ctx)]
                                                 [outcome (or 'blamed 'runtime-error)])))
                       config)
       (define first-untyped-mod-in-ctx
         (for/or ([stack-frame (in-list ctx)])
           (and (hash-has-key? config stack-frame)
                (equal? (hash-ref config stack-frame) 'none)
                stack-frame)))
       (define would-probably-be-type-error?
         (equal? first-untyped-mod-in-ctx mutated-mod))
       (cond [would-probably-be-type-error?
              (mutant-summary id
                              (struct-copy run-status
                                           status
                                           [outcome 'type-error]
                                           [blamed (list mutated-mod)]
                                           [errortrace-stack #f]
                                           [context-stack #f])
                              (hash-set config first-untyped-mod-in-ctx 'types))]
             [first-untyped-mod-in-ctx
              (mutant-summary id status (hash-set config first-untyped-mod-in-ctx 'types))]
             [else #f])]
      [else #f]))

  (define start-summary (last summaries))
  (let next-summary ([filtered-version (list start-summary)])
    (define predecessor (first filtered-version))
    (define maybe-successor (mutant-summary->successor predecessor))
    (if maybe-successor
        (next-summary (cons maybe-successor filtered-version))
        filtered-version)))

(define (erasure-context-bts->filtered-versions bts-by-mutator)
  (for/hash ([{mutator bts} (in-hash bts-by-mutator)])
    (values mutator
            (map erasure-context-bt->filtered-version bts))))

(module+ main
  ;;; Don't need any of this now that we have the erasure ctx filtered data
  ;; (use-disk-data-cache? #f)
  ;; (define erasure-ctx-bts (get-bts-by-mutator-for-mode "erasure-stack-first--context"))
  ;; (define erasure-ctx-filtered-bts (erasure-context-bts->filtered-versions erasure-ctx-bts))
  ;; (get-bts-by-mutator-for-mode-backdoor
  ;;  (match-lambda ["erasure-stack-first--context-filtered"
  ;;                 erasure-ctx-filtered-bts]
  ;;                [else #f]))

  (generate-figure:success-bars '("erasure-stack-first--errortrace"
                                  "erasure-stack-first--errortrace-filtered"
                                  "erasure-stack-first--context"
                                  "erasure-stack-first--context-filtered"))
  (generate-figure:bt-lengths-table '("erasure-stack-first--errortrace"
                                      "erasure-stack-first--errortrace-filtered"
                                      "erasure-stack-first--context"
                                      "erasure-stack-first--context-filtered"))
  (generate-figure:avo-bars '("erasure-stack-first--errortrace"
                              "erasure-stack-first--errortrace-filtered"
                              "erasure-stack-first--context"
                              "erasure-stack-first--context-filtered")
                            '("erasure-stack-first--errortrace"
                              "erasure-stack-first--errortrace-filtered"
                              "erasure-stack-first--context"
                              "erasure-stack-first--context-filtered")))
