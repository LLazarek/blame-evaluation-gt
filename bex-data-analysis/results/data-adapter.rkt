#lang at-exp racket

(provide adapt-mutant-summary)

(require (prefix-in current: bex/runner/mutation-runner-data)
         bex/experiment/blame-trail-data)

;; Commits culminating in 03e1d55f572ce5d605d433a28830d1732dd061e8 changed the
;; definition of run-status to have two more fields: the errortrace and context
;; stacks.
;; This module adapts data from before that commit so that it fits the new format.
;; It's intended to be applied to all blame trails, adapting those that need adaptation
;; and leaving new data alone.

;; The old run-status definition
(struct run-status (mutated-module
                    index
                    mutated-id
                    outcome
                    blamed
                    result-value)
  #:prefab)

(define (adapt-mutant-summary summary)
  (struct-copy mutant-summary
               summary
               [run-status (adapt-run-status (mutant-summary-run-status summary))]))

(define (adapt-run-status maybe-old-rs)
  (match maybe-old-rs
    [(run-status mutated-module
                 index
                 mutated-id
                 outcome
                 blamed
                 result-value)
     (current:run-status mutated-module
                      index
                      mutated-id
                      outcome
                      blamed
                      empty
                      empty
                      result-value)]
    [(? current:run-status? rs) rs]))
