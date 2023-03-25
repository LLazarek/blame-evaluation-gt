#lang at-exp racket

(provide (contract-out [instrument-program instrument-program/c]))

(require "instrument-program.rkt"
         "instrument-modules-and-insert-interface-adapter-module.rkt"
         "../../mutation-adapter/generate-adapters.rkt")

(define (instrument-program a-program make-instrumented-module)
  (instrument-program/adapter-generator a-program
                                        make-instrumented-module
                                        generate-empty-middle-module))

(define (generate-empty-middle-module original-interface-mod-stx
                                      mutated-interface-mod-stx
                                      mutation-type
                                      interface-mod-name)
  (adapter-ctcs->module-stx empty
                            empty
                            interface-mod-name
                            original-interface-mod-stx))

