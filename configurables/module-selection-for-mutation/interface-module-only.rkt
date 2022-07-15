#lang at-exp racket/base

(require racket/contract/base
         racket/format
         "../program-instrumentation/instrument-modules-and-insert-interface-adapter-module.rkt"
         "module-selector.rkt"
         "../../configurations/configure-benchmark.rkt")

(provide (contract-out [select-modules-to-mutate module-selector/c]))


(define (select-modules-to-mutate bench)
  (define mutatable-module-names (benchmark->mutatable-modules bench))
  (unless (member type-interface-file-name mutatable-module-names)
    (raise-user-error 'select-modules-to-mutate:interface-module-only
                      @~a{
                          the interface-module-only configuration is set for @;
                          module-selection-for-mutation, but the current benchmark @;
                          does not have an interface module to mutate, or the currently configured @;
                          configuration module does not consider the interface module mutatable.
                          The mutatable modules reported by `benchmark->mutatable-modules` are:
                          @mutatable-module-names
                          }))
  (list type-interface-file-name))
