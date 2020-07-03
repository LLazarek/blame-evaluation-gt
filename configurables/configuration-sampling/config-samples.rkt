#lang at-exp racket/base

(require "../../util/optional-contracts.rkt")
(provide (contract-out [config-samples/c contract?]))

(require "../../configurations/config.rkt"
         "../../util/path-utils.rkt"
         (only-in racket/contract/base -> listof)
         racket/math)

(define config-samples/c (config/c natural? module-name? . -> . (listof config/c)))
