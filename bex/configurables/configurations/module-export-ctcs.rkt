#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "../../util/ctc-utils.rkt")
(provide serialize-config
         deserialize-config
         config-at-max-precision-for?
         increment-config-precision-for

         benchmark->mutatable-modules
         benchmark-configuration->program
         (contract-out
          [make-max-bench-config
           (->i ([bench benchmark/c])
                [result {bench}
                        (config-for-benchmark/c bench)])]
          [configure-benchmark
           (->i ([bench benchmark/c]
                 [config {bench}
                         (config-for-benchmark/c bench)])
                [result benchmark-configuration/c])]))

;; e.g.
;; (hash "main.rkt" (hash 'f 'max 'g 'none 'main 'types))
(define/contract (config-for-benchmark/c b)
  (benchmark/c . -> . (config/c . -> . boolean?))

  (simple-flat-contract-with-explanation
   (λ (config)
     (and (equal? (sort-file-names (map file-name-string-from-path
                                        (benchmark-typed b)))
                  (sort-file-names (hash-keys config)))
          (for/and ([mod-level (in-hash-values config)])
            (member mod-level '(none types max)))))
   @~a{a config for @~v[b]}))

(define (sort-file-names names)
  (sort names string<?))



(require "../../configurations/configure-benchmark.rkt"
         "../../configurations/config.rkt"
         "../../util/path-utils.rkt"
         "../../util/program.rkt"
         "common.rkt"
         syntax/parse)

(define (config-at-max-precision-for? name config)
  (equal? (hash-ref config name) 'max))

(define (increment-config-precision-for name config
                                        #:increment-max-error?
                                        [error-if-already-max? #t])
  (match (hash-ref config name)
    ['none (hash-set config name 'types)]
    ['types (hash-set config name 'max)]
    [else
     #:when error-if-already-max?
     (error 'increment-config-precision-for
            @~a{Given config with value @name already at max level: @config})]
    [else config]))

;; Produces the names of the mutatable modules in `a-benchmark`
(define (benchmark->mutatable-modules a-benchmark)
  (map file-name-string-from-path (benchmark-untyped a-benchmark)))

(define (make-max-bench-config a-benchmark)
  (define mods (benchmark-untyped a-benchmark))
  (for/hash ([path (in-list mods)])
    (values (file-name-string-from-path path)
            'max)))

(define-values {level->digit digit->level}
  (1-to-1-map->converters 'max   #\2
                          'types #\1
                          'none  #\0))
(define serialize-config (make-config-serializer level->digit))
(define deserialize-config (make-config-deserializer digit->level
                                                     benchmark->mutatable-modules))

(module+ test
  (require ruinit
           racket)

  (test-begin
    #:name test:increment-config-precision-for
    (test-equal? (increment-config-precision-for
                  "main.rkt"
                  (hash "main.rkt" 'none))
                 (hash "main.rkt" 'types))
    (test-equal? (increment-config-precision-for
                  "main.rkt"
                  (hash "main.rkt" 'types))
                 (hash "main.rkt" 'max))
    (test-equal? (increment-config-precision-for
                  "main.rkt"
                  (hash "main.rkt" 'max)
                  #:increment-max-error? #f)
                 (hash "main.rkt" 'max))
    (test-exn exn:fail?
              (increment-config-precision-for
               "main.rkt"
               (hash "main.rkt" 'max))))

  (test-begin
    #:name config-at-max-precision-for?
    (not/test (config-at-max-precision-for?
               "main.rkt"
               (hash "main.rkt" 'none)))
    (not/test (config-at-max-precision-for?
               "main.rkt"
               (hash "main.rkt" 'types)))
    (config-at-max-precision-for?
     "main.rkt"
     (hash "main.rkt" 'max)))


  (define-test-env
    [setup! cleanup!]
    #:directories ([test-temp "./test-temp"]
                   [a-benchmark-dir "./test-temp/a-benchmark"]
                   [typed "./test-temp/a-benchmark/typed"]
                   [untyped "./test-temp/a-benchmark/untyped"]
                   [both "./test-temp/a-benchmark/both"]
                   [base "./test-temp/a-benchmark/base"]

                   [not-a-benchmark "./test-temp/not-a-benchmark"])
    #:files ([main/t  (build-path typed "main.rkt")   ""]
             [a/t     (build-path typed "a.rkt")      ""]
             [b/t     (build-path typed "b.rkt")      ""]
             [garbage (build-path typed "garbage")    "some dumb garbage"]
             [main    (build-path untyped "main.rkt") @~a{#lang racket
                                                          (define/configurable-ctc a any/c 5)
                                                          (define/configurable-ctc (f x) any/c 5)}]
             [a       (build-path untyped "a.rkt")    @~a{#lang racket
                                                          (define/configurable-ctc y any/c 5)
                                                          (define/configurable-ctc ((g) x) any/c 5)}]
             [b       (build-path untyped "b.rkt")    "#lang racket b"]
             [adapter (build-path both "adapter.rkt") "#lang racket adapter"]))

  (test-begin
    #:name serialization
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))

    (test-equal? (serialize-config (hash "main.rkt" 'none
                                         "a.rkt" 'none
                                         "b.rkt" 'none))
                 ; 000
                 0)
    (test-equal? (serialize-config (hash "main.rkt" 'max
                                         "a.rkt" 'max
                                         "b.rkt" 'max))
                 222)
    (test-equal? (serialize-config (hash "main.rkt" 'max
                                         "a.rkt" 'types
                                         "b.rkt" 'none))
                 102)
    (test-equal? (serialize-config (hash "main.rkt" 'max
                                         "a.rkt" 'none
                                         "b.rkt" 'none))
                 ; 002
                 2)

    (test-equal? (deserialize-config 2 #:benchmark a-benchmark)
                 (hash "main.rkt" 'max
                       "a.rkt" 'none
                       "b.rkt" 'none))

    (ignore (define-simple-test (test-roundtrip config)
              (define id (serialize-config config))
              (test-equal? (deserialize-config id #:benchmark a-benchmark)
                           config)))
    (test-roundtrip (hash "main.rkt" 'none
                          "a.rkt" 'none
                          "b.rkt" 'none))
    (test-roundtrip (hash "main.rkt" 'max
                       "a.rkt" 'none
                       "b.rkt" 'none))
    (test-roundtrip (hash "main.rkt" 'max
                          "a.rkt" 'types
                          "b.rkt" 'none))))






(define (configure-benchmark bench config)
  (match-define (benchmark typed untyped base both)
    bench)
  (match-define-values {(list main) others}
                       (partition (path-ends-with "main.rkt")
                                  untyped))
  (define adapters (benchmark-both->files both))
  (benchmark-configuration main
                           (append others
                                   adapters)
                           base
                           config))

(define (benchmark-both->files both)
  (match both
    [#f '()]
    [dir (directory-list dir #:build? #t)]))

(define (benchmark-configuration->program c-bench)
  (define plain-program (make-program (benchmark-configuration-main c-bench)
                                      (benchmark-configuration-others c-bench)))
  (insert-program-configuration-selection plain-program
                                          (benchmark-configuration-config c-bench)))

(define (insert-program-configuration-selection a-program program-config)
  (program (insert-mod-configuration-selection (program-main a-program)
                                               (hash-ref program-config
                                                         "main.rkt"))
           (map (λ (m)
                  (match m
                    [(mod (app explode-path/string (list _ ... "both" _))
                          _)
                     m]
                    [(mod path _)
                     (insert-mod-configuration-selection
                      m
                      (hash-ref program-config
                                (file-name-string-from-path path)))]))
                (program-others a-program))))

(define (insert-mod-configuration-selection a-mod level)
  (mod (mod-path a-mod)
       (syntax-parse (mod-stx a-mod)
         [(module name lang (mod-begin . rest))
          #`(module name lang (mod-begin (require (for-syntax racket/base)) (define-syntax ctc-level '#,level) . rest))])))

;; lltodo: will need this when supporting import contracts
(define-syntax-class configurable-provide
  #:commit
  #:attributes [[ids 1] [ctc-maps 1]]
  (pattern ({~datum provide} (configurable-contract-out [ids ctc-maps] ...))))

(define (module->configurable-ids a-mod-path)
  (syntax-parse (mod-stx (make-mod a-mod-path))
    [(module name lang (mod-begin {~or* prov:configurable-provide _} ...))
     (map syntax->datum (filter values (attribute prov.ids)))]))

(module+ test
  (test-begin
    #:name configure-benchmark
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore   (define a-benchmark (read-benchmark a-benchmark-dir))
              (define config-1 (hash "main.rkt" 'none
                                     "a.rkt" 'none
                                     "b.rkt" 'none)))
    (test-match (configure-benchmark a-benchmark
                                     config-1)
                (benchmark-configuration (== main paths=?)
                                         (list-no-order (== a paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== config-1)))
    (ignore (define a-types-config (hash-set config-1
                                             (file-name-string-from-path a)
                                             'types)))
    (test-match (configure-benchmark a-benchmark
                                     a-types-config)
                (benchmark-configuration (== main paths=?)
                                         (list-no-order (== a paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== a-types-config))))

  (test-begin
    #:name benchmark-configuration->program
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-match
     (benchmark-configuration->program
      (configure-benchmark a-benchmark
                           (hash (file-name-string-from-path main)
                                 'max
                                 (file-name-string-from-path a)
                                 'types
                                 (file-name-string-from-path b)
                                 'none)))
     (program (mod (== main paths=?)
                   (app syntax->datum
                        `(module main racket
                           (#%module-begin
                            (define-syntax ctc-level 'max)
                            (define/configurable-ctc a any/c 5)
                            (define/configurable-ctc (f x) any/c 5)))))
              (list-no-order
               (mod (== a paths=?)
                    (app syntax->datum
                         `(module a racket
                            (#%module-begin
                             (define-syntax ctc-level 'types)
                             (define/configurable-ctc y any/c 5)
                             (define/configurable-ctc ((g) x) any/c 5)))))
               (mod (== b paths=?)
                    (app syntax->datum
                         `(module b racket
                            (#%module-begin
                             (define-syntax ctc-level 'none)
                             b))))
               (mod (== adapter paths=?)
                    (app syntax->datum
                         `(module adapter racket
                            (#%module-begin
                             adapter))))))))

  (test-begin
    #:name make-max-bench-config
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (make-max-bench-config a-benchmark)
                 (hash "main.rkt" 'max
                       "a.rkt" 'max
                       "b.rkt" 'max)))

  (test-begin
    #:name benchmark->mutatable-modules
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (list->set (benchmark->mutatable-modules a-benchmark))
                 (set "main.rkt" "a.rkt" "b.rkt"))))
