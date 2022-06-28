#lang at-exp racket

(require "../../util/optional-contracts.rkt"
         "../../util/ctc-utils.rkt")
(provide serialize-config
         deserialize-config
         config-at-max-precision-for?
         increment-config-precision-for

          benchmark->mutatable-modules
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
          (for*/and ([mod-config (in-hash-values config)]
                     [key (in-hash-keys mod-config)])
            (symbol? key))))
   @~a{a config for @~v[b]}))

(define (sort-file-names names)
  (sort names string<?))



(require "../../configurations/configure-benchmark.rkt"
         "../../util/path-utils.rkt"
         "../../util/experiment-exns.rkt"
         "../../util/program.rkt"
         "common.rkt"
         syntax/parse
         syntax/parse/lib/function-header)

(define (config-at-max-precision-for? name config)
  (match-define (list mod id) name)
  (equal? (hash-ref (hash-ref config mod) id) 'max))

(define (increment-config-precision-for name config
                                        #:increment-max-error?
                                        [error-if-already-max? #t])
  (match-define (list mod id) name)
  (hash-update config mod
               (λ (mod-config)
                 (match (hash-ref mod-config id)
                   ['none (hash-set mod-config id 'types)]
                   ['types (hash-set mod-config id 'max)]
                   [else
                    #:when error-if-already-max?
                    (error 'increment-config-precision-for
                           @~a{Given config with value @name already at max level: @config})]
                   [else mod-config]))))

(define-values {level->digit digit->level}
  (1-to-1-map->converters 'max   #\2
                          'types #\1
                          'none  #\0))
(define serialize-config (make-config-serializer (make-config-serializer level->digit
                                                                         symbol<?)))
(define (deserialize-config config-number
                            #:benchmark reference-benchmark)
  (define mods (benchmark->mutatable-modules reference-benchmark))
  (define ordered-mods (reverse (sort mods string<?)))
  (define ordered-mod-chars (reverse (string->list (~a config-number))))
  (let loop ([remaining-mods ordered-mods]
             [current-mod #f]
             [remaining-ids-in-mod empty]
             [remaining-digits ordered-mod-chars]

             [config (hash)]
             [sub-config (hash)])
    (match (list remaining-mods remaining-ids-in-mod remaining-digits)
      [(list '() '() '())
       (if current-mod
           (hash-set config current-mod sub-config)
           config)]
      [(or (list (not '())     '()      '())
           (list      '() (not '())     '())
           (list      '()      '() (not '())))
       (error 'deserialize-config
              @~a{
                  deserialization mismatch between config number @;
                  @config-number @;
                  and benchmark @;
                  @reference-benchmark
                  })]
      [(list (cons mod more-mods) '() _)
       (loop more-mods
             mod
             (module->configurable-ids (find-module-path-by-name reference-benchmark
                                                                 mod))
             remaining-digits

             (hash-set config current-mod sub-config)
             (hash))]
      [(list _ (cons id more-ids) (cons n more-digits))
       (loop remaining-mods
             current-mod
             more-ids
             more-digits

             config
             (hash-set sub-config id (digit->level n)))])))

(define (find-module-path-by-name a-benchmark mod-name)
  (findf (path-ends-with mod-name)
         (benchmark-untyped a-benchmark)))

;; (define (serialize-config config) config)
;; (define (deserialize-config config
;;                             #:reference [reference-config #f]
;;                             #:modules [mods #f])
;;   config)

(module+ test
  (require ruinit
           racket)

  (test-begin
    #:name test:increment-config-precision-for
    (test-equal? (increment-config-precision-for
                  (list "main.rkt" 'baz)
                  (hash "main.rkt" (hash 'baz 'none
                                         'bazzle 'types)))
                 (hash "main.rkt" (hash 'baz 'types
                                        'bazzle 'types)))
    (test-equal? (increment-config-precision-for
                  (list "main.rkt" 'baz)
                  (hash "main.rkt" (hash 'baz 'types
                                         'bazzle 'types)))
                 (hash "main.rkt" (hash 'baz 'max
                                        'bazzle 'types)))
    (test-equal? (increment-config-precision-for
                  (list "main.rkt" 'baz)
                  (hash "main.rkt" (hash 'baz 'max
                                         'bazzle 'types))
                  #:increment-max-error? #f)
                 (hash "main.rkt" (hash 'baz 'max
                                        'bazzle 'types)))
    (test-exn exn:fail?
              (increment-config-precision-for
                  (list "main.rkt" 'baz)
                  (hash "main.rkt" (hash 'baz 'max
                                         'bazzle 'types)))))

  (test-begin
    #:name config-at-max-precision-for?
    (not/test (config-at-max-precision-for?
               (list "main.rkt" 'baz)
               (hash "main.rkt" (hash 'baz 'none
                                      'bazzle 'types))))
    (not/test (config-at-max-precision-for?
               (list "main.rkt" 'baz)
               (hash "main.rkt" (hash 'baz 'types
                                      'bazzle 'types))))
    (config-at-max-precision-for?
     (list "main.rkt" 'baz)
     (hash "main.rkt" (hash 'baz 'max
                            'bazzle 'types))))
  ;; lltodo: tests for serialization/deserialization
  )






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
           (map (λ (mod)
                  (insert-mod-configuration-selection
                   mod
                   (hash-ref program-config
                             (file-name-string-from-path (mod-path mod)))))
                (program-others a-program))))

(define (insert-mod-configuration-selection a-mod mod-config)
  (mod (mod-path a-mod)
       (syntax-parse (mod-stx a-mod)
         [(module name lang (mod-begin . rest))
          #`(module name lang (mod-begin (define-for-syntax ctc-config #,mod-config) . rest))])))

;; Produces the names of the mutatable modules in `a-benchmark`
(define (benchmark->mutatable-modules a-benchmark)
  (map file-name-string-from-path (benchmark-untyped a-benchmark)))

(define (make-max-bench-config a-benchmark)
  (define mods (benchmark-untyped a-benchmark))
  (for/hash ([path (in-list mods)])
    (values (file-name-string-from-path path)
            (for/hash ([id (in-list (module->configurable-ids path))])
              (values id 'max)))))

(define-syntax-class configurable-def
  #:commit
  #:attributes [id ctc-map]
  (pattern ({~datum define/configurable-ctc} id:id ctc-map:expr . _))
  (pattern ({~datum define/configurable-ctc} head:function-header ctc-map:expr . _)
           #:with id #'head.name))
(define (module->configurable-ids a-mod-path)
  (syntax-parse (mod-stx (make-mod a-mod-path))
    [(module name lang (mod-begin {~or* e:configurable-def _} ...))
     (attribute e.id)]))

(module+ test
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
                                                          (define/configurable-ctc x any/c 5)
                                                          (define/configurable-ctc (f x) any/c 5)}]
             [a       (build-path untyped "a.rkt")    @~a{#lang racket
                                                          (define/configurable-ctc y any/c 5)
                                                          (define/configurable-ctc ((g) x) any/c 5)}]
             [b       (build-path untyped "b.rkt")    "#lang racket b"]
             [adapter (build-path both "adapter.rkt") "#lang racket adapter"]))


  (test-begin
    #:name configure-benchmark
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore   (define a-benchmark (read-benchmark a-benchmark-dir))
              (define config-1 (hash (file-name-string-from-path main)
                                   (hash 'x 'none
                                         'f 'none)
                                   (file-name-string-from-path a)
                                   (hash 'y 'none
                                         'g 'none)
                                   (file-name-string-from-path b)
                                   (hash))))
    (test-match (configure-benchmark a-benchmark
                                     config-1)
                (benchmark-configuration (== main paths=?)
                                         (list-no-order (== a paths=?)
                                                        (== b paths=?)
                                                        (== adapter paths=?))
                                         (== base paths=?)
                                         (== config-1)))
    (ignore (define a-types-config (hash-update config-1
                                                (file-name-string-from-path a)
                                                (λ (c) (hash-set c 'x 'types)))))
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
                                 (hash 'x 'max
                                       'f 'none)
                                 (file-name-string-from-path a)
                                 (hash 'y 'types
                                       'g 'types)
                                 (file-name-string-from-path b)
                                 (hash))))
     (program (mod (== main paths=?)
                   (app syntax->datum
                        `(module ,_ racket
                           (#%module-begin
                            (define-for-syntax ctc-config ,(== #hash((x . max) (f . none))))
                            (define/configurable-ctc x any/c 5)
                            (define/configurable-ctc (f x) any/c 5)))))
              (list-no-order
               (mod (== a paths=?)
                    (app syntax->datum
                         `(module ,_ racket
                            (#%module-begin
                             (define-for-syntax ctc-config ,(== #hash((y . types) (g . types))))
                             (define/configurable-ctc y any/c 5)
                             (define/configurable-ctc ((g) x) any/c 5)))))
               (mod (== b paths=?)
                    (app syntax->datum
                         `(module ,_ racket
                            (#%module-begin
                             (define-for-syntax ctc-config ,(== #hash()))))))))))

  (test-begin
    #:name make-max-bench-config
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (make-max-bench-config a-benchmark)
                 (hash "a.rkt" (hash 'x 'max 'f 'max)
                       "b.rkt" (hash 'y 'max 'g 'max))))

  (test-begin
    #:name benchmark->mutatable-modules
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore (define a-benchmark (read-benchmark a-benchmark-dir)))
    (test-equal? (list->set (benchmark->mutatable-modules a-benchmark))
                 (set "main.rkt" "a.rkt" "b.rkt" "adapter.rkt"))))
