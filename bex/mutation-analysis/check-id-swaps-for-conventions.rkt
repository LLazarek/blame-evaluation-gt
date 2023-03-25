#lang at-exp rscript

[require "../util/mutant-util.rkt"
         "../util/path-utils.rkt"
         "../util/program.rkt"
         "../util/for.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "mutation-analysis-summaries.rkt"
         (prefix-in db: "../db/db.rkt")]

(define-runtime-paths
  [default-benchmarks-dir "../../gtp-benchmarks/benchmarks"])

(define (within-which-convention-swap? mutation)
  (match mutation
    [(list (? symbol? a) (? symbol? b))
     (for/first* ([{name predicate} (in-hash naming-conventions)])
                 (and (predicate (symbol->string a))
                      (predicate (symbol->string b))
                      name))]
    [else #f]))
(define (into/outof-which-convention-swap? mutation)
  (match mutation
    [(list (? symbol? a) (? symbol? b))
     (for/first* ([{name predicate} (in-hash naming-conventions)])
                 (and (xor (predicate (symbol->string a))
                           (predicate (symbol->string b)))
                      name))]
    [else #f]))

(define ((starts-with? prefix) str)
  (string-prefix? str prefix))
(define ((ends-with? suffix) str)
  (string-suffix? str suffix))

;; (hash/c symbol? (string? . -> . boolean?))
(define naming-conventions
  (hash 'parameters (starts-with? "current-")
        'classes (ends-with? "%")
        'interfaces (ends-with? "<%>")
        'predicates (ends-with? "?")
        'unit-signatures (ends-with? "^")
        'units (ends-with? "@")))

(main
 #:arguments {[(hash-table ['db-path db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['config-path (app install-configuration! _)])
               args]
              #:once-each
              [("-d" "--db")
               'db-path
               "DB containing mutants to check."
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-c" "--config")
               'config-path
               "Config specifying mutation engine to use."
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-b" "--benchmarks-dir")
               'benchmarks-dir
               ("Directory containing benchmarks."
                @~a{Default: @default-benchmarks-dir})
               #:collect {"path" take-latest default-benchmarks-dir}]}
 #:check [(db:path-to-db? db-path)
          @~a{@db-path does not look like a db.}]
 #:check [(path-to-existant-directory? benchmarks-dir)
          @~a{Can't find directory @benchmarks-dir}]

 (define db (db:get db-path))
 (for* ([bench-name          (in-list (db:keys db))]
        [bench               (in-value (read-benchmark (build-path benchmarks-dir bench-name)))]
        [max-config          (in-value (make-max-bench-config bench))]
        [configured-bench    (in-value (configure-benchmark bench max-config))]
        [bench-program       (in-value (benchmark-configuration->program configured-bench))]

        [{mod-name summary}  (in-hash (db:read db bench-name))]
        [mod                 (in-value (pick-file-by-name (program->mods bench-program)
                                                          mod-name
                                                          #:key mod-path))]
        [id-swap-mutator     (in-list '("top-level-id-swap" "imported-id-swap"))]
        [index               (in-list (hash-ref (summary-valid-indices summary)
                                                id-swap-mutator
                                                empty))])
   (define mutation (third (extract-mutation mod index bench-program)))
   (define maybe-convention-swap (into/outof-which-convention-swap? mutation))
   (when maybe-convention-swap
     (displayln @~a{@bench-name @mod-name @index : @maybe-convention-swap @mutation}))))
