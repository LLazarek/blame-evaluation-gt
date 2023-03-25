#lang at-exp rscript

(require bex/util/read-module
         bex/util/program
         bex/util/mutant-util
         bex/util/path-utils
         bex/runner/mutation-runner
         bex/mutation-adapter/generate-adapters
         bex/mutation-adapter/mutation-adapter
         (submod bex/mutation-adapter/mutation-adapter tds)
         bex/configurations/config
         bex/configurations/configure-benchmark
         bex/configurables/configurables
         (prefix-in db: bex/db/db)
         bex/mutation-analysis/mutation-analysis-summaries
         racket/runtime-path)

(define-runtime-path benchmarks-dir "../../../gtp-benchmarks/benchmarks")

;; program/c mod-name? mutation-index?
(struct mutant* (program mod index) #:transparent)

(define (categorize mutants)
  (for/hash ([m (in-list mutants)])
    (match-define (mutant benchmark-name mod index) m)
    (values m
            (categorize-mutant (mutant* (benchmark-name->program benchmark-name)
                                        mod
                                        index)))))
(define (mutant-samples-db->mutants db)
  (for*/list ([benchmark-name (db:keys db)]
              [{mod indexes} (in-hash (db:read db benchmark-name))]
              [index (in-list indexes)])
    (mutant benchmark-name mod index)))
(define (summaries-db->mutants db)
  (for*/list ([benchmark-name (db:keys db)]
              [{mod summary} (in-hash (db:read db benchmark-name))]
              [indexes (in-hash-values (summary-valid-indices summary))]
              [index (in-list indexes)])
    (mutant benchmark-name mod index)))

(define (benchmark-name->program name)
  (define path (build-path benchmarks-dir name))
  (define the-benchmark (read-benchmark path))
  (define the-benchmark-configuration
    (configure-benchmark the-benchmark (make-max-bench-config the-benchmark)))
  ((configured:benchmark-configuration->program) the-benchmark-configuration))

;; mutant*? -> (values syntax? syntax?)
(define (mutant->module-syntaxes m)
  (match-define (mutant* a-program mutated-mod-name index) m)
  (match (program->mods a-program)
    [(list _ ... (and mutated-mod (mod (? (path-ends-with mutated-mod-name)) original-stx)) _ ...)
     (define-values {mutated-stx id}
       (mutate-module mutated-mod index #:in a-program))
     (values original-stx
             mutated-stx)]))

(define base-type 'base)
(define complex-type 'composite)
(define function-swap 'fun-swap)
(define struct-field-swap 'struct-swap)
(define class-field-swap 'class-swap)
(define ->path '->)
(define container 'container)
(define struct-field 'struct)
(define class-field 'class-fld)
(define class-method 'class-mtd)

(define typedef 'typedef)

(define (category? l)
  (match l
    [(list (or (== ->path)
               (== container)
               (== struct-field)
               (== class-field)
               (== class-method))
           ...
           (or (== function-swap)
               (== struct-field-swap)
               (== class-field-swap)
               (== complex-type)
               (== base-type)))
     #t]
    [else #f]))

;; mutant*? -> category?
(define (categorize-mutant a-mutant*)
  (define-values {original-mod-stx mutated-mod-stx} (mutant->module-syntaxes a-mutant*))
  (match-define (mutated-type name original-type new-type mutated-struct? mutated-definition?)
    (find-mutated-type original-mod-stx mutated-mod-stx))
  (define diff (sexp->type-diff (sexp-diff original-type new-type)))
  (define base-category (categorize-mutant-diff diff))
  (if mutated-definition?
      (cons typedef base-category)
      base-category))


(define/contract (categorize-mutant-diff a-td)
  (any/c . -> . category?) ;; protect against typos!

  (let loop ([td a-td]
             [path empty])
    (define (return v)
      (reverse (cons v path)))
    (match td
      [(td:base (? list?) 'Any)
       (return complex-type)]
      [(? td:base?)
       (return base-type)]
      [(or (td:-> _ (list (cons _ a) (cons _ b)) #f '())
           (td:-> _ '() #f (list (cons _ a) (cons _ b)))
           (td:->* _ (list (cons _ a) (cons _ b)) '())
           (td:->* _ (list (cons _ a)) (list (cons _ b)))
           (td:->* _ (list) (list (cons _ a) (cons _ b))))
       (return function-swap)]
      [(or (td:-> _ (list (cons _ a)) #f '())
           (td:-> _ '() a '())
           (td:-> _ '() #f (list (cons _ a)))
           (td:->* _ (list (cons _ a)) '())
           (td:->* _ '() (list (cons _ a))))
       (loop a
             (cons ->path path))]
      [(or (td:parametric _ a)
           (td:and a _)
           (td:rec _ a)
           (td:option a)
           (td:instanceof a))
       (loop a path)]
      [(or (td:listof a)
           (td:list* _ (list (cons _ a)) #f)
           (td:list* _ '() a)
           (td:vectorof a)
           (td:boxof a)
           (td:parameterof a)
           (td:setof a)
           (td:pairof a #f)
           (td:pairof #f a)
           (td:hash a #f)
           (td:hash #f a))
       (loop a (cons container path))]
      [(td:struct #f _ _ (list (cons _ _) ..2))
       (return struct-field-swap)]
      [(td:struct #f _ _ (list (cons _ a)))
       (loop a (cons struct-field path))]
      [(or (td:class (list (cons _ _) ..2) '() '())
           (td:class '() (list (cons _ _) ..2) '()))
       (return class-field-swap)]
      [(or (td:class (list (cons _ a)) '() '())
           (td:class '() (list (cons _ a)) '()))
       (loop a (cons class-field path))]
      [(td:class '() '() (list (cons _ a)))
       (loop a (cons class-method path))])))

(main
 #:arguments ([(hash-table ['config config-path]
                           ['summaries-db db-path]
                           ['outpath outpath])
               _]
              #:once-each
              [("-c" "--config")
               'config
               ("Config for obtaining mutation info.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-s" "--summaries-db")
               'summaries-db
               ("Path to the db of mutant summaries for which to plot categories.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-o" "--out")
               'outpath
               ("Path at which to save the plot.")
               #:collect ["path" take-latest #f]
               #:mandatory])

 (require data-frame
          (prefix-in plot: plot)
          complot)

 (define db (db:get db-path))
 (install-configuration! config-path)
 (define cs (categorize (summaries-db->mutants db)))
 (parameterize ([plot:plot-x-tick-label-anchor  'top-right]
                [plot:plot-x-tick-label-angle   40]
                [plot:plot-font-size            14])
   (render (add-to (plot (for/data-frame {mutant category}
                           ([{m c} (in-hash cs)])
                           (values m c)))
                   (histogram #:x "category"
                              #:bar-ordering (λ (a b) (< (length a) (length b)))
                              #:color '(205 247 255))
                   (x-axis)
                   (y-axis #:label "count"
                           #:max 60)
                   #;(title "Interesting mutants per mutation location in type"))
           outpath
           #:width 1700
           #:height 1000)))

(module test racket/base)
