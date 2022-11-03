#lang at-exp rscript

(require "../util/read-module.rkt"
         "../util/program.rkt"
         "../util/mutant-util.rkt"
         "../util/path-utils.rkt"
         "../runner/mutation-runner.rkt"
         "../mutation-adapter/generate-adapters.rkt"
         "../mutation-adapter/mutation-adapter.rkt"
         (submod "../mutation-adapter/mutation-adapter.rkt" tds)
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         (prefix-in db: "../db/db.rkt")
         "mutation-analysis-summaries.rkt"
         racket/runtime-path)

(define-runtime-path benchmarks-dir "../../gtp-benchmarks/benchmarks")

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

;; program/c mod-name? mutation-index?
(struct mutant* (program mod index) #:transparent)

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
(define function-swap 'f-swap)
(define struct-field-swap 'struct-swap)
(define class-field-swap 'class-swap)
(define ->path '->)
(define container 'container)
(define struct-field 'struct)
(define class-field 'class-f)
(define class-method 'class-m)

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
               (== base-type)))
     #t]
    [else #f]))

;; mutant*? -> category?
(define (categorize-mutant a-mutant*)
  (define-values {original-mod-stx mutated-mod-stx} (mutant->module-syntaxes a-mutant*))
  (match-define (mutated-type name original-type new-type _ _)
    (find-mutated-type original-mod-stx mutated-mod-stx))
  (define diff (sexp->type-diff (sexp-diff original-type new-type)))
  (categorize-mutant-diff diff))


(define/contract (categorize-mutant-diff a-td)
  (any/c . -> . category?) ;; protect against typos!

  (let loop ([td a-td]
             [path empty])
    (define (return v)
      (reverse (cons v path)))
    (match td
      [(? td:base?)
       (return base-type)]
      [(or (td:-> _ (list (cons _ a) (cons _ b)) #f '())
           (td:-> _ '() #f (list (cons _ a) (cons _ b)))
           (td:->* _ (list (cons _ a) (cons _ b)) '()))
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
      [(td:struct #f _ (list (cons _ _) ..2))
       (return struct-field-swap)]
      [(td:struct #f _ (list (cons _ a)))
       (loop a (cons struct-field path))]
      [(or (td:class (list (cons _ _) ..2) '() '())
           (td:class '() (list (cons _ _) ..2) '()))
       (return class-field-swap)]
      [(or (td:class (list (cons _ a)) '() '())
           (td:class '() (list (cons _ a)) '()))
       (loop a (cons class-field path))]
      [(td:class '() '() (list (cons _ a)))
       (loop a (cons class-method path))])))

;; (define (uniq l)
;;   (if (empty? l)
;;       l
;;       (for/fold ([result (take l 1)]
;;                  [last-v (first l)]
;;                  #:result (reverse result))
;;                 ([v (in-list (rest l))]
;;                  #:unless (equal? v last-v))
;;         (values (cons v result)
;;                 v))))

;; (module+ test
;;   (require ruinit)
;;   (test-begin
;;     #:name uniq
;;     (test-equal? (uniq '(1 2 1 1 1 3 1 2 2 2 2 4))
;;                  '(1 2 1 3 1 2 4))
;;     (test-equal? (uniq '())
;;                  '())
;;     (test-equal? (uniq '(1 2 3))
;;                  '(1 2 3))))


(module+ test
  (define db (db:get "../dbs/type-api-mutations/dyn-err-summaries.rktdb"))
  (install-configuration! "../configurables/bltym-configs/TR.rkt")
  (define cs (categorize (summaries-db->mutants db)))
  (require data-frame
           sawzall
           graphite)
  #;(render (add-to (plot (reorder (for/data-frame {mutant category}
                                   ([{m c} (in-hash cs)])
                                   (values m c))
                                 (cons "category" (λ (a b) (> (length a) (length b))))))
                  (histogram #:x "category")
                  (x-axis)
                  (y-axis #:label "count"))
            #:width 3400)
  (plot (add-to (plot (reorder (for/data-frame {mutant category}
                                   ([{m c} (in-hash cs)])
                                   (values m c))
                                 (cons "category" (λ (a b) (> (length a) (length b))))))
                  (histogram #:x "category")
                  (x-axis)
                  (y-axis #:label "count"))
          #:width 3400))
