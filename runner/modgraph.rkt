#lang at-exp racket

(provide order-by-dependencies)

(require syntax/to-string
         "../util/read-module.rkt")

;; (listof path?) -> (listof path?)
;; Order the given set of modules such that every module in the list
;; only depends on modules earlier in the list than itself.
;;
;; Example: given modules A, B, C, D
;; A depends on B, D
;; B depends on D, C
;; D depends on C
;; Expected result: '(C D B A)
(define (order-by-dependencies modules
                               [read-module read-module]
                               [get-path-string identity])
  (->i ([modules (listof any/c)])
       ([read-module (any/c . -> . syntax?)]
        [get-path-string (any/c . -> . path-string?)])
       [result (listof any/c)]
       #:post (modules result)
       (for/and ([m (in-list result)]
                 [modules-before (in-list (prefixes result))])
         (set-empty?
          (set-subtract (module-dependencies m
                                             modules
                                             read-module
                                             get-path-string)
                        modules-before))))

  (define dependencies-by-module
    (make-immutable-hash
     (map (λ (m)
            (cons m (module-dependencies m
                                         modules
                                         read-module
                                         get-path-string)))
          modules)))
  (define ((all-dependencies-in? modules-seen) m)
    (set-empty? (set-subtract (hash-ref dependencies-by-module m)
                              modules-seen)))
  (let loop ([module-order-so-far empty]
             [modules-remaining modules])
    (cond [(empty? modules-remaining)
           module-order-so-far]
          [else
           (define feasible-modules
             (filter (all-dependencies-in? module-order-so-far)
                     modules-remaining))
           (when (empty? feasible-modules)
             (error 'order-by-dependencies
                    "Module dependency cycle found in ~v"
                    (get-path-string modules)))
           (loop (append module-order-so-far feasible-modules)
                 (set-subtract modules-remaining feasible-modules))])))

(module+ test
  (require ruinit)
  (define-test-env {setup-test-env! cleanup-test-env!}
    #:directories ()
    #:files ([a.rkt
              "a.rkt"
              "#lang racket
(require bar
         \"b.rkt\"
         foo
         \"c.rkt\")
(+ 2 2)
"]
             [b.rkt
              "b.rkt"
              "#lang racket
(require foo
         \"c.rkt\"
         syntax/to-string
         baz)
(+ 2 2)
"]
             [c.rkt
              "c.rkt"
              "#lang racket
(require racket/match)
(+ 2 2)
"]))
  (test-begin
    #:name order-by-dependencies
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    (test-equal? (order-by-dependencies '("a.rkt" "b.rkt" "c.rkt"))
                 '("c.rkt" "b.rkt" "a.rkt"))
    (test-equal? (order-by-dependencies '("../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                          "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                          "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                          "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '("../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"
                   "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                   "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                   "../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"))))

;; Produces a list of the list-prefixes of l, including l itself as
;; the last element
(define (prefixes l)
  (append (for/list ([i (in-range (length l))])
            (take l i))
          (list l)))

(module+ test
  (test-begin
    (test-equal? (prefixes empty)
                 '(()))
    (test-equal? (prefixes '(1))
                 '(() (1)))
    (test-equal? (prefixes '(1 2))
                 '(() (1) (1 2)))
    (test-equal? (prefixes '(1 2 3))
                 '(() (1) (1 2) (1 2 3)))))

(define/contract (module-dependencies m possible-depends
                                      [read-module read-module]
                                      [get-path-string identity])
  (->i ([m any/c]
        [possible-depends (listof any/c)])
       ([read-module (any/c . -> . syntax?)]
        [get-path-string (any/c . -> . string?)])
       [result (possible-depends)
               (and/c (listof any/c)
                      (curryr subset? possible-depends))])

  (define module-stx (read-module m))
  (define module-str (syntax->string module-stx))
  (define (module-mentions? other-module)
    (define-values (__1 basename __2)
      (split-path (get-path-string other-module)))
    (string-contains? module-str (path->string basename)))
  (filter module-mentions? (set-subtract possible-depends
                                         (list m))))

(module+ test
  (test-begin
    #:name module-dependencies
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    ;; Test artifical programs
    (test-equal? (module-dependencies "a.rkt" '("a.rkt" "b.rkt" "c.rkt"))
                 '("c.rkt" "b.rkt"))
    (test-equal? (module-dependencies "b.rkt" '("a.rkt" "b.rkt" "c.rkt"))
                 '("c.rkt"))
    (test-equal? (module-dependencies "c.rkt" '("a.rkt" "b.rkt" "c.rkt"))
                 '())

    ;; Test on forth benchmark
    (test-equal? (module-dependencies "../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                      '("../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '("../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"))
    (test-equal? (module-dependencies "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                      '("../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '("../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
    (test-equal? (module-dependencies "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                      '("../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '("../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"
                   "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"))
    (test-equal? (module-dependencies "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"
                                      '("../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                        "../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '())))
