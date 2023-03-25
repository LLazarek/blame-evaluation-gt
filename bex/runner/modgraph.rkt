#lang at-exp racket

;; Very hackish topological sort for modules in a program.
;; This is not the right way to do this at all.
;; But for the specific case of the programs in gtp-benchmarks, it works ok.

(provide order-by-dependencies)

(require syntax/to-string
         "../util/read-module.rkt"
         "../util/path-utils.rkt")



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
    (define basename (file-name-string-from-path (get-path-string other-module)))
    (or (string-contains? module-str (~a "\"" basename "\""))
        (string-contains? module-str (~a "/" basename "\""))))
  (filter module-mentions? (set-subtract possible-depends
                                         (list m))))

(module+ test
  (require ruinit)
  (define-test-env {setup-test-env! cleanup-test-env!}
    #:directories ()
    #:files ([a.rkt
              "a.rkt"
              @~a{#lang racket
                  (require bar
                           \"b.rkt\"
                           foo
                           \"c.rkt\")
                  (+ 2 2)
                  }]
             [b.rkt
              "b.rkt"
              @~a{#lang racket
                  (require foo
                           \"c.rkt\"
                           \"another-a.rkt\"
                           syntax/to-string
                           baz)
                  (+ 2 2)
                  }]
             [c.rkt
              "c.rkt"
              @~a{#lang racket
                  (require racket/match)
                  (+ 2 2)
                  }]
             [another-a.rkt
              "another-a.rkt"
              @~a{#lang racket
                  (provide foo)
                  }]

             ;; The essence of the forth benchmark, a good test because it's a
             ;; bit pathological with requires frequently split up, and
             ;; modifiers like only-in
             [forth-main.rkt
              "forth-main.rkt"
              @~a{#lang racket/base
                  (require (only-in "forth-eval.rkt"
                                    forth-eval*
                                    ))
                  (require (only-in racket/file file->lines))
                  }]
             [forth-command.rkt
              "forth-command.rkt"
              @~a{#lang racket/base
                  (provide command% CMD*)
                  (require "../base/untyped.rkt" racket/match racket/class (only-in racket/string string-join) (for-syntax racket/base racket/syntax syntax/parse))
                  (require (only-in "forth-stack.rkt" stack-drop stack-dup stack-init stack-over stack-pop stack-push stack-swap))
                  (define command% (class object% (super-new) (init-field id descr exec)))
                  }]
             [forth-eval.rkt
              "forth-eval.rkt"
              @~a{#lang racket/base
                  (provide forth-eval*)
                  (require "../base/untyped.rkt" racket/match racket/class (only-in racket/port with-input-from-string))
                  (require (only-in "forth-command.rkt" CMD* command%))
                  (require (only-in "forth-stack.rkt" stack-init))
                  (define defn-command #f)
                  }]
             [forth-stack.rkt
              "forth-stack.rkt"
              @~a{#lang racket/base
                  (provide stack-drop stack-dup stack-init stack-over stack-pop stack-push stack-swap)
                  (define (list->stack xs) xs)
                  }]
))

  (test-begin
    #:name module-dependencies
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    ;; Test artifical programs
    (test-equal? (module-dependencies "a.rkt" '("a.rkt" "b.rkt" "c.rkt" "another-a.rkt"))
                 '("c.rkt" "b.rkt"))
    (test-equal? (module-dependencies "b.rkt" '("a.rkt" "b.rkt" "c.rkt" "another-a.rkt"))
                 '("another-a.rkt" "c.rkt"))
    (test-equal? (module-dependencies "c.rkt" '("a.rkt" "b.rkt" "c.rkt" "another-a.rkt"))
                 '())

    (test-equal? (module-dependencies "forth-main.rkt"
                                      '("forth-main.rkt"
                                        "forth-command.rkt"
                                        "forth-eval.rkt"
                                        "forth-stack.rkt"))
                 '("forth-eval.rkt"))
    (test-equal? (module-dependencies "forth-command.rkt"
                                      '("forth-main.rkt"
                                        "forth-command.rkt"
                                        "forth-eval.rkt"
                                        "forth-stack.rkt"))
                 '("forth-stack.rkt"))
    (test-equal? (module-dependencies "forth-eval.rkt"
                                      '("forth-main.rkt"
                                        "forth-command.rkt"
                                        "forth-eval.rkt"
                                        "forth-stack.rkt"))
                 '("forth-stack.rkt"
                   "forth-command.rkt"))
    (test-equal? (module-dependencies "forth-stack.rkt"
                                      '("forth-main.rkt"
                                        "forth-command.rkt"
                                        "forth-eval.rkt"
                                        "forth-stack.rkt"))
                 '())

    (when (directory-exists? "../../../gtp-benchmarks/benchmarks/take5-mixin")
      (test-equal? (module-dependencies "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/deck.rkt"
                                        (map path->string
                                             (directory-list "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped"
                                                             #:build? #t)))
                   '("../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/for-player.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/for-dealer.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/basics.rkt"))))

  (test-begin
    #:name order-by-dependencies
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    (test-equal? (order-by-dependencies '("forth-main.rkt"
                                          "forth-command.rkt"
                                          "forth-eval.rkt"
                                          "forth-stack.rkt"))
                 '("forth-stack.rkt"
                   "forth-command.rkt"
                   "forth-eval.rkt"
                   "forth-main.rkt"))
    ;; Also a nice test, but only for the standard benchmarks!
    #;(test-equal? (order-by-dependencies (map path->string
                                             (directory-list "../../../gtp-benchmarks/benchmarks/take5/untyped"
                                                             #:build? #t)))
                 '("../../../gtp-benchmarks/benchmarks/take5/untyped/basics.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/card.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/stack.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/player.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/card-pool.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/deck.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/dealer.rkt"
                   "../../../gtp-benchmarks/benchmarks/take5/untyped/main.rkt"))
    (when (directory-exists? "../../../gtp-benchmarks/benchmarks/take5-mixin")
      (test-equal? (order-by-dependencies (map path->string
                                               (directory-list "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped"
                                                               #:build? #t)))
                   '("../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/basics.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/card.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/stack.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/player.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/card-pool.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/for-dealer.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/for-player.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/deck.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/dealer.rkt"
                     "../../../gtp-benchmarks/benchmarks/take5-mixin/untyped/main.rkt")))))


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
  #;(->i ([modules (listof any/c)])
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
                    (map get-path-string modules)))
           (loop (append module-order-so-far feasible-modules)
                 (set-subtract modules-remaining feasible-modules))])))

(module+ test
  (test-begin
    #:name order-by-dependencies
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    (test-equal? (order-by-dependencies '("a.rkt" "b.rkt" "c.rkt" "another-a.rkt"))
                 '("c.rkt" "another-a.rkt" "b.rkt" "a.rkt"))
    (test-equal? (order-by-dependencies '("../../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"
                                          "../../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                                          "../../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                                          "../../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"))
                 '("../../../gtp-benchmarks/benchmarks/forth/untyped/stack.rkt"
                   "../../../gtp-benchmarks/benchmarks/forth/untyped/command.rkt"
                   "../../../gtp-benchmarks/benchmarks/forth/untyped/eval.rkt"
                   "../../../gtp-benchmarks/benchmarks/forth/untyped/main.rkt"))))

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

