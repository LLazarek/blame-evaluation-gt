#lang at-exp racket

(provide run-with-mutated-module
         make-mutated-program-runner
         mutate-module
         mutation-index-exception?
         [struct-out run-status]
         run-status/c
         index-exceeded?)

(require racket/runtime-path
         syntax/parse
         syntax/strip-context
         syntax/location
         "../mutate-benchmark/mutate-benchmark.rkt"
         "../mutate/mutated.rkt"
         "../mutate/mutate-program.rkt"
         "../mutate/top-level-selectors.rkt"
         "sandbox-runner.rkt"
         "program.rkt"
         "instrumented-runner.rkt"
         "../util/path-utils.rkt"
         "../util/ctc-utils.rkt")

(define-logger mutant-runner)

(define (mutate-top-level-selector stx)
  (syntax-parse stx
    #:datum-literals [define :]
    [({~and define def} id/sig
                        {~optional {~seq : type}}
                        body ...)
     (define body-stxs (syntax-e (syntax/loc stx (body ...))))
     (define (reconstruct-definition body-stxs/mutated)
       (quasisyntax/loc stx
         (def id/sig {~? {~@ : type}} #,@body-stxs/mutated)))
     (values body-stxs
             (leftmost-identifier-in #'id/sig)
             reconstruct-definition)]
    [_ (values #f #f #f)]))

(define mutate-expression-filter (λ (e)
                                   (syntax-parse e
                                     ;; Ignore expressions with `:` anywhere.
                                     ;; e.g. syntax of for contains : not at the
                                     ;; head of the expr
                                     [(_ ... {~datum :} _ ...) #f]
                                     [else #t])))

;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-stx mutation-index)
  (syntax-parse module-stx
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:do [(define program-stx #'{body ...})
           (match-define (mutated (mutated-program program-stx/mutated
                                                   mutated-id)
                                  _)
             (mutate-benchmark program-stx mutation-index
                               #:top-level-select mutate-top-level-selector
                               #:expression-filter mutate-expression-filter))]
     #:with program/mutated program-stx/mutated
     #:with mutated-mod-stx
     (datum->syntax #'mod-body
                    (syntax-e #'(mod-begin {~@ . program/mutated}))
                    #'mod-body
                    #'mod-body)
     (values (strip-context
              #'(module name lang mutated-mod-stx))
             mutated-id)]))

(define (simple-form-path? path)
  (and (path? path)
       (complete-path? path)
       (for/and ([p (in-list (explode-path path))])
         (path-for-some-system? p))))

(define/contract (make-mutated-program-runner a-program
                                              module-to-mutate
                                              mutation-index
                                              #:modules-base-path [base-path #f]
                                              #:write-modules-to [write-to-dir #f]
                                              #:on-module-exists [on-module-exists 'error]
                                              #:mutator [mutate mutate-module])
  (->i ([a-program program/c]
        [module-to-mutate mod/c]
        [mutation-index natural?])
       (#:modules-base-path [base-path (or/c simple-form-path? #f)]
        #:write-modules-to [write-to-dir (or/c path-string? #f)]
        #:on-module-exists [on-module-exists (or/c 'error 'replace)]
        #:mutator [mutate (syntax? natural? . -> . (values syntax? symbol?))])
       #:pre/desc {base-path write-to-dir}
       (or (not (and write-to-dir
                     (not (unsupplied-arg? write-to-dir))
                     (or (not base-path)
                         (unsupplied-arg? base-path))))
           "must specify #:modules-base-path if #:write-modules-to is specified")
       #:pre/desc {a-program module-to-mutate}
       (or (->bool (member module-to-mutate (list* (program-main a-program)
                                                   (program-others a-program))))
           "module-to-mutate must be part of a-program")

       (values [runner (-> any)]
               [mutated-id symbol?]))

  (define mod-paths/write-to
    (and
     write-to-dir
     (for/fold ([new-paths #hash()])
               ([mod (in-list (list* (program-main a-program)
                                     (program-others a-program)))])
       (define old-path (mod-path mod))
       (define rel-path (find-relative-path base-path old-path))
       (define new-path (simple-form-path (build-path write-to-dir rel-path)))
       (hash-set new-paths old-path new-path))))

  (define (maybe-write-module! a-mod)
    (when write-to-dir
      (define path (mod-path a-mod))
      (define new-path (hash-ref mod-paths/write-to path
                                 (λ _
                                   (error 'write-modules-to
                                          "~v not found in ~v"
                                          path
                                          mod-paths/write-to))))
      (log-mutant-runner-debug "writing module configuration for ~a to ~a"
                               (find-relative-path base-path path)
                               new-path)
      (make-parent-directory* new-path)
      (call-with-output-file new-path #:exists on-module-exists
        (λ (out) (pretty-write (syntax->datum (mod-stx a-mod)) out)))))

  ;; ll: Ugly hack to get the mutated id out of the instrumentor
  (define mutated-id-box (box #f))
  (define (instrument-module a-mod)
    (match a-mod
      [(and (== module-to-mutate)
            (mod path stx))
       (define-values (mutated-stx mutated-id)
         (mutate stx mutation-index))
       (maybe-write-module! (mod path mutated-stx))

       ;; ll: see above...
       (set-box! mutated-id-box mutated-id)

       mutated-stx]
      [(mod _ stx)
       (maybe-write-module! a-mod)
       stx]))

  (define (setup-namespace! ns)
    ;; Make racket/contract come from the same namespace so that
    ;; we can inspect contract violations thrown inside eval
    (namespace-attach-module (current-namespace)
                             'racket/contract
                             ns)
    ;; Require errortrace so that we get accurate runtime error location
    ;; information
    ;; lltodo: apparent bug? This fails saying that it can't find errortrace
    ;; at a path where errortrace exists.
    #;((namespace-require 'errortrace ns))
    ;; but this works:
    (parameterize ([current-namespace ns])
      (namespace-require 'errortrace)))

  (define runner
    (make-instrumented-runner a-program
                              instrument-module
                              #:setup-namespace setup-namespace!))
  (define mutated-id (unbox mutated-id-box))

  (values runner mutated-id))






(define index-exceeded-outcome 'index-exceeded)
(define outcomes `(,index-exceeded-outcome
                   blamed
                   runtime-error
                   type-error
                   oom
                   timeout
                   syntax-error
                   completed))

(struct run-status (mutated-module
                    index
                    mutated-id
                    outcome
                    blamed
                    result-value)
  #:prefab)

(define run-status/c
  (struct/dc run-status
             [mutated-module  module-name?]
             [index           natural?]
             [mutated-id      {outcome}
                              (if (equal? outcome index-exceeded-outcome)
                                  #f
                                  symbol?)]
             [outcome         (apply or/c outcomes)]
             [blamed          {outcome}
                              (if (member outcome '(blamed type-error))
                                  (or/c module-name?
                                        ;; This case allows the benchmarks to
                                        ;; blame library code, which happens for
                                        ;; instance in the quadU mutant
                                        ;; "quad-main.rkt" @ 79
                                        library-path?)
                                  any/c)]
             [result-value    any/c]))

;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) index-exceeded-outcome))

(define (type-checker-failure? e)
  (and (exn:fail:syntax? e)
       (regexp-match? "Type Checker:"
                      (exn-message e))))

(define/contract (run-with-mutated-module a-program
                                          module-to-mutate
                                          mutation-index
                                          #:suppress-output? [suppress-output? #t]
                                          #:timeout/s [timeout/s (* 3 60)]
                                          #:memory/gb [memory/gb 3]
                                          #:modules-base-path [base-path #f]
                                          #:write-modules-to [write-to-dir #f]
                                          #:on-module-exists [on-module-exists 'error]
                                          #:mutator [mutate mutate-module])
  (->i ([a-program program/c]
        [module-to-mutate mod/c]
        [mutation-index natural?])
       (#:suppress-output? [suppress-output? boolean?]
        #:timeout/s [timeout/s number?]
        #:memory/gb [memory/gb number?]
        #:modules-base-path [base-path (or/c simple-form-path? #f)]
        #:write-modules-to [write-to-dir (or/c path-string? #f)]
        #:on-module-exists [on-module-exists (or/c 'error 'replace)]
        #:mutator [mutate (syntax? natural? . -> . (values syntax? symbol?))])

       #:pre/desc (base-path write-to-dir)
       (or (not (and write-to-dir (not (unsupplied-arg? write-to-dir))
                     (or (not base-path) (unsupplied-arg? base-path))))
           "must specify #:modules-base-path if #:write-modules-to is specified")

       [result run-status/c])

  (define (make-status status-sym [blamed #f] [mutated-id #f] [result #f])
    (run-status (mod->name module-to-mutate)
                mutation-index
                mutated-id
                status-sym
                blamed
                result))
  (with-handlers ([mutation-index-exception?
                   (λ (e) (make-status 'index-exceeded))])
    (define-values {run mutated-id}
      (make-mutated-program-runner a-program
                                   module-to-mutate
                                   mutation-index
                                   #:modules-base-path base-path
                                   #:write-modules-to write-to-dir
                                   #:on-module-exists on-module-exists
                                   #:mutator mutate))
    (define ((make-status* status-sym) [blamed #f] [result #f])
      (make-status status-sym
                   blamed
                   mutated-id))
    (define (extract-blamed e)
      (define blame-obj (exn:fail:contract:blame-object e))
      (define blamed
        (match (blame-positive blame-obj)
          [`(function ,id) id]
          [`(definition ,id) id]
          [(or 'cast 'typed-world)
           (srcloc-source (blame-source blame-obj))]
          [other other]))
      (define blamed-mod-name
        (match blamed
          ;; NOTE: This depends on a modification to Typed Racket;
          ;; Specifically `require/contract` must be modified to change
          ;; the positive party, by extending the list with
          ;; ```from #,(syntax->datum #'lib)```
          [`(interface for ,_ from ,mod-name)
           mod-name]
          [(? path-string?)
           (file-name-string-from-path blamed)]
          [else
           (error 'extract-blamed
                  @~a{
                      Unexpected blamed party shape: @~v[blamed]
                      Mutant info:
                      @(format-mutant-info a-program
                                           module-to-mutate
                                           mutation-index)

                      The blame error message is:
                      @(exn-message e)
                      })]))
      ((make-status* 'blamed) blamed-mod-name))
    (define (extract-type-error-source e)
      (define failing-stxs (exn:fail:syntax-exprs e))
      (define module-name
        (match failing-stxs
          [(list* (app syntax-source-file-name #f)
                  ...
                  (app syntax-source-file-name (? path? file-name-path))
                  _)
           (path->string file-name-path)]
          [else
           (error 'extract-type-error-source
                  @~a{
                      Couldn't find mod name in type error stxs.
                      Program:
                      @(format-mutant-info a-program
                                           module-to-mutate
                                           mutation-index)

                      Stxs:
                      @~v[failing-stxs]

                      The type error message is:
                      @(exn-message e)
                      })]))
      ((make-status* 'type-error) module-name (exn-message e)))
    (define (extract-runtime-error-location e)
      (define ctx (continuation-mark-set->context
                   (exn-continuation-marks e)))
      (define mod-with-error
        (for*/first ([ctx-item (in-list ctx)]
                     [ctx-mod-path
                      (in-value
                       (match ctx-item
                         [(cons _ (srcloc (? path? path) _ _ _ _))
                          (path->string path)]
                         [(cons _ (srcloc (? string? path-str) _ _ _ _))
                          (string-trim path-str "\"")]
                         [(cons (? symbol?
                                   (app symbol->string
                                        (regexp @regexp{^body of "(.+)"$}
                                                (list _ path-str))))
                                _)
                          path-str]
                         [(cons (? symbol?
                                   (app symbol->string
                                        (regexp @regexp{^body of '(.+)$}
                                                (list _ mod-name-sym))))
                                _)
                          (~a mod-name-sym ".rkt")]
                         [else #f]))]
                     #:when ctx-mod-path
                     [mod (in-list (list* (program-main a-program)
                                          (program-others a-program)))]
                     #:when (equal? (file-name-string-from-path (mod-path mod))
                                    (file-name-string-from-path ctx-mod-path)))
          (file-name-string-from-path ctx-mod-path)))
      (define mod-with-error-name
        (match mod-with-error
          [(? string? name) name]
          [#f
           (eprintf @~a{
                        Couldn't find a mod name in ctx from runtime error, @;
                        possibly because the error happened while @;
                        instantiating a module.
                        Program:
                        @(format-mutant-info a-program
                                             module-to-mutate
                                             mutation-index)

                        Ctx:
                        @pretty-format[ctx]

                        The runtime error message is:
                        @(exn-message e)

                        Assuming that errortrace failed us and moving on.
                        })
           #f]))
      ((make-status* 'runtime-error) mod-with-error-name))
    (define run/handled
      (λ _
        (with-handlers
          ([exn:fail:contract:blame? extract-blamed]
           [type-checker-failure? extract-type-error-source]
           [exn:fail:out-of-memory? (λ _ ((make-status* 'oom)))]
           [exn:fail:syntax? (λ _ ((make-status* 'syntax-error)))]
           [exn? extract-runtime-error-location])
          (run)
          ((make-status* 'completed)))))
    (run-with-limits run/handled
                     #:timeout/s timeout/s
                     #:timeout-result (make-status* 'timeout)
                     #:memory/gb memory/gb
                     #:oom-result (make-status* 'oom)
                     #:suppress-output? suppress-output?)))

(define (format-mutant-info a-program module-to-mutate mutation-index)
  (define program-paths
    (match a-program
      [(program (mod main-path _)
                (list (mod other-paths _) ...))
       (list* main-path other-paths)]))
  (define mutant-info
    (list* 'mutant-info
           (path->string (mod-path module-to-mutate))
           mutation-index
           (map path->string program-paths)))
  (pretty-format mutant-info #:mode 'write))



(module+ test
  (require ruinit)
  (define-test (test-stx=? a b)
    (test-equal? (syntax->datum a) (syntax->datum b)))
  (define-test (test-mutate-top-level-selector stx
                                               body-stxs/expected
                                               id/expected
                                               test-reconstruct)
    (define-values {body-stxs id reconstruct}
      (mutate-top-level-selector stx))
    (and/test/message
     [(for/and/test ([part (in-list body-stxs)]
                     [part/expected (in-list body-stxs/expected)])
                    (test-stx=? part part/expected))
      @~a{Body stxs are different:}]
     [(test-equal? id id/expected)
      @~a{Mutated ids are different:}]
     [(test-reconstruct reconstruct)
      @~a{Reconstruction test failed:}]))
  (test-begin
    #:name mutate-top-level-selector
    (test-mutate-top-level-selector #'(define x 5)
                                    (list #'5)
                                    'x
                                    (λ (reconstruct)
                                      (test-stx=? (reconstruct (list #'42))
                                                  #'(define x 42)))))
  (define-test (test/no-error run-thunk test-thunk)
    (with-handlers ([exn:fail? (λ (e) (fail (exn-message e)))])
      (test-thunk (run-thunk))))
  (test-begin
    #:name run-with-mutated-module/mutations
    (ignore
     (define a (mod "a.rkt"
                    #'(module a racket
                        (#%module-begin
                         (require "b.rkt")

                         (define a 1)
                         (define b 1)

                         (define (foo x y)
                           (if #t
                               (- y x)
                               (+ (sleep 1)
                                  ;; Displays useful for debugging memory
                                  ;; limits.
                                  (displayln
                                   (exact->inexact (/ (current-memory-use)
                                                      (expt 10 9))))
                                  (make-bytes (expt 10 9)) ; 1gb
                                  (displayln
                                   (exact->inexact (/ (current-memory-use)
                                                      (expt 10 9))))
                                  (foo x y))))

                         (displayln (list 'a a))
                         (displayln (list 'b b))
                         (foo d c)))))
     (define b (mod "b.rkt"
                    #'(module b racket
                        (#%module-begin
                         (provide c d)

                         (require "c.rkt")

                         (displayln "B")

                         (define c
                           5)
                         (define d
                           (if #t 1 "one"))
                         (displayln (list 'c c))
                         (displayln (list 'd d))))))
     (define c (mod "c.rkt"
                    #'(module c typed/racket
                        (#%module-begin
                         (define x : Number 5)
                         (void)))))
     (define p (program a (list b c))))
    (test/no-error
     (λ _ (with-output-to-string
            (λ _ (run-with-mutated-module p
                                          a
                                          0
                                          #:suppress-output? #f))))
     (λ (output) (test-equal? output
                              "B
(c 5)
(d 1)
(a -1)
(b 1)
4
")))

    (test/no-error
     (λ _ (with-output-to-string
            (λ _ (run-with-mutated-module p
                                          b
                                          0
                                          #:suppress-output? #f))))
     (λ (output) (test-equal? output
                              "B
(c -5)
(d 1)
(a 1)
(b 1)
-6
")))

    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   10
                                   #:timeout/s 5
                                   #:memory/gb 3))
     (λ (r) (test-match r (struct* run-status ([outcome 'timeout])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   10
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'oom]))))))

  (test-begin
    #:name run-with-mutated-module/ctc-violations
    (ignore
     (define main (mod "main.rkt"
                       #'(module main racket
                           (#%module-begin
                            (require "second.rkt")
                            (define (foo x) (if (positive? x) (foo (- x)) (* x x 3)))
                            (define (main) (bar (foo 2) "yes"))
                            (main)))))
     (define second (mod "second.rkt"
                         #'(module second typed/racket
                             (#%module-begin
                              (: bar (-> Integer String Nonpositive-Integer))
                              (provide bar)
                              (define (bar x s) (if (positive? x) (- x) x))))))
     (define p (program main (list second))))
    (test-exn exn:fail:contract?
              ((make-instrumented-runner
                (program (mod "main.rkt"
                              #'(module main racket
                                  (#%module-begin
                                   (require "second.rkt")
                                   (define (foo x) (if (positive? x) (foo (- x)) (/ x x 3)))
                                   (define (main) (bar (foo 2) "yes"))
                                   (main))))
                         (list second))
                (λ (a-mod) (mod-stx a-mod))
                #:setup-namespace
                (λ (ns)
                  (namespace-attach-module (current-namespace)
                                           'racket/contract
                                           ns)))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   main
                                   3
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'blamed]
                                               [blamed "main.rkt"]))))))

  (define (replace-stx-location stx new-file-name)
    (define-values {read-port write-port} (make-pipe))
    (pretty-write (syntax->datum stx) write-port)
    (read-module/port read-port #:source new-file-name))
  (define (mod/loc path stx)
    (mod path
         (replace-stx-location stx path)))

  (test-begin
    #:name run-with-mutated-module/type-error
    (ignore
     (define a (mod/loc "a.rkt"
                        #'(module a racket
                            (#%module-begin
                             (require "b.rkt")

                             (define a 1)
                             (define b 1)

                             (define (foo x y)
                               (if #t
                                   (- y x)
                                   (+ (sleep 1)
                                      (for/vector #:length 4294967296 () #f)
                                      (foo x y))))

                             ;; to have errors
                             (define x (list-ref '(0 1 2 3 4 5) 5))

                             (displayln (list 'a a))
                             (displayln (list 'b b))
                             (foo d c)))))
     (define b (mod/loc "b.rkt"
                        #'(module b typed/racket
                            (#%module-begin
                             (provide c d)
                             (require "c.rkt")

                             (define d : (List False One) (list #f 1))

                             (: c Boolean)
                             (define c (first d))

                             (displayln (list 'c c))
                             (displayln (list 'd d))))))
     (define c (mod/loc "c.rkt"
                        #'(module c typed/racket
                            (#%module-begin
                             (: x One)
                             (define x 1)
                             (displayln (list 'x x))))))
     (define p (program a (list b c))))

    (test/no-error
     (λ _ (run-with-mutated-module p
                                   b
                                   0
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'type-error]
                                               [blamed "b.rkt"])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   c
                                   0
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'type-error]
                                               [blamed "c.rkt"])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   11 ;; runtime error -> blame on a.rkt
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "a.rkt"]))))))


  (define-test-env {setup-test-env! cleanup-test-env!}
    #:directories ([test-mods-dir "./test-mods"])
    #:files ([a.rkt (build-path test-mods-dir "a.rkt")
                    @~a{
                        #lang racket
                        (require "b.rkt")
                        (define x (foo (+ 1) "2" 0))
                        (x)
                        (define (main)
                          (+ "a" "b"))
                        (main)
                        }]
             [b.rkt (build-path test-mods-dir "b.rkt")
                    @~a{
                        #lang racket
                        (provide foo)
                        (require "c.rkt")
                        (define (foo x y z)
                          (when (and (number? x)
                                     (negative? x))
                            (list-ref '(1 2 3) x))
                          (bar x y z))
                        }]
             [c.rkt (build-path test-mods-dir "c.rkt")
                    @~a{
                        #lang racket
                        (provide bar)
                        (define (bar x y z)
                          (+ (- x) z))
                        }]))
  (require "../util/read-module.rkt")
  (test-begin
    #:name mod/loc-vs-file
    #:before (setup-test-env!)
    #:after (cleanup-test-env!)
    (ignore
     (define a/ml (mod/loc a.rkt
                           #'(module a racket
                               (#%module-begin
                                (require "b.rkt")
                                (define x (foo (+ 1) "2" 0))
                                (x)
                                (define (main)
                                  (+ "a" "b"))
                                (main)))))
     (define b/ml (mod/loc b.rkt
                           #'(module a racket
                               (#%module-begin
                                (provide foo)
                                (require "c.rkt")
                                (define (foo x y z)
                                  (when (and (number? x)
                                             (negative? x))
                                    (list-ref '(1 2 3) x))
                                  (bar x y z))))))
     (define c/ml (mod/loc c.rkt
                           #'(module a racket
                               (#%module-begin
                                (provide bar)
                                (define (bar x y z)
                                  (+ (- x) z))))))
     (define a/f (mod a.rkt
                      (read-module a.rkt)))
     (define b/f (mod b.rkt
                      (read-module b.rkt)))
     (define c/f (mod c.rkt
                      (read-module c.rkt)))
     (define p/ml (program a/ml (list b/ml c/ml)))
     (define p/f  (program a/f  (list b/f  c/f))))
    (test/no-error
     (λ _ (run-with-mutated-module p/ml
                                   a/ml
                                   3
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-equal?
             r
             (run-with-mutated-module p/f
                                      a/f
                                      3
                                      #:timeout/s 60
                                      #:memory/gb 1))))
    (test/no-error
     (λ _ (run-with-mutated-module p/ml
                                   a/ml
                                   3
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "a.rkt"]))))))
  (test-begin
    #:name run-with-mutated-module/runtime-error-locations
    ;; #:before (setup-test-env!)
    ;; #:after (cleanup-test-env!)
    (ignore
     (define a (mod/loc (simple-form-path "./test-mods/a.rkt")
                        #'(module a racket
                            (#%module-begin
                             (require "b.rkt")
                             (define x (foo (+ 1) "2" 0))
                             (x)
                             (define (main)
                               (+ "a" "b"))
                             (main)))))
     (define b (mod/loc (simple-form-path "./test-mods/b.rkt")
                        #'(module a racket
                            (#%module-begin
                             (provide foo)
                             (require "c.rkt")
                             (define (foo x y z)
                               (when (and (number? x)
                                          (negative? x))
                                 (list-ref '(1 2 3) x))
                               (bar x y z))))))
     (define c (mod/loc (simple-form-path "./test-mods/c.rkt")
                        #'(module a racket
                            (#%module-begin
                             (provide bar)
                             (define (bar x y z)
                               (+ (- x) z))))))
     (define p (program a (list b c))))

    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   0 ; swap x y args of foo -> crash in bar
                                   #:timeout/s 60
                                   #:memory/gb 1
                                   #:suppress-output? #f))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "c.rkt"])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   1 ; (+ 1) to (- 1) -> crash in foo
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "b.rkt"])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   4 ; (+ 1) to (+ 0) -> crash in top level of a.rkt
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "a.rkt"])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   8 ; 0 to 1 -> crash in main
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed "a.rkt"]))))))

  (test-begin
    #:name mutate-expression-filter
    (mutate-expression-filter #'(+ 2 2))
    (not (mutate-expression-filter #'(: foo Value)))
    (not (mutate-expression-filter #'[element-in-for : Type (in-list l)]))
    ;; but do descend into the rest of the for
    (mutate-expression-filter #'(for ([x : Type (in-list l)]) x))
    ;; or the clauses
    (mutate-expression-filter #'([x : Type (in-list l)]))))


;; for debugging
(module+ debug
  (provide diff-mutation
           mutant-count
           format-raw-config-for-runner)

  (require "../util/read-module.rkt"
           ruinit/diff/diff)
  (define (diff-mutation module-to-mutate mutation-index)
    (define orig-module-stx
      (match module-to-mutate
        [(mod _ stx) stx]
        [(? path-string? path) (read-module path)]
        [other (raise-argument-error 'diff-mutation
                                     "either a mod/c or a path-string?"
                                     other)]))
    (define-values (mutated-program-stx mutated-id)
      (mutate-module orig-module-stx mutation-index))
    (printf "--------------------\nMutated: ~a\n" mutated-id)
    (displayln (dumb-diff-lines/string
                (pretty-format (syntax->datum orig-module-stx))
                (pretty-format (syntax->datum mutated-program-stx)))))
  (define (mutant-count module-to-mutate)
    (define module-stx (read-module module-to-mutate))
    (let next-mutant ([index 0])
      (define index-exceeded?
        (with-handlers ([mutation-index-exception? (λ _ #t)])
          (mutate-module module-stx index)
          #f))
      (if index-exceeded?
          index
          (next-mutant (add1 index)))))
  (define (maybe-make-path x)
    (if (string? x)
        (simple-form-path x)
        x))
  (define (format-raw-config-for-runner config)
    (for/hash ([(mod mod-ids) (in-hash config)])
      (values (maybe-make-path mod)
              (for/hash ([(id level) (in-hash mod-ids)])
                (values (maybe-make-path id)
                        level))))))
