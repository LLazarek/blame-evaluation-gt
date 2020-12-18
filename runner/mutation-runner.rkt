#lang at-exp racket

(provide run-with-mutated-module
         make-mutated-program-runner
         mutate-module
         mutation-index-exception?
         [struct-out run-status]
         run-status/c
         index-exceeded?
         current-mutated-program-exn-recordor)

(require racket/runtime-path
         syntax/parse
         syntax/strip-context
         "mutation-runner-data.rkt"
         "../configurables/configurables.rkt"
         "../mutate/mutated.rkt"
         "../mutate/mutate-program.rkt"
         "../util/path-utils.rkt"
         "../util/ctc-utils.rkt"
         "../util/read-module.rkt"
         "../configurations/config.rkt"
         "sandbox-runner.rkt"
         "../util/program.rkt"
         "../util/experiment-exns.rkt"
         "instrumented-runner.rkt"
         "error-extractors/extract-type-error-source.rkt"
         (prefix-in racket-contracts:
                    "../configurables/blame-following/natural-blame.rkt"))

(define-logger mutation-runner)

(define current-mutated-program-exn-recordor (make-parameter #f))

(define (mutate-module the-module mutation-index #:in the-program)
  (define mutate-benchmark (configured:mutate-benchmark))
  (log-mutation-runner-info
   @~a{Mutating module @the-module @"@" @mutation-index using configured mutator @mutate-benchmark})
  (syntax-parse (mod-stx the-module)
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:do [(define program-stx #'{body ...})
           (match-define (mutated (mutated-program program-stx/mutated
                                                   mutated-id)
                                  _)
             (mutate-benchmark program-stx
                               mutation-index
                               #:program the-program))]
     #:with program/mutated program-stx/mutated
     #:with mutated-mod-stx
     (datum->syntax #'mod-body
                    (syntax-e #'(mod-begin {~@ . program/mutated}))
                    #'mod-body
                    #'mod-body)
     (values (strip-context
              #'(module name lang mutated-mod-stx))
             mutated-id)]))

;; Returns the predicate recognizing transient blame exns if the module is
;; available, otherwise #f
(define (try-dynamic-require-transient-blame-exn-predicate!)
  (with-handlers ([exn:fail:filesystem:missing-module?
                   ;; if we can't load the transient exn module, it's not
                   ;; installed and we definitely can't get transient blames
                   (const #f)])
    (dynamic-require 'typed-racket/utils/transient-contract-struct
                     'exn:fail:contract:blame:transient?)))

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
        #:mutator [mutate (mod/c natural? #:in program/c . -> . (values syntax? symbol?))])
       #:pre/desc {base-path write-to-dir}
       (or (not (and write-to-dir
                     (not (unsupplied-arg? write-to-dir))
                     (or (not base-path)
                         (unsupplied-arg? base-path))))
           "must specify #:modules-base-path if #:write-modules-to is specified")
       #:pre/desc {a-program module-to-mutate}
       (or (->bool (member module-to-mutate (program->mods a-program)))
           "module-to-mutate must be part of a-program")

       (values [runner (-> any)]
               [mutated-id symbol?]))

  (define mod-paths/write-to
    (and
     write-to-dir
     (for/fold ([new-paths #hash()])
               ([mod (in-list (program->mods a-program))])
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
      (log-mutation-runner-debug "writing module configuration for ~a to ~a"
                               (find-relative-path base-path path)
                               new-path)
      (make-parent-directory* new-path)
      (call-with-output-file new-path #:exists on-module-exists
        (λ (out) (pretty-write (syntax->datum (mod-stx a-mod)) out)))))

  ;; ll: Ugly hack to get the mutated id out of the instrumentor
  (define mutated-id-box (box #f))
  (define (record-mutated-id/maybe-write-modules a-mod)
    (match a-mod
      [(and (== module-to-mutate)
            (mod path stx))
       (define-values (mutated-stx mutated-id)
         (mutate a-mod mutation-index #:in a-program))
       (maybe-write-module! (mod path mutated-stx))

       ;; ll: see above...
       (set-box! mutated-id-box mutated-id)

       (replace-stx-location mutated-stx path)]
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
    ;; information.
    #;(eval '(require (lib "errortrace")) ns)
    (parameterize ([current-namespace ns])
      (namespace-require '(lib "errortrace")))
    ;; Also attach transient blame exn definition module so we can inspect
    ;; those
    (when (try-dynamic-require-transient-blame-exn-predicate!)
      (namespace-attach-module (current-namespace)
                               'typed-racket/utils/transient-contract-struct
                               ns)))

  (define configured-instrumenter (configured:instrument-module))
  (define make-configured-runner (configured:make-benchmark-runner))

  (log-mutation-runner-info
   @~a{Constructing mutated-program-runner with
                    configured instrumenter: @configured-instrumenter
                    make-configured-runner:  @make-configured-runner})

  (define runner
    (make-instrumented-runner
     a-program
     (compose1 record-mutated-id/maybe-write-modules
               configured-instrumenter)
     #:setup-namespace setup-namespace!
     #:run-with (make-configured-runner a-program
                                        (mod->name module-to-mutate)
                                        mutation-index)
     ;; Make relative path references work correctly. This is necessary because
     ;; of unification (see `unify-program.rkt`); without it, relative refs to
     ;; files in e.g. `base` fail because the `unified` directory doesn't exist.
     ;; E.g: for `(file->lines "../base/foobar.txt")` the path would become
     ;;      ".../benchmark/unified/../base/foobar.txt"
     #:before-main (λ (ns)
                     (define program-dir-path
                       (apply build-path-string
                              (append (drop-right (explode-path (mod-path module-to-mutate)) 2)
                                      '("untyped"))))
                     (eval `(current-directory ,program-dir-path) ns))))
  (define mutated-id (unbox mutated-id-box))

  (values runner mutated-id))



(define (type-checker-failure? e)
  (and (exn:fail:syntax? e)
       (regexp-match? "Type Checker:"
                      (exn-message e))))

(define/contract (run-with-mutated-module a-program
                                          module-to-mutate
                                          mutation-index
                                          program-config
                                          #:suppress-output? [suppress-output? #t]
                                          #:timeout/s [timeout/s (* 3 60)]
                                          #:memory/gb [memory/gb 3]
                                          #:modules-base-path [base-path #f]
                                          #:write-modules-to [write-to-dir #f]
                                          #:on-module-exists [on-module-exists 'error]
                                          #:mutator [mutate mutate-module])
  (->i ([a-program program/c]
        [module-to-mutate mod/c]
        [mutation-index natural?]
        [program-config config/c])
       (#:suppress-output? [suppress-output? boolean?]
        #:timeout/s [timeout/s number?]
        #:memory/gb [memory/gb number?]
        #:modules-base-path [base-path (or/c simple-form-path? #f)]
        #:write-modules-to [write-to-dir (or/c path-string? #f)]
        #:on-module-exists [on-module-exists (or/c 'error 'replace)]
        #:mutator [mutate (mod/c natural? #:in program/c . -> . (values syntax? symbol?))])

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
      (log-mutation-runner-debug @~a{Making run-status with outcome @status-sym blaming @blamed})
      (make-status status-sym
                   blamed
                   mutated-id))
    (define format-mutant-info-for-error
      (thunk (format-mutant-info a-program
                                 module-to-mutate
                                 mutation-index)))
    (define make-extract-blamed (configured:make-extract-blamed))
    (define make-extract-runtime-error-location (configured:make-extract-runtime-error-location))
    (log-mutation-runner-info
     @~a{Making configured blamed and runtime-error location extractors with
                make-extract-blamed: @make-extract-blamed
                make-extract-runtime-error-location: make-extract-runtime-error-location})
    (define extract-blamed
      (make-extract-blamed a-program
                           program-config
                           format-mutant-info-for-error))
    (define extract-type-error-source
      (make-extract-type-error-source a-program
                                      program-config
                                      format-mutant-info-for-error))
    (define extract-runtime-error-location
      (make-extract-runtime-error-location a-program
                                           program-config
                                           format-mutant-info-for-error))
    (define exn:fail:contract:blame:transient?
      (try-dynamic-require-transient-blame-exn-predicate!))
    (define (runtime-error-with-blame? e)
      (define (contract-from-runtime? blame-obj)
        (define src (srcloc-source (blame-source blame-obj)))
        (and (string? src)
             (regexp-match? #rx"^<collects>/"
                            src)))
      ;; Can't get the source of the blame from a transient error, it blows up
      ;; because the blame object is not actually a blame object.
      ;; So just short circuit if we have one of those.
      (and exn:fail:contract:blame:transient?
           (not (exn:fail:contract:blame:transient? e))
           (exn:fail:contract:blame? e)
           (contract-from-runtime? (exn:fail:contract:blame-object e))))
    (define extract-runtime-contract-error-blamed-location
      ;; lltodo: this should really refer not to the configurable, but a common
      ;; utility required by both
      (racket-contracts:make-extract-blamed a-program
                                            program-config
                                            format-mutant-info-for-error))
    (define (report-unexpected-error name message e)
      (raise-experiment-user-error name
                                   @~a{
                                       @message
                                       Message:
                                       @exn-message[e]

                                       Context:
                                       @(pretty-format
                                         (continuation-mark-set->context
                                          (exn-continuation-marks e)))
                                       }))
    (define (handle-module-evaluation-error runner-e)
      (define e (exn:fail:runner:module-evaluation-error runner-e))
      (log-mutation-runner-info
       @~a{Run raised a module evaluation error with message: @(exn-message e)})
      (when (current-mutated-program-exn-recordor) ((current-mutated-program-exn-recordor) e))
      (match e
        [(? type-checker-failure?)
         ((make-status* 'type-error)
          (extract-type-error-source e))]
        [(? exn:fail?)
         ;; Any failures during compilation that aren't type errors are
         ;; categorized as syntax errors.
         ((make-status* 'syntax-error))]
        [else
         (report-unexpected-error 'handle-module-evaluation-error
                                  "Unexpected error while loading modules."
                                  e)]))
    (define (handle-runtime-error runner-e)
      (define e (exn:fail:runner:runtime-error runner-e))
      (log-mutation-runner-info
       @~a{Run raised a runtime error with message: @(exn-message e)})
      (log-mutation-runner-debug
       @~a{Is it an internal error? @(experiment-internal-error? e)})
      (when (current-mutated-program-exn-recordor) ((current-mutated-program-exn-recordor) e))
      (match e
        [(? runtime-error-with-blame?)
         ((make-status* 'runtime-error)
          (extract-runtime-contract-error-blamed-location e))]
        [(? exn:fail:contract:blame?)
         ((make-status* 'blamed)
          (extract-blamed e))]
        [(? exn:fail:syntax?) ; don't think should ever happen?
         ((make-status* 'syntax-error))]
        [(? exn:fail?)
         ((make-status* 'runtime-error)
          (extract-runtime-error-location e))]
        [else
         (report-unexpected-error 'handle-runtime-error
                                  "Unexpected non-`exn:fail?` error while running program."
                                  e)]))
    (define run/handled
      (λ _
        (with-handlers
          (;; see configurables/benchmark-runner/load-pre-computed-result.rkt
           [run-status? values]

           [experiment-internal-error? raise]
           [(exn:fail:runner-wrapped? experiment-internal-error?) (compose1 raise
                                                                            exn:fail:runner-unwrap)]

           [exn:fail:runner:module-evaluation? handle-module-evaluation-error]
           [exn:fail:runner:runtime?           handle-runtime-error]

           [exn? (λ (e)
                   (when (current-mutated-program-exn-recordor)
                     ((current-mutated-program-exn-recordor) e))
                   (report-unexpected-error 'run-with-mutated-module
                                            "Something has gone horribly wrong."
                                            e))])
          (run)
          ((make-status* 'completed)))))
    (log-mutation-runner-info @~a{Running handled run thunk with limits})
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
  (require ruinit
           racket/runtime-path)

  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (define-runtime-path transient-config "../configurables/configs/transient-oldest.rkt")
  (install-configuration! test-config)

  (define-test (test/no-error run-thunk test-thunk)
    (with-handlers ([exn:fail? (λ (e)
                                 ((error-display-handler) "" e)
                                 (fail "test raised exception"))])
      (test-thunk (run-thunk))))
  (define p-config
    ;; The modules must all be present in this config, but their levels don't matter
    (hash "a.rkt" 'none
          "b.rkt" 'none
          "c.rkt" 'none))
  (test-begin
    #:name run-with-mutated-module/mutations
    (ignore
     (define a (mod "./test-mods/a.rkt"
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
     (define b (mod "./test-mods/b.rkt"
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
     (define c (mod "./test-mods/c.rkt"
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
                                          p-config
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
                                          p-config
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
                                   p-config
                                   #:timeout/s 5
                                   #:memory/gb 3))
     (λ (r) (test-match r (struct* run-status ([outcome 'timeout])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   10
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'oom]))))))

  (test-begin
    #:name run-with-mutated-module/ctc-violations
    (ignore
     (define main (mod "./test-mods/main.rkt"
                       #'(module main racket
                           (#%module-begin
                            (require "second.rkt")
                            (define (foo x) (if (positive? x) (foo (- x)) (* x x 3)))
                            (define (main) (bar (foo 2) "yes"))
                            (main)))))
     (define second (mod "./test-mods/second.rkt"
                         #'(module second typed/racket
                             (#%module-begin
                              (: bar (-> Integer String Nonpositive-Integer))
                              (provide bar)
                              (define (bar x s) (if (positive? x) (- x) x))))))
     (define p (program main (list second))))
    (test-exn (λ (e)
                (and (exn:fail:runner:runtime? e)
                     (exn:fail:contract? (exn:fail:runner:runtime-error e))))
              ((make-instrumented-runner
                (program (mod "./test-mods/main.rkt"
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
                                   4
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'blamed]
                                               [blamed '("main.rkt")]))))))

  (define (mod/loc path stx)
    (mod path
         (replace-stx-location stx path)))

  (test-begin
    #:name run-with-mutated-module/type-error
    (ignore
     (define a (mod/loc "./test-mods/a.rkt"
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
     (define b (mod/loc "./test-mods/b.rkt"
                        #'(module b typed/racket
                            (#%module-begin
                             (provide c d)
                             (require "c.rkt")

                             (define d : (List False One) (list #f 1))

                             (: c Boolean)
                             (define c (first d))

                             (displayln (list 'c c))
                             (displayln (list 'd d))))))
     (define c (mod/loc "./test-mods/c.rkt"
                        #'(module c typed/racket
                            (#%module-begin
                             (provide x)
                             (: x One)
                             (define x 1)
                             (displayln (list 'x x))))))
     (define p (program a (list b c))))

    (test/no-error
     (λ _ (run-with-mutated-module p
                                   b
                                   0
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'type-error]
                                               [blamed '("b.rkt")])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   c
                                   0
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'type-error]
                                               [blamed '("c.rkt")])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   13 ;; runtime error -> blame on a.rkt
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("a.rkt")])))))

    (ignore (define mutator-that-changes-source-locations
              (λ (a-mod index #:in program)
                (values (syntax-parse (mod-stx a-mod)
                          [(mod-stuff
                            ...
                            (top-level-stuff
                             ...
                             (def c-id (the-first d-id))
                             disp1
                             disp2))
                           (datum->syntax
                            #'hello
                            (syntax->datum
                             #'(mod-stuff
                                ...
                                (top-level-stuff
                                 ...
                                 (def c-id (x d-id))
                                 disp1
                                 disp2))))])
                        'x))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   b
                                   0
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1
                                   #:mutator mutator-that-changes-source-locations
                                   #:suppress-output? #f))
     (λ (r) (test-match r (struct* run-status ([outcome 'type-error]
                                               [blamed '("b.rkt")]))))))


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
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-equal?
             r
             (run-with-mutated-module p/f
                                      a/f
                                      3
                                      p-config
                                      #:timeout/s 60
                                      #:memory/gb 1))))
    (test/no-error
     (λ _ (run-with-mutated-module p/ml
                                   a/ml
                                   3
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("a.rkt")]))))))
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
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1
                                   #:suppress-output? #f))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("c.rkt")])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   1 ; (+ 1) to (- 1) -> crash in foo
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("b.rkt")])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   4 ; (+ 1) to (+ 0) -> crash in top level of a.rkt
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("a.rkt")])))))
    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   8 ; 0 to 1 -> crash in main
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("a.rkt")])))))


    ;; Test errors that require errortrace to get the right location
    (ignore
     (define d (mod/loc (simple-form-path "./test-mods/d.rkt")
                        #'(module d racket
                            (#%module-begin
                             (require "e.rkt")
                             (define x 0)
                             (define (main x) (g x))
                             (main 42)))))
     (define e (mod/loc (simple-form-path "./test-mods/e.rkt")
                        #'(module e racket
                            (#%module-begin
                             (provide g)
                             (define (g x) (error 'g "bug"))))))
     (define p2 (program d (list e))))
    (test/no-error
     (λ _ (run-with-mutated-module p2
                                   d
                                   0 ; mutation doesn't matter
                                   (hash "d.rkt" 'none
                                         "e.rkt" 'none)
                                   #:timeout/s 60
                                   #:memory/gb 1))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("e.rkt")]))))))


  (test-begin
    #:name run-with-mutated-module/runtime-error-locations-with-blame
    (ignore
     (define a (mod/loc (simple-form-path "./test-mods/a.rkt")
                        #'(module a racket
                            (#%module-begin
                             (define (f x)
                               (~r x #:min-width 2))
                             (f -42)))))
     (define p (program a (list))))

    (test/no-error
     (λ _ (run-with-mutated-module p
                                   a
                                   0 ; swap x and #:min-width -> ~r raises ctc violation
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1
                                   #:suppress-output? #t))
     (λ (r) (test-match r (struct* run-status ([outcome 'runtime-error]
                                               [blamed '("a.rkt")])))))

    ;; The handling for above needs to not trigger on -- and not blow up on --
    ;; actual contract violations from transient. This is an example that caught
    ;; me:
    (ignore
     (define a (mod/loc (simple-form-path "./test-mods/a.rkt")
                        #'(module a racket
                            (#%module-begin
                             (require "b.rkt")
                             (define x #f)
                             (f x)))))
     (define b (mod/loc (simple-form-path "./test-mods/b.rkt")
                        #'(module b typed/racket
                            (#%module-begin
                             (provide f)
                             (: f (-> Number Number))
                             (define (f x)
                               (+ x x))))))
     (define p (program a (list b))))
    (test/no-error
     (λ _
       (call-with-configuration
        transient-config
        (λ _
          (run-with-mutated-module p
                                   a
                                   0 ; #f -> #t
                                   p-config
                                   #:timeout/s 60
                                   #:memory/gb 1
                                   #:suppress-output? #f))))
     (λ (r) (test-match r (struct* run-status ([outcome 'blamed]
                                               [blamed '("b.rkt"
                                                         "a.rkt")])))))))


;; for debugging
(module+ debug
  (provide diff-mutation)

  (require "../util/read-module.rkt"
           ruinit/diff/diff)
  (define (diff-mutation module-to-mutate mutation-index the-program)
    (define the-mod
      (match module-to-mutate
        [(mod _ stx) module-to-mutate]
        [(? path-string? path) (make-mod path)]
        [other (raise-argument-error 'diff-mutation
                                     "either a mod/c or a path-string?"
                                     other)]))
    (define-values (mutated-program-stx mutated-id)
      (mutate-module the-mod mutation-index #:in the-program))
    (printf "--------------------\nMutated: ~a\n" mutated-id)
    (dumb-diff-lines/string
     (pretty-format (syntax->datum (mod-stx the-mod)))
     (pretty-format (syntax->datum mutated-program-stx)))))
