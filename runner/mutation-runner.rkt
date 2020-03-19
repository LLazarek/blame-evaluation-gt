#lang at-exp racket

(provide run-with-mutated-module
         report-progress
         make-mutated-program-runner
         mutate-module
         mutation-index-exception?
         [struct-out run-status]
         index-exceeded?)

(require racket/runtime-path
         syntax/parse
         syntax/strip-context
         "../mutate/mutate.rkt"
         "sandbox-runner.rkt"
         "instrumented-runner.rkt"
         "../util/path-utils.rkt")

(define-logger mutant-runner)

(define mutate-top-level-selector select-define)
(define mutate-expression-filter (λ (e)
                                   (syntax-parse e
                                     [({~datum :} . _) #f]
                                     [else #t])))

;; Produce the mutated syntax for the module at the given path
(define (mutate-module module-stx mutation-index)
  (syntax-parse module-stx
    #:datum-literals [module]
    [(module name lang {~and mod-body (mod-begin body ...)})
     #:do [(define program-stx #'{body ...})
           (match-define (mutated-program program-stx/mutated mutated-id)
             (mutate-program program-stx mutation-index
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
       #:pre/desc (base-path write-to-dir)
       (or (not (and write-to-dir (not (unsupplied-arg? write-to-dir))
                     (or (not base-path) (unsupplied-arg? base-path))))
           "must specify #:modules-base-path if #:write-modules-to is specified")

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
                             ns))

  (define runner
    (make-instrumented-runner a-program
                              instrument-module
                              #:setup-namespace setup-namespace!))
  (define mutated-id (unbox mutated-id-box))

  (values runner mutated-id))






(struct run-status (result-value
                    outcome
                    blamed
                    mutated-module
                    mutated-id
                    index)
  #:prefab)

;; run-status -> bool
(define (index-exceeded? rs)
  (equal? (run-status-outcome rs) 'index-exceeded))


(define report-progress (make-parameter #f))

;; run-status -> void
(define/match (display-run-status rs)
  [{(run-status result-value outcome blamed
                mutated-module mutated-id index)}
   (printf "
Run: mutated ~a in module ~a at index ~a
Status: ~a
Returned: ~a
Blamed: ~a

"
           mutated-id mutated-module index
           outcome
           result-value
           blamed)])


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

       [result run-status?])

  (define (make-status status-sym [blamed #f] [mutated-id #f] [result #f])
    (run-status result
                status-sym
                blamed
                module-to-mutate
                mutated-id
                mutation-index))
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
    (define ((make-status* status-sym) [blamed #f])
      (make-status status-sym
                   blamed
                   mutated-id))
    (define (extract-blamed e)
      (define blamed ((compose
                       (match-lambda [`(function ,id) id]
                                     [`(definition ,id) id]
                                     [other other])
                       blame-positive
                       exn:fail:contract:blame-object)
                      e))
      ((make-status* 'blamed)
       (cons blamed (exn-continuation-marks e))))
    (define (extract-type-error-source e)
      (define failing-stxs (exn:fail:syntax-exprs e))
      (define module-name
        (match failing-stxs
          [(list* (app syntax-source (or (? string? path-str)
                                         (? path? (app path->string path-str))))
                  _)
           (file-name-string-from-path path-str)]
          [else
           "??? couldn't extract module name ???"]))
      ((make-status* 'type-error)
       module-name))
    (define run/handled
      (λ _
        (with-handlers
          ([exn:fail:contract:blame? extract-blamed]
           [type-checker-failure? extract-type-error-source]
           [exn:fail:out-of-memory? (λ _ ((make-status* 'oom)))]
           [exn? (make-status* 'crashed)])
          (run)
          ((make-status* 'completed)))))
    (run-with-limits run/handled
                     #:timeout/s timeout/s
                     #:timeout-result (make-status* 'timeout)
                     #:memory/gb memory/gb
                     #:oom-result (make-status* 'oom)
                     #:suppress-output? suppress-output?)))



(module+ test
  (require ruinit)
  (test-begin
    #:name run-with-mutated-module
    (ignore
     (define a (mod "a.rkt"
                    #'(module a racket
                        (#%module-begin
                         (require "b.rkt")

                         (define/contract a any/c 1)
                         (define/contract b any/c 1)

                         (define/contract (foo x y)
                           (-> number? number? number?)
                           (if #t
                               (- y x)
                               (+ (sleep 1)
                                  (for/vector #:length 4294967296 () #f)
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

                         (define/contract c
                           any/c
                           5)
                         (define/contract d
                           number?
                           (if #t 1 "one"))
                         (displayln (list 'c c))
                         (displayln (list 'd d))))))
     (define c (mod "c.rkt"
                    #'(module c typed/racket
                        (#%module-begin
                         (void)))))
     (define p (program a (list b c))))
    (test-equal?
     (with-output-to-string
       (λ _ (run-with-mutated-module p
                                     a
                                     0
                                     #:suppress-output? #f)))
     "B
(c 5)
(d 1)
(a 0)
(b 1)
4
")

    (test-equal?
     (with-output-to-string
       (λ _ (run-with-mutated-module p
                                     b
                                     0
                                     #:suppress-output? #f)))
     "B
(c -1)
(d 1)
(a 1)
(b 1)
-2
")

    (test-match
     (run-with-mutated-module p
                              a
                              2
                              #:timeout/s 3
                              #:memory/gb 1)
     (struct* run-status ([outcome 'timeout])))
    (test-match
     (run-with-mutated-module p
                              a
                              2
                              #:timeout/s 10
                              #:memory/gb 0.5)
     (struct* run-status ([outcome 'oom]))))

  (test-begin
    #:name run-with-mutated-module/type-error
    (ignore
     (define a (mod "a.rkt"
                    #'(module a racket
                        (#%module-begin
                         (require "b.rkt")

                         (define/contract a any/c 1)
                         (define/contract b any/c 1)

                         (define/contract (foo x y)
                           (-> number? number? number?)
                           (if #t
                               (- y x)
                               (+ (sleep 1)
                                  (for/vector #:length 4294967296 () #f)
                                  (foo x y))))

                         (displayln (list 'a a))
                         (displayln (list 'b b))
                         (foo d c)))))
     (define b (mod "b.rkt"
                    #'(module b typed/racket
                        (#%module-begin
                         (provide c d)
                         (require "c.rkt")

                         (: d (List Boolean Natural))
                         (define d (list #f 1))

                         (: c Boolean)
                         (define c (list-ref d 0))

                         (displayln (list 'c c))
                         (displayln (list 'd d))))))
     (define c (mod "c.rkt"
                    #'(module c typed/racket
                        (#%module-begin
                         (: x One)
                         (define x 1)
                         (displayln (list 'x x))))))
     (define p (program a (list b c))))

    (test-match
     (run-with-mutated-module p
                              b
                              0
                              #:timeout/s 3
                              #:memory/gb 1)
     (struct* run-status ([outcome 'type-error]
                          ;; because the syntax is from here!
                          [blamed "mutation-runner.rkt"])))
    (test-match
     (run-with-mutated-module p
                              c
                              0
                              #:timeout/s 3
                              #:memory/gb 1)
     (struct* run-status ([outcome 'type-error]
                          ;; because the syntax is from here!
                          [blamed "mutation-runner.rkt"])))))


;; for debugging
(module+ debug
  (provide diff-mutation
           mutant-count
           format-raw-config-for-runner)

  (require "../util/read-module.rkt"
           ruinit/diff/diff)
  (define (diff-mutation module-to-mutate mutation-index)
    (define orig-module-stx (read-module module-to-mutate))
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
