#lang at-exp racket

(require "../runner/mutation-runner.rkt"
         (submod "../runner/mutation-runner.rkt" debug)
         "../runner/program.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../mutate/mutate.rkt"
         "../runner/unify-program.rkt"
         (submod "../experiment/mutant-factory.rkt" test)
         "../util/path-utils.rkt"
         racket/runtime-path)

(define-runtime-path benchmarks-dir "../../gtp-benchmarks/benchmarks")

(define (find-benchmark name-or-path)
  (match name-or-path
    [(regexp #rx"/") name-or-path]
    [name (build-path benchmarks-dir name)]))

(define (read-config identifier)
  (match identifier
    [(? hash? h) h]
    [(and (regexp @~a{^#hash})
          hash-string)
     (call-with-input-string hash-string read)]
    [(regexp @~a{^#s\(aggregated-result .+(#hash.+\)\))}
             (list _ hash-string))
     (call-with-input-string hash-string read)]
    [other
     (raise-user-error
      @~a{Unable to extract configuration from identifier: @~v[other]})]))

(define (debug-mutant bench-name-or-path
                      mutated-module-name
                      index
                      identifier
                      #:diff-mutant? [diff-mutant? #f]
                      #:run? [run? #f]
                      #:write-modules-to [dump-dir-name #f]
                      #:suppress-output? [suppress-output? #f])
  (define bench-path (find-benchmark bench-name-or-path))
  (define the-benchmark (read-benchmark bench-path))
  (define config (read-config identifier))
  (match-define
    (struct* benchmark-configuration
             ([main main-path]
              [others others-paths]))
    (configure-benchmark the-benchmark config))
  (define module-to-mutate-path
    (pick-file-by-name (list* main-path others-paths)
                       mutated-module-name))
  (define the-program (make-unified-program main-path
                                            others-paths))
  (define the-program-mods (list* (program-main the-program)
                                  (program-others the-program)))
  (define the-module-to-mutate
    (find-unified-module-to-mutate module-to-mutate-path
                                   the-program-mods))

  (unless (member the-module-to-mutate the-program-mods)
    (raise-user-error
     @~a{
         Error: Module to mutate not in given program.
         Program: @the-program
         Module: @the-module-to-mutate
         }))

  (when diff-mutant?
    (diff-mutation the-module-to-mutate index))
  (when run?
    (displayln "Running mutant...")
    (with-handlers ([exn:fail?
                     (λ (e)
                       (displayln 'inside-handler)
                       ((error-display-handler
                         @~a{
                             Mutant crashed with exn:
                             @pretty-format[e]

                             exn has msg:
                             }
                         e)))])
      (when (not suppress-output?)
        (displayln @~a{
                       Mutant output:
                       ,------------------------------
                       }))
      (match-define
        (struct* run-status
                 ([mutated-id mutated-id]
                  [outcome outcome]
                  [blamed blamed]
                  [result-value result-value]))
        (run-with-mutated-module
         the-program
         the-module-to-mutate
         index
         #:timeout/s (* 10 60)
         #:memory/gb 5
         #:modules-base-path (find-program-base-path the-program)
         #:write-modules-to dump-dir-name
         #:on-module-exists 'replace
         #:suppress-output? suppress-output?))
      (when (not suppress-output?)
        (displayln @~a{
                       `------------------------------
                       }))
      (displayln
       @~a{
           Mutated: @mutated-id
           Outcome: @outcome @;
           @(match outcome
              [(or 'type-error 'blamed)
               @~a{

                   Blamed:  @blamed
                   }]
              [else ""])
           Result:  @result-value
           }))))

(define debug-mutant/infer
  (make-keyword-procedure
   (λ (kws kw-args . pos-args)
     (match pos-args
       [(list bench-name-or-path
              infer-arg)
        (define-values {mutated-module index identifier}
          (infer-debug-mutant-arguments infer-arg))
        (displayln
         @~a{
             Inferred mutated-module: @mutated-module
                      mutation index: @index
                      identifier:     @~v[identifier]
             })
        (keyword-apply debug-mutant
                       kws
                       kw-args
                       (list bench-name-or-path
                             mutated-module
                             index
                             identifier))]
       [pass-thru-args
        (keyword-apply debug-mutant kws kw-args pass-thru-args)]))))

(define (infer-debug-mutant-arguments infer-v)
  ;; (displayln @pregexp{
;;                       #s\(aggregated-result \S+ \d+ #s\(run-status ("[^"]+") (\d+) .+(#hash.+\)\))
;; @;" balance quotes
;;                       })
;;   (displayln (~v infer-v))
  (match infer-v
    [(regexp @pregexp{
                      #s\(aggregated-result \S+ \d+ #s\(run-status ("[^"]+") (\d+) .+(#hash.+\)\))
@;" balance quotes
                      }
             (list _ mod-name index config))
     (values (call-with-input-string mod-name read)
             (call-with-input-string index    read)
             (call-with-input-string config   read))]
    [(struct* aggregated-result
              ([run-status (struct* run-status
                                    ([mutated-module mod-name]
                                     [index index]))]
               [config config]))
     (values mod-name index config)]
    [other
     (raise-user-error
      @~a{Unable to infer `debug-mutant` arguments from @~v[other]})]))


(module+ test
  (require ruinit)

  (test-begin
    #:name find-benchmark
    (test-equal? (find-benchmark "sieve")
                 (build-path benchmarks-dir "sieve"))
    (test-equal? (find-benchmark "kcfa")
                 (build-path benchmarks-dir "kcfa"))
    (test-equal? (find-benchmark "/foo/bar/baz")
                 "/foo/bar/baz"))

  (test-begin
    #:name read-config
    (test-equal? (read-config @~a{
#s(aggregated-result test 0 #s(run-status "ai.rkt" 0 atom-eval blamed "couldnt-find-mod-name" #f) #hash(("ai.rkt" . types) ("benv.rkt" . types) ("denotable.rkt" . types) ("main.rkt" . types) ("structs.rkt" . types) ("time.rkt" . types) ("ui.rkt" . types)))
})
                 #hash(("ai.rkt" . types) ("benv.rkt" . types) ("denotable.rkt" . types) ("main.rkt" . types) ("structs.rkt" . types) ("time.rkt" . types) ("ui.rkt" . types))))

  (define-test (test-with-values generator . receiver-tests)
    (call-with-values generator
                      (λ results
                        (unless (= (length results)
                                   (length receiver-tests))
                          (fail @~a{
                                    Expected @(length receiver-tests) results @;
                                    but got @(length results)
                                    }))
                        (for/and/test ([result (in-list results)]
                                       [test (in-list receiver-tests)])
                                      (test result)))))
  (test-begin
    #:name infer-debug-mutant-arguments
    (test-with-values
     (λ _ (infer-debug-mutant-arguments
           '#s(aggregated-result test
                                 0
                                 #s(run-status "ai.rkt"
                                               0
                                               atom-eval
                                               blamed
                                               "couldnt-find-mod-name"
                                               #f)
                                 #hash(("ai.rkt" . types)))))
     (λ (mod-name) (test-equal? mod-name "ai.rkt"))
     (λ (index) (test-= index 0))
     (λ (config) (test-equal? config #hash(("ai.rkt" . types)))))
    (test-with-values
     (λ _ (infer-debug-mutant-arguments
           @~a{#s(aggregated-result test @;
                                    0 @;
                                    #s(run-status "ai.rkt" @;
                                                  0 @;
                                                  atom-eval @;
                                                  blamed @;
                                                  "couldnt-find-mod-name" @;
                                                  #f) @;
                                    #hash(("ai.rkt" . types)))}))
     (λ (mod-name) (test-equal? mod-name "ai.rkt"))
     (λ (index) (test-= index 0))
     (λ (config) (test-equal? config #hash(("ai.rkt" . types)))))))
