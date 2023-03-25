#lang at-exp racket

(provide debug-mutant
         debug-mutant/infer)

(require "../runner/mutation-runner.rkt"
         (submod "../runner/mutation-runner.rkt" debug)
         "program.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../runner/unify-program.rkt"
         "../experiment/blame-trail-data.rkt"
         "../util/mutant-util.rkt"
         "../util/path-utils.rkt"
         "../configurables/configurables.rkt"
         racket/runtime-path)

(define-runtime-path benchmarks-dir "../../../gtp-benchmarks/benchmarks")
(define-runtime-path config-dir "../configurables/configs")

(define (find-benchmark name-or-path)
  (match name-or-path
    [(regexp #rx"/") name-or-path]
    [name (build-path benchmarks-dir name)]))

(define (read-config identifier the-benchmark)
  (match identifier
    [(hash-table [(? symbol?) _] ...)
     (for/hash ([{k v} (in-hash identifier)])
       (values (~a k) v))]
    [(hash-table [(? string?) _] ...) identifier]
    [(and (regexp @~a{^#hash})
          hash-string)
     (call-with-input-string hash-string read)]
    [(regexp @~a{^#s\(mutant-summary.+(#hash.+\)\))}
             (list _ hash-string))
     (call-with-input-string hash-string read)]
    [(and (or 'types 'none) level)
     (define mods (map file-name-string-from-path
                       (benchmark-untyped the-benchmark)))
     (for/hash ([mod (in-list mods)])
       (values mod level))]
    [(? serialized-config? n)
     (deserialize-config n #:benchmark the-benchmark)]
    [other
     (raise-user-error
      @~a{Unable to extract configuration from identifier: @~v[other]})]))

(define (decode-config bench-name-or-path
                       identifier
                       #:config [config-name "TR"])
  (define experiment-config (build-path config-dir (~a config-name ".rkt")))
  (unless (file-exists? experiment-config)
    (raise-user-error
     'debug-mutant
     @~a{Unable to find config named @|config-name|.rkt in @config-dir}))

  (install-configuration! experiment-config)
  (define bench-path (find-benchmark bench-name-or-path))
  (define the-benchmark (read-benchmark bench-path))
  (read-config identifier the-benchmark))

(define (debug-mutant bench-name-or-path
                      mutated-module-name
                      index
                      identifier
                      #:interactive? [interactive? #t]

                      #:diff-mutant? [diff-mutant? #f]
                      #:stop-diff-early? [stop-diff-early? #f]
                      #:run? [run? #f]
                      #:run-process? [run-via-process? #f]
                      #:run-process-async? [run-via-process-async? #f]
                      #:write-modules-to [dump-dir-path #f]
                      #:suppress-output? [suppress-output? (not interactive?)]
                      #:config [config-name "TR"]
                      #:print-exn? [print-exn? interactive?])
  (define experiment-config (build-path config-dir (~a config-name ".rkt")))
  (unless (file-exists? experiment-config)
    (raise-user-error
     'debug-mutant
     @~a{Unable to find config named @|config-name|.rkt in @config-dir}))

  (install-configuration! experiment-config)
  (define bench-path (find-benchmark bench-name-or-path))
  (define the-benchmark (read-benchmark bench-path))
  (define config (read-config identifier the-benchmark))
  (match-define (and the-benchmark-configuration
                     (struct* benchmark-configuration
                              ([main main-path]
                               [others others-paths])))
    (configure-benchmark the-benchmark config))
  (define module-to-mutate-path
    (pick-file-by-name (list* main-path others-paths)
                       mutated-module-name))
  (define the-benchmark-program
    ((configured:benchmark-configuration->program) the-benchmark-configuration))
  (define the-program (unify-program-for-running the-benchmark-program))
  (define the-program-mods (program->mods the-program))
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

  (define dump-dir-path*
    (match dump-dir-path
      [#f #f]
      [else
       (when (directory-exists? dump-dir-path) (delete-directory/files dump-dir-path))
       (make-directory* dump-dir-path)
       (define dump-dir-path-or-name* (build-path dump-dir-path "unified"))
       (when (directory-exists? (build-path bench-path "base"))
         (copy-directory/files (build-path bench-path "base")
                               (build-path dump-dir-path "base")))
       dump-dir-path-or-name*]))

  (when diff-mutant?
    (displayln (diff-mutation the-module-to-mutate index the-program
                              #:full? (not stop-diff-early?))))
  (when (or run?
            run-via-process?
            run-via-process-async?)
    (with-handlers ([exn:fail?
                     (λ (e)
                       (cond [interactive?
                              (displayln @~a{
                                             Mutant crashed with exn:
                                             @pretty-format[e]

                                             exn has msg:
                                             })
                              ((error-display-handler)
                               ""
                               e)]
                             [else
                              (run-status the-module-to-mutate
                                          index
                                          '?
                                          '<crashed>
                                          #f
                                          #f
                                          #f
                                          e)]))])
      (when interactive?
        (displayln "Running mutant..."))
      (when (and interactive?
                 (not suppress-output?))
        (displayln @~a{
                       Mutant output:
                       ,------------------------------
                       }))
      (define (run-via-process-async)
        (when interactive?
          (displayln
           "Running as seperate process via `spawn-mutant-runner`..."))
        (define outfile (make-temporary-file))
        (define errfile (make-temporary-file))
        (define ctl
          (parameterize ([mutant-error-log errfile]
                         [default-memory-limit/gb 3])
            (spawn-mutant-runner
             the-benchmark-configuration
             mutated-module-name
             index
             outfile
             experiment-config
             #:write-modules-to dump-dir-path*
             #:force-module-write? #t)))
        (define (cleanup-and-get-results)
          (begin0 (match (ctl 'status)
                    ['running
                     (when interactive?
                       (displayln "Mutant is still running. Killing it."))
                     (ctl 'kill)
                     (sleep 1)
                     (when interactive?
                       (displayln "Killed. Error file contents:"))
                     (displayln (file->string errfile))
                     (file->string outfile)]
                    [else
                     (displayln (file->string errfile))
                     (file->value outfile)])
            (delete-file outfile)
            (delete-file errfile)))
        (values ctl outfile errfile cleanup-and-get-results))
      (cond [run-via-process-async? (run-via-process-async)]
            [else
             (match-define (and rs
                                (struct* run-status
                                         ([mutated-id mutated-id]
                                          [outcome outcome]
                                          [blamed blamed]
                                          [errortrace-stack errortrace]
                                          [context-stack context]
                                          [result-value result-value])))
               (cond [run?
                      (parameterize ([current-mutated-program-exn-recordor
                                      (if print-exn?
                                          (λ (e) ((error-display-handler) "" e))
                                          (current-mutated-program-exn-recordor))])
                        (run-with-mutated-module
                         the-program
                         the-module-to-mutate
                         index
                         config
                         #:timeout/s (default-timeout/s)
                         #:memory/gb 3
                         #:modules-base-path (find-program-base-path the-program)
                         #:write-modules-to dump-dir-path*
                         #:on-module-exists 'replace
                         #:suppress-output? suppress-output?))]
                     [run-via-process?
                      (define-values {ctl outfile errfile cleanup-and-get-results}
                        (run-via-process-async))
                      (when interactive?
                        (displayln "Waiting up to 6min for mutant to finish..."))
                      (define wait-thd
                        (thread (thunk (ctl 'wait))))
                      (sync/timeout (* 6 60) wait-thd)
                      (cleanup-and-get-results)]))
             (when (and interactive?
                        (not suppress-output?))
               (displayln @~a{
                              `------------------------------
                              }))
             (if interactive?
                 (displayln
                  @~a{
                      Mutated: @mutated-id
                      Outcome: @outcome @;
                      @(match outcome
                         [(or 'type-error 'blamed 'runtime-error)
                          @~a{

                              Blamed:  @blamed
                              }]
                         [else ""])
                      Errortrace stack: @errortrace
                      Context stack:    @context
                      Result:  @result-value
                      })
                 rs)]))))

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
  (define maybe-adapt-mutant-summary
    (with-handlers ([exn:fail? (const identity)])
      (dynamic-require 'bex-data-analysis/results/data-adapter
                       'adapt-mutant-summary)))
  (match infer-v
    [(regexp @pregexp{
                      #s\(mutant-summary \d+ #s\(run-status ("[^"]+") (\d+) .+(#hash.+\)\))
@;" balance quotes
                      }
             (list _ mod-name index config))
     (values (call-with-input-string mod-name read)
             (call-with-input-string index    read)
             (call-with-input-string config   read))]
    [(struct* mutant-summary
              ([run-status (struct* run-status
                                    ([mutated-module mod-name]
                                     [index index]))]
               [config config]))
     (values mod-name index config)]
    [(and (? mutant-summary?)
          (app maybe-adapt-mutant-summary
               (struct* mutant-summary
                        ([run-status (struct* run-status
                                              ([mutated-module mod-name]
                                               [index index]))]
                         [config config]))))
     (values mod-name index config)]
    [(regexp @regexp{^\(mutant-info})
     (infer-debug-mutant-arguments (call-with-input-string infer-v read))]
    [(list* 'mutant-info
            module-to-mutate-path
            mutation-index
            configured-mods)
     (define config (for/hash ([mod (in-list configured-mods)])
                      (define name (file-name-string-from-path mod))
                      (define untyped? (string-contains? mod "untyped"))
                      (values name (if untyped? 'none 'types))))
     (values (file-name-string-from-path module-to-mutate-path)
             mutation-index
             config)]
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

  (define test-summary
    '#s(mutant-summary
        552
        #s(run-status "ai.rkt"
                      18
                      next
                      type-error
                      ("ai.rkt")
                      ()
                      ()
                      #f)
        #hash(("ai.rkt" . types)
              ("benv.rkt" . none)
              ("denotable.rkt" . none)
              ("main.rkt" . none)
              ("structs.rkt" . types)
              ("time.rkt" . types)
              ("ui.rkt" . types))))

  (test-begin
    #:name read-config
    (test-equal? (read-config (~s test-summary) #f)
                 (mutant-summary-config test-summary))
    (test-equal? (read-config 'types
                              (read-benchmark
                               (find-benchmark "kcfa")))
                 #hash(("ai.rkt" . types)
                       ("benv.rkt" . types)
                       ("denotable.rkt" . types)
                       ("main.rkt" . types)
                       ("structs.rkt" . types)
                       ("time.rkt" . types)
                       ("ui.rkt" . types))))

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
    (ignore
     )
    (test-with-values
     (λ _ (infer-debug-mutant-arguments test-summary))
     (λ (mod-name) (test-equal? mod-name "ai.rkt"))
     (λ (index) (test-= index 18))
     (λ (config) (test-equal? config (mutant-summary-config test-summary))))
    (test-with-values
     (λ _ (infer-debug-mutant-arguments (~s test-summary)))
     (λ (mod-name) (test-equal? mod-name "ai.rkt"))
     (λ (index) (test-= index 18))
     (λ (config) (test-equal? config (mutant-summary-config test-summary))))))
