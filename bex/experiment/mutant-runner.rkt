#lang at-exp racket/base

(require racket/list
         racket/match
         racket/sequence
         racket/string
         racket/cmdline
         racket/port
         racket/format
         "../runner/mutation-runner.rkt"
         "../runner/unify-program.rkt"
         "../util/program.rkt"
         "../configurables/configurables.rkt"
         "../configurations/configure-benchmark.rkt")

(define (fail fmt-str . fmt-args)
  (apply eprintf
         (string-append fmt-str "\n")
         fmt-args)
  (exit 1))

(module+ main
  (define the-benchmark-configuration (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define write-modules-to (make-parameter #f))
  (define on-module-exists (make-parameter 'error))
  (define timeout/s (make-parameter #f))
  (define memory/gb (make-parameter #f))
  (define mutant-output-path (make-parameter #f))
  (define configuration-path (make-parameter #f))

  (command-line
   #:once-each
   [("-b" "--benchmark-configuration")
    benchmark-configuration-str
    ("`write` form of the `benchmark-configuration` to run."
     "This is a mandatory argument.")
    (with-handlers ([exn:fail:read?
                     (λ _
                       (fail
                        @~a{
                            Unable to read benchmark configuration
                            Provided: @~v[benchmark-configuration-str]}))])
      (the-benchmark-configuration (with-input-from-string benchmark-configuration-str read)))]
   [("-M" "--module-to-mutate")
    mutate-path
    ("Module to mutate path."
     "This is a mandatory argument.")
    (module-to-mutate mutate-path)]
   [("-i" "--mutation-index")
    index
    ("Mutation index."
     "This is a mandatory argument.")
    (mutation-index (string->number index))]
   [("-w" "--write-modules-to")
    output-path
    "Path to output mutated modules to"
    (write-modules-to output-path)]
   [("-f" "--overwrite-modules")
    "When writing modules, overwrite files that already exist"
    (on-module-exists 'replace)]
   [("-t" "--timeout")
    seconds
    "Timeout limit (seconds)"
    (timeout/s (string->number seconds))]
   [("-g" "--memory-limit")
    gb
    "Memory limit (GB)"
    (memory/gb (string->number gb))]

   [("-O" "--output")
    path
    "Send mutant output to `path` instead of suppressing it."
    (mutant-output-path path)]

   [("-c" "--config")
    path
    ("The configuration with which to run the mutant."
     "This is a mandatory argument.")
    (configuration-path path)])

  (define mutant-output-path-port
    (match (mutant-output-path)
      [#f #f]
      [path
       (define port (open-output-file #:exists 'replace path))
       (file-stream-buffer-mode port 'line)
       port]))
  (define (mutant-output-or get-port)
    (or mutant-output-path-port
        (get-port)))

  (define missing-arg
    (for/first ([arg (in-list (list (the-benchmark-configuration)
                                    (module-to-mutate)
                                    (mutation-index)
                                    (configuration-path)))]
                [flag (in-list '(-b -M -i -c))]
                #:unless arg)
      flag))

  (when missing-arg
    (fail
     @~a{Error: Missing mandatory argument: @missing-arg}))

  (install-configuration! (configuration-path))

  (define the-program
    (unify-program-for-running
     ((configured:benchmark-configuration->program)
      (the-benchmark-configuration))))

  (define the-program-mods (program->mods the-program))

  (define the-module-to-mutate
    (find-unified-module-to-mutate (module-to-mutate)
                                   the-program-mods))

  (unless (member the-module-to-mutate the-program-mods)
    (fail
     @~a{
         Error: Module to mutate not in given program.
         Program: @the-program
         Module: @(module-to-mutate)
         }))

  (define the-run-status
      (parameterize ([current-output-port (mutant-output-or current-output-port)]
                     [current-error-port  (mutant-output-or current-error-port)]
                     [current-mutated-program-exn-recordor
                      (and mutant-output-path-port
                           (λ (e) ((error-display-handler) (exn-message e) e)))])
        (run-with-mutated-module
         the-program
         the-module-to-mutate
         (mutation-index)
         (benchmark-configuration-config (the-benchmark-configuration))
         #:timeout/s (timeout/s)
         #:memory/gb (memory/gb)
         #:modules-base-path (find-program-base-path the-program)
         #:write-modules-to (write-modules-to)
         #:on-module-exists (on-module-exists)
         #:suppress-output? (not (mutant-output-path)))))
  (when mutant-output-path-port
    (close-output-port mutant-output-path-port))

  (writeln the-run-status))


(module+ test
  (require ruinit
           racket
           "../configurables/configurables.rkt"
           racket/runtime-path)

  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (install-configuration! (simple-form-path test-config))

  (define-test-env {setup! cleanup!}
    #:directories ([test-bench "./test-bench"]
                   [ut "./test-bench/untyped"]
                   [t  "./test-bench/typed"])
    #:files ([test-module-1 (build-path ut "test-mod-1.rkt")
                           @~a{
                               #lang racket

                               (require "test-mod-2.rkt")

                               (define (baz x y)
                                 (if (even? x)
                                     y
                                     (/ y x)))

                               (foo (baz 0 1))
                               }]
             [test-module-2 (build-path t "test-mod-2.rkt")
                           @~a{
                               #lang typed/racket

                               (provide foo)

                               (: foo (-> Number Number))
                               (define (foo x)
                                 (+ x x))
                               }]))
  (test-begin
    #:name mutant-runner
    #:short-circuit
    #:before (setup!)
    #:after (cleanup!)

    (ignore
     (define test-program (make-unified-program test-module-1
                                                (list test-module-2)))
     (define to-mutate
       (find-unified-module-to-mutate test-module-1
                                      (program->mods test-program))))

    (or (run-with-mutated-module
         test-program
         to-mutate
         0
         (hash))
        #t)))
