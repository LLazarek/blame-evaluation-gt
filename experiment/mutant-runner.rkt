#lang at-exp racket/base

(require racket/list
         racket/match
         racket/sequence
         racket/string
         racket/cmdline
         racket/port
         racket/format
         "../runner/mutation-runner.rkt"
         "../runner/program.rkt"
         "../runner/unify-program.rkt"
         "../configurables/configurables.rkt")

(define (fail fmt-str . fmt-args)
  (apply eprintf
         (string-append fmt-str "\n")
         fmt-args)
  (exit 1))

(module+ main
  (define main-module (make-parameter #f))
  (define other-modules (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define write-modules-to (make-parameter #f))
  (define on-module-exists (make-parameter 'error))
  (define timeout/s (make-parameter #f))
  (define memory/gb (make-parameter #f))
  (define mutant-output-path (make-parameter #f))

  (command-line
   #:once-each
   [("-m" "--main-module")
    main-path
    ("Main module path."
     "This is a mandatory argument.")
    (main-module main-path)]
   [("-o" "--other-modules")
    other-module-path-list
    ("`write` form of a list of other module paths."
     "This is a mandatory argument.")
    (with-handlers ([exn:fail:read?
                     (λ _
                       (fail
                        @~a{
                            Unable to read path list
                            Provided: @~v[other-module-path-list]}))])
      (other-modules
       (call-with-input-string other-module-path-list
                               read)))]
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
    "The configuration with which to run the mutant."
    (current-configuration-path path)])

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
    (for/first ([arg (in-list (list (main-module)
                                    (other-modules)
                                    (module-to-mutate)
                                    (mutation-index)))]
                [flag (in-list '(-m -o -M -i))]
                #:unless arg)
      flag))

  (when missing-arg
    (fail
     @~a{Error: Missing mandatory argument: @missing-arg}))

  (define the-program
    (make-unified-program (main-module)
                          (other-modules)))

  (define the-program-mods (list* (program-main the-program)
                                  (program-others the-program)))

  (define the-module-to-mutate
    (find-unified-module-to-mutate (module-to-mutate)
                                   the-program-mods))

  (unless (member the-module-to-mutate the-program-mods)
    (fail
     @~a{
         Error: Module to mutate not in given program.
         Program: @the-program
         Module: @the-module-to-mutate
         }))

  (define the-run-status
      (parameterize ([current-output-port (mutant-output-or current-output-port)]
                     [current-error-port  (mutant-output-or current-error-port)])
        (run-with-mutated-module
         the-program
         the-module-to-mutate
         (mutation-index)
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

  (define-runtime-path test-config "../configurables/test.config")
  (current-configuration-path test-config)

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
                                      (list* (program-main test-program)
                                             (program-others test-program)))))

    (or (run-with-mutated-module
         test-program
         to-mutate
         0)
        #t)))
