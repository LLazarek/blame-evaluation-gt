#lang at-exp racket/base

(require racket/list
         racket/match
         racket/sequence
         racket/string
         racket/cmdline
         racket/port
         racket/format
         "../runner/mutation-runner.rkt"
         "../runner/instrumented-runner.rkt"
         "../mutate/mutate.rkt"
         "program.rkt")

(define (fail fmt-str . fmt-args)
  (apply eprintf
         (string-append fmt-str "\n")
         fmt-args)
  (exit 1))

(define (find-program-base-path a-program)
  (define files
    (map mod-path
         (list* (program-main a-program)
                (program-others a-program))))
  (define main (first files))
  (define candidate-subpath-parts
    (in-combinations (explode-path main)))
  (define ((path-prefix?-of a-path) subpath)
    (string-prefix? (path->string a-path)
                    (path->string subpath)))
  (define candidate-subpaths
    (sequence-filter
     (path-prefix?-of main)
     (sequence-map (match-lambda
                     ['() (string->path "nothing")]
                     [parts (apply build-path parts)])
                   candidate-subpath-parts)))
  (for/last ([candidate candidate-subpaths]
             #:when (for/and ([f (in-list files)])
                      ((path-prefix?-of f) candidate)))
    candidate))

(module+ test
  (require ruinit)
  (test-begin
    #:name find-program-base-path
    (test-equal?
     (find-program-base-path
      (program (mod (string->path "/foo/bar/bench/untyped/main.rkt") #'())
               (list (mod (string->path "/foo/bar/bench/typed/baz.rkt") #'())
                     (mod (string->path "/foo/bar/bench/typed/bez.rkt") #'()))))
     (string->path "/foo/bar/bench"))))

(module+ main
  (define main-module (make-parameter #f))
  (define other-modules (make-parameter #f))
  (define module-to-mutate (make-parameter #f))
  (define mutation-index (make-parameter #f))
  (define write-modules-to (make-parameter #f))
  (define on-module-exists (make-parameter 'error))
  (define timeout/s (make-parameter #f))
  (define memory/gb (make-parameter #f))

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
    (other-modules (call-with-input-string other-module-path-list
                                           read))]
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
    (memory/gb (string->number gb))])

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
    (make-program (main-module)
                  (other-modules)))

  (define the-module-to-mutate
    (make-mod (module-to-mutate)))

  (unless (member the-module-to-mutate
                  (list* (program-main the-program)
                         (program-others the-program)))
    (fail
     @~a{
         Error: Module to mutate not in given program.
         Program: @the-program
         Module: @the-module-to-mutate
         }))

  (define the-run-status
    (run-with-mutated-module
     the-program
     the-module-to-mutate
     (mutation-index)
     #:timeout/s (timeout/s)
     #:memory/gb (memory/gb)
     #:modules-base-path (find-program-base-path the-program)
     #:write-modules-to (write-modules-to)
     #:on-module-exists (on-module-exists)))

  (writeln the-run-status))
