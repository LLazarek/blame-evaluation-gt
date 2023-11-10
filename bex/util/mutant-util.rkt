#lang at-exp racket

(provide (contract-out
          [spawn-mutant-runner
           ({benchmark-configuration/c
             module-name?
             natural?
             path-string?
             path-string?}
            {#:timeout/s (or/c #f number?)
             #:memory/gb (or/c #f number?)
             #:log-mutation-info? boolean?
             #:save-output (or/c #f path-string?)
             #:write-modules-to (or/c #f path-string?)
             #:force-module-write? boolean?}
            . ->* .
            procedure?)]
          [in-mutation-indices
           (module-name? benchmark/c . -> . (stream/c natural?))]
          [max-mutation-index
           (module-name? benchmark/c . -> . (or/c -1 natural?))]
          [max-mutation-index-exceeded?
           (path-to-existant-file? natural? program/c . -> . boolean?)]
          [extract-mutation
           (mod/c natural? program/c . -> . (list/c symbol? string? (list/c any/c any/c)))]

          [mutant-error-log (parameter/c path-string?)]
          [default-memory-limit/gb (parameter/c (and/c number? positive?))]
          [default-timeout/s (parameter/c (and/c number? positive?))]
          [current-run-with-condor-machines (parameter/c (or/c #f (listof string?)))]
          [current-mutant-runner-log-mutation-info? (parameter/c boolean?)])
         (struct-out mutant))

(require racket/runtime-path
         racket/logging
         net/base64
         syntax/parse
         mutate/logger
         mutate/traversal
         "../configurations/configure-benchmark.rkt"
         "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         "../util/binary-search.rkt"
         "../util/program.rkt"
         "../configurables/configurables.rkt"
         "experiment-exns.rkt")

(define-runtime-path mutant-runner-path "../experiment/mutant-runner.rkt")
(define-runtime-path racket-timeout-b64-path "run-racket-with-timeout-b64.rkt")
(define racket-path (find-executable-path (find-system-path 'exec-file)))
(define timeout-path (find-executable-path "timeout"))

(define mutant-error-log (make-parameter "./mutant-errors.txt"))

(define default-memory-limit/gb (make-parameter 3))
(define default-timeout/s (make-parameter (* 5 60)))

(define current-run-with-condor-machines
  (make-parameter (and (getenv "BEX_CONDOR_MACHINES")
                       (string-split (getenv "BEX_CONDOR_MACHINES")))))

(define condor_submit
  (find-executable-path "condor_submit"))
(define condor_q
  (find-executable-path "condor_q"))
(define condor_rm
  (find-executable-path "condor_rm"))
(define get-condor-job-info
  ;; to avoid polling condor too frequently, and parsing/reparsing the same
  ;; output, cache the results and only refresh every `CACHE-EXPIRATION-SECS`
  (let ([last-poll-time (current-inexact-monotonic-milliseconds)]
        [cached-output #f]
        [CACHE-EXPIRATION-SECS 1])
    (thunk
     (cond [(or (not cached-output)
                (> (- (current-inexact-monotonic-milliseconds)
                      last-poll-time)
                   (* CACHE-EXPIRATION-SECS 1000)))
            (define condor-dump
              (with-output-to-string
                (thunk (system* condor_q))))
            (set! cached-output
                  (and (regexp-match? "-- Schedd: peroni.cs.northwestern.edu" condor-dump)
                       (regexp-match* @pregexp{([1_])\s+[1_]\s+1 (\d{7}\.0)}
                                      condor-dump
                                      #:match-select rest)))
            cached-output]
           [else cached-output]))))

(define current-mutant-runner-log-mutation-info? (make-parameter #f))
(define (spawn-mutant-runner a-benchmark-configuration
                             module-to-mutate-name
                             mutation-index
                             outfile
                             config-path
                             #:timeout/s [timeout/s #f]
                             #:memory/gb [memory/gb #f]
                             #:log-mutation-info? [log-mutation-info? (current-mutant-runner-log-mutation-info?)]
                             #:save-output [output-path #f]

                             #:write-modules-to [dump-dir-path #f]
                             #:force-module-write? [force-module-write? #f])
  (define module-to-mutate
    (resolve-configured-benchmark-module a-benchmark-configuration
                                         module-to-mutate-name))
  (cond
    [(current-run-with-condor-machines)
     (define job-file-contents
       @~a{
           # Set the universe
           Universe = vanilla

           # Describe the target machine
           Requirements = (@(string-join (for/list ([name (in-list (current-run-with-condor-machines))])
                                           @~a{(Machine == "@|name|.cs.northwestern.edu")})
                                         " || "))

           Rank = TARGET.Mips
           Copy_To_Spool = False

           # Notification
           Notification = error

           # Set the environment
           Getenv = True

           Arguments = "@; close "
@(string-join (map (λ (arg) (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (~a arg)) #"")))
(flatten (list
(if timeout/s (* 1.5 timeout/s) 0)
(if log-mutation-info?
    (list "-O" "info@mutate")
    empty)
"--"
mutant-runner-path
"-b" (serialize-benchmark-configuration a-benchmark-configuration)
"-M" module-to-mutate
"-i" (~a mutation-index)
"-t" (~a (or timeout/s
             (default-timeout/s)))
"-g" (~a (or memory/gb
             (default-memory-limit/gb)))
"-c" (~a (simple-form-path config-path))
(if output-path
    (list "-O" output-path)
    empty)
(if dump-dir-path
    (list "-w" dump-dir-path)
    empty)
(if force-module-write?
    '("-f")
    empty)
))))"
@; close "

           Executable = /project/blgt/racket-timeout-b64.sh
           Error = @(mutant-error-log)
           Output = @outfile
           Log = condor-log.txt
           # Ask Condor to kill any jobs that are taking so long they must be stuck
           @; from https://stackoverflow.com/questions/5900400/maximum-run-time-in-condor
           periodic_remove = (CommittedTime - CommittedSuspensionTime) > @(* (default-timeout/s) 3)

           +IsWholeMachineJob = false
           +IsSuspensionJob = false

           Queue

           })
     (match (with-output-to-string
              (thunk
               (parameterize ([current-error-port (current-output-port)])
                 (with-input-from-string job-file-contents
                   (thunk (system* condor_submit
                                   "-verbose"
                                   "-"))))))
       [(regexp @pregexp{\*\* Proc ([\d.]+):} (list _ id))
        (define (get-proc-status)
          (if (member id (map second (get-condor-job-info)))
              'running
              'done-ok))
        (match-lambda ['status
                       (get-proc-status)]
                      ['kill
                       (void (with-output-to-string (thunk (system* condor_rm id))))]
                      ['wait
                       (let loop ()
                         (when (equal? (get-proc-status) 'running)
                           (sleep 1)
                           (loop)))]
                      [other (raise-internal-experiment-error
                              'spawn-mutant-runner
                              @~a{unimplemented process ctl for: @other})])]
       [something-else
        (raise-internal-experiment-error
         'spawn-mutant-runner
         @~a{
             failed to submit condor job, it said:
             @something-else
             })])]
    [else
     (call-with-output-file outfile #:mode 'text #:exists 'replace
       (λ (outfile-port)
         (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
           (λ (error-log-port)
             (match-define (list #f runner-in _ #f runner-ctl)
               (apply process*/ports
                      outfile-port #f error-log-port

                      ;; workaround for runner sometimes getting stuck outside
                      ;; sandbox without any clear explanation
                      ;; 0 means no timeout: see `man timeout`
                      ;; -k 5 means send SIGKILL 5 sec after polite exit signal
                      timeout-path "-k" "5" (~a (if timeout/s (* 1.5 timeout/s) 0))

                      racket-path
                      (append
                       (if log-mutation-info?
                           (list "-O" "info@mutate")
                           empty)
                       (list "--"
                             mutant-runner-path
                             "-b" (serialize-benchmark-configuration a-benchmark-configuration)
                             "-M" module-to-mutate
                             "-i" (~a mutation-index)
                             "-t" (~a (or timeout/s
                                          (default-timeout/s)))
                             "-g" (~a (or memory/gb
                                          (default-memory-limit/gb)))
                             "-c" config-path)
                       (if output-path
                           (list "-O" output-path)
                           empty)
                       (if dump-dir-path
                           (list "-w" dump-dir-path)
                           empty)
                       (if force-module-write?
                           '("-f")
                           empty))))
             (close-output-port runner-in)
             runner-ctl))))]))

(define (resolve-configured-benchmark-module a-benchmark-configuration
                                             a-module-name)
  (findf (path-ends-with a-module-name)
         (list* (benchmark-configuration-main a-benchmark-configuration)
                (benchmark-configuration-others a-benchmark-configuration))))

(define (in-mutation-indices module-to-mutate-name bench)
  (define max-index (max-mutation-index module-to-mutate-name bench))
  (in-range (add1 max-index)))

(define (max-mutation-index module-to-mutate-name bench)
  (define max-config (make-max-bench-config bench))
  (define the-benchmark-configuration
    (configure-benchmark bench
                         max-config))
  (define module-to-mutate-path
    (findf
     (path-ends-with module-to-mutate-name)
     (list*
      (benchmark-configuration-main the-benchmark-configuration)
      (benchmark-configuration-others the-benchmark-configuration))))
  (define the-program
    (benchmark-configuration->program the-benchmark-configuration))
  (result-index ((lowest-upper-bound-binary-search
                  (λ (index)
                    (if (max-mutation-index-exceeded? module-to-mutate-path
                                                      index
                                                      the-program)
                        (go-lower)
                        (go-higher))))
                 #:increase-max? #t)))

(define (max-mutation-index-exceeded? module-to-mutate-path mutation-index program)
  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (mutate-module (findf (match-lambda [(mod path _) (paths=? path module-to-mutate-path)])
                          (program->mods program))
                   mutation-index
                   #:in program)
    #f))

(struct mutant (benchmark module index) #:prefab)

(define (extract-mutation mod index program
                          #:normalize [normalize strip-annotations])
  (define mutated-expr (box #f))
  (define mutated-id
    (with-intercepted-logging
      (match-lambda
        [(vector _ _ (and l (list type before after)) _)
         (set-box! mutated-expr l)]
        [other (void)])
      (thunk
       (define-values {_ id}
         (mutate-module mod index #:in program))
       id)
      #:logger mutate-logger
      'info))
  (define-values {mutation-type mutated-expr/annotations-stripped}
    (match (unbox mutated-expr)
      [(list type before after)
       (values type (normalize (list before after)))]
      [else (values #f #f)]))
  (list mutated-id
        mutation-type
        mutated-expr/annotations-stripped))

(define (strip-annotations mutated-expr)
  (match (select-exprs-as-if-untyped (datum->syntax #f mutated-expr))
    [(list stripped-expr _ _)
     (match (syntax->list stripped-expr)
       [(list lone-subexpr)
        #:when (syntax-parse mutated-expr
                 [[name:id {~datum :} T] #t]
                 [else #f])
        (strip-annotations lone-subexpr)]
       [(? list? subexprs)
        (filter-map strip-annotations subexprs)]
       [#f (syntax->datum stripped-expr)])]
    [#f #f]))

(module+ test
  (require ruinit
           "../experiment/mutant-factory-test-helper.rkt"
           "../configurables/configurables.rkt"
           racket/runtime-path)

  (define-runtime-path test-config "../configurables/configs/test.rkt")
  (install-configuration! (simple-form-path test-config))
  (define main-mutation-count 18)
  (define second-mutation-count 3)
  (test-begin/with-env
   #:name max-mutation-index-exceeded?

   (ignore
    (define the-program (make-program main-path (list e-path loop-path))))

   (for/and/test ([i (in-range 3)])
                 (not (max-mutation-index-exceeded? e-path i the-program)))
   (max-mutation-index-exceeded? e-path 3 the-program)

   (not (max-mutation-index-exceeded? main-path 0 the-program))
   (max-mutation-index-exceeded? main-path 1 the-program)

   (for/and/test
    ([rt-main (in-list (list rt-main/t rt-main/ut))]
     [rt-second (in-list (list rt-second/t rt-second/ut))])
    (define rt/program (make-program rt-main (list rt-second)))
    (and/test/message
     [(for/and/test ([i (in-range main-mutation-count)])
                    (extend-test-message
                     (not (max-mutation-index-exceeded? rt-main i rt/program))
                     @~a{(stopped at index @i)}))
      @~a{Not all expected mutations of @rt-main happening}]
     [(max-mutation-index-exceeded? rt-main main-mutation-count rt/program)
      @~a{@rt-main has more mutations than expected}]
     [(for/and/test ([i (in-range second-mutation-count)])
                    (extend-test-message
                     (not (max-mutation-index-exceeded? rt-second i rt/program))
                     @~a{(stopped at index @i)}))
      @~a{@rt-second doesn't have the expected mutations}]
     [(max-mutation-index-exceeded? rt-second second-mutation-count rt/program)
      @~a{@rt-second has more mutations than expected}])))

  ;; equiv to above, but as a stream
  (test-begin/with-env
   #:name in-mutation-indices
   (ignore (define bench (read-benchmark realistic-test-bench)))
   (test-equal? (stream->list (in-mutation-indices "main.rkt" bench))
                (build-list main-mutation-count values))
   (test-equal? (stream->list (in-mutation-indices "second.rkt" bench))
                (build-list second-mutation-count values)))

  (test-begin
    #:name extract-mutation
    (ignore (define test-mod (mod "test.rkt"
                                  #'(module test racket
                                      (#%module-begin
                                       (define x (+ 'a 'b))))))
            (define test-prog (program test-mod empty)))
    (test-equal? (extract-mutation test-mod 1 test-prog)
                 '(x "arithmetic-op-swap" (+ -)))
    (ignore (define test-mod-typed (mod "test.rkt"
                                  #'(module test racket
                                      (#%module-begin
                                       (define x ((ann + T) 'a 'b))))))
            (define test-prog-typed (program test-mod-typed empty)))
    (test-equal? (extract-mutation test-mod-typed 1 test-prog-typed)
                 '(x "arithmetic-op-swap" (+ -)))))
