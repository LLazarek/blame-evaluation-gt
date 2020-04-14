#lang at-exp racket

(provide (contract-out
          [spawn-mutant-runner
           ({benchmark-configuration/c
             module-name?
             natural?
             path-string?}
            {#:timeout/s (or/c #f number?)
             #:memory/gb (or/c #f number?)}
            . ->* .
            procedure?)]
          [in-mutation-indices
           (module-name? benchmark/c . -> . (stream/c natural?))]

          [mutant-error-log (parameter/c path-string?)]
          [default-memory-limit/gb (parameter/c (and/c number? positive?))]
          [default-timeout/s (parameter/c (and/c number? positive?))]))

(require racket/runtime-path
         "../configurations/configure-benchmark.rkt"
         "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt")

(define-runtime-path mutant-runner-path "mutant-runner.rkt")
(define racket-path (find-executable-path (find-system-path 'exec-file)))

(define mutant-error-log (make-parameter "./mutant-errors.txt"))

(define default-memory-limit/gb (make-parameter 3))
(define default-timeout/s (make-parameter (* 2 60)))

(define (spawn-mutant-runner a-benchmark-configuration
                             module-to-mutate-name
                             mutation-index
                             outfile
                             #:timeout/s [timeout/s #f]
                             #:memory/gb [memory/gb #f]
                             #:log-mutation-info? [log-mutation-info? #f])
  (match-define (benchmark-configuration main others* base-dir)
    a-benchmark-configuration)
  (define module-to-mutate
    (resolve-configured-benchmark-module a-benchmark-configuration
                                         module-to-mutate-name))
  (define others
    (map (match-lambda [(? path? p)
                        (path->string p)]
                       [other (~a other)])
         others*
         ;; This isn't necessary: only the base, typed, and untyped dirs have code.
         #;(match base-dir
           [#f others*]
           [dir (append others*
                        ;; base-dir sometimes contains other files, like
                        ;; workload replay histories; skip them.
                        (filter (λ (p) (regexp-match? #rx"\\.rkt$" p))
                                (directory-list dir #:build? #t)))])))
  (call-with-output-file outfile #:mode 'text
    (λ (outfile-port)
      (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
        (λ (error-log-port)
          (match-define (list #f runner-in _ #f runner-ctl)
            (apply process*/ports
                   outfile-port #f error-log-port
                   racket-path
                   (append
                    (if log-mutation-info?
                        (list "-O" "info@mutate")
                        empty)
                    (list "--"
                          mutant-runner-path
                          "-m" main
                          "-o" (~s others)
                          "-M" module-to-mutate
                          "-i" (~a mutation-index)
                          "-t" (~a (or timeout/s
                                       (default-timeout/s)))
                          "-g" (~a (or memory/gb
                                       (default-memory-limit/gb)))))))
          (close-output-port runner-in)
          runner-ctl)))))

(define (resolve-configured-benchmark-module a-benchmark-configuration
                                             a-module-name)
  (findf (path-ends-with a-module-name)
         (list* (benchmark-configuration-main a-benchmark-configuration)
                (benchmark-configuration-others a-benchmark-configuration))))

(define (in-mutation-indices module-to-mutate-name bench)
  (define max-config (make-max-bench-config bench))
  (define the-benchmark-configuration
    (configure-benchmark bench
                         max-config))
  (define module-to-mutate
    (findf (path-ends-with module-to-mutate-name)
           (list*
            (benchmark-configuration-main the-benchmark-configuration)
            (benchmark-configuration-others the-benchmark-configuration))))
  (let next-index ([i 0])
    (if (max-mutation-index-exceeded? module-to-mutate i)
        empty-stream
        (stream-cons i (next-index (add1 i))))))

(define/contract (max-mutation-index-exceeded? module-to-mutate mutation-index)
  (path-to-existant-file?
   natural?
   . -> .
   boolean?)

  ;; `mutate-module` throws if index is too large, so just try
  ;; mutating to see whether or not it throws
  (with-handlers ([mutation-index-exception? (λ _ #t)])
    (mutate-module (read-module module-to-mutate)
                   mutation-index)
    #f))

(module+ test
  (require ruinit
           "mutant-factory-test-helper.rkt")

  (test-begin/with-env
   #:name max-mutation-index-exceeded?

   (for/and/test ([i (in-range 3)])
                 (not (max-mutation-index-exceeded? e-path i)))
   (max-mutation-index-exceeded? e-path 3)

   (not (max-mutation-index-exceeded? main-path 0))
   (max-mutation-index-exceeded? main-path 1)

   (for/and/test
    ([rt-main (in-list (list rt-main/t rt-main/ut))]
     [rt-second (in-list (list rt-second/t rt-second/ut))])
    (and/test/message
     [(for/and/test ([i (in-range 6)])
                    (extend-test-message
                     (not (max-mutation-index-exceeded? rt-main i))
                     @~a{(stopped at index @i)}))
      @~a{Not all expected mutations of @rt-main happening}]
     [(max-mutation-index-exceeded? rt-main 6)
      @~a{@rt-main has more mutations than expected}]
     [(for/and/test ([i (in-range 2)])
                    (extend-test-message
                     (not (max-mutation-index-exceeded? rt-second i))
                     @~a{(stopped at index @i)}))
      @~a{@rt-second doesn't have the expected mutations}]
     [(max-mutation-index-exceeded? rt-second 2)
      @~a{@rt-second has more mutations than expected}])))

  ;; equiv to above, but as a stream
  (test-begin/with-env
   #:name in-mutation-indices
   (ignore (define bench (read-benchmark realistic-test-bench)))
   (test-equal? (stream->list (in-mutation-indices "main.rkt" bench))
                '(0 1 2 3 4 5))
   (test-equal? (stream->list (in-mutation-indices "second.rkt" bench))
                '(0 1))))
