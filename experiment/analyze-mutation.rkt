#lang at-exp racket

(require "../process-q/functional.rkt"
         "../configurations/configure-benchmark.rkt"
         "../mutate/mutate.rkt"
         "../runner/mutation-runner.rkt"
         "../util/path-utils.rkt"
         "../util/read-module.rkt"
         racket/runtime-path)

(define-runtime-path mutant-runner-path "mutant-runner.rkt")
(define racket-path (find-executable-path (find-system-path 'exec-file)))

(define process-limit (make-parameter 3))
(define data-output-dir (make-parameter "./mutant-data"))
(define mutant-error-log (make-parameter "./mutant-errors.txt"))
(define default-memory-limit/gb (make-parameter 3))
(define default-timeout/s (make-parameter (* 2 60)))

(define module-name? string?)

(define/contract (spawn-mutant-runner a-benchmark-configuration
                                      module-to-mutate-name
                                      mutation-index
                                      outfile
                                      #:timeout/s [timeout/s #f]
                                      #:memory/gb [memory/gb #f])
  ({benchmark-configuration/c
   module-name?
   natural?
   path-string?}
   {#:timeout/s (or/c #f number?)
    #:memory/gb (or/c #f number?)}
   . ->* .
   procedure?)

  (match-define (benchmark-configuration main others* base-dir)
    a-benchmark-configuration)
  (define module-to-mutate
    (resolve-configured-benchmark-module a-benchmark-configuration
                                         module-to-mutate-name))
  (define others
    (map (match-lambda [(? path? p)
                        (path->string p)]
                       [other (~a other)])
         (match base-dir
           [#f others*]
           [dir (append others*
                        (directory-list dir #:build? #t))])))
  (call-with-output-file outfile #:mode 'text
    (λ (outfile-port)
      (call-with-output-file (mutant-error-log) #:mode 'text #:exists 'append
        (λ (error-log-port)
          (match-define (list #f runner-in _ #f runner-ctl)
            (process*/ports
             outfile-port #f error-log-port
             racket-path "-O" "info@mutate" "--"
             mutant-runner-path
             "-m" main
             "-o" (~s others)
             "-M" module-to-mutate
             "-i" (~a mutation-index)
             "-t" (~a (if timeout/s
                          timeout/s
                          (default-timeout/s)))
             "-g" (~a (if memory/gb memory/gb (default-memory-limit/gb)))))
          (close-output-port runner-in)
          runner-ctl)))))

(define (resolve-configured-benchmark-module a-benchmark-configuration a-module-name)
  (findf (path-ends-with a-module-name)
         (list* (benchmark-configuration-main a-benchmark-configuration)
                (benchmark-configuration-others a-benchmark-configuration))))


(define/contract (mutation-info-for-all-mutants bench proc-limit)
  (benchmark/c natural? . -> . any)

  (define mutatable-module-names (benchmark->mutatable-modules bench))
  (define max-config (make-max-bench-config bench))

  (unless (directory-exists? (data-output-dir))
    (make-directory (data-output-dir)))

  (define q
    (for/fold ([q (make-process-Q proc-limit
                                  ; (hash/c name? (hash/c 'type-error natural? 'total natural?))
                                  (hash))])
              ([module-to-mutate-name mutatable-module-names]
               [i-1 (in-naturals)]
               #:when #t
               [index (in-mutation-indices module-to-mutate-name
                                           bench)]
               [i-2 (in-naturals)])
      (enq-process q
                   (λ _ (mutation-info-for bench
                                           module-to-mutate-name
                                           index
                                           (~a i-1 '- i-2))))))

  (define q* (proc-Q-wait q))
  (pretty-display (proc-Q-data q*)))

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

(define (mutation-info-for bench
                           module-to-mutate-name
                           index
                           id)
  (define max-config (make-max-bench-config bench))
  (define the-benchmark-configuration
    (configure-benchmark bench
                         max-config))
  (define outfile (build-path (data-output-dir)
                              (format "~a_index~a_~a.rktd"
                                      module-to-mutate-name
                                      index
                                      id)))
  (define ctl (spawn-mutant-runner the-benchmark-configuration
                                   module-to-mutate-name
                                   index
                                   outfile))
  (define (will:record-type-error q* info)
    (define-values {type-error? mutation-type}
      (extract-mutation-type-and-result outfile))
    (define (update-inner-hash h)
      (hash-update h mutation-type add1 0))
    (define (update h)
      (define h* (hash-update h "total" add1 0))
      (hash-update h*
                   (if type-error? "success" "fail")
                   update-inner-hash
                   (hash)))
    (proc-Q-data-set q* (update (proc-Q-data q*))))
  (process-info #f #f 0 #f
                ctl
                will:record-type-error))

(define (extract-mutation-type-and-result f)
  (define output-regexp
    (pregexp @~a{
                 mutate: type: (\S+)
                 mutate: Mutating.+
                 #s\(run-status "[^"]+" \d+ \S+ (\S+)
@; close "{
                 }))
  #;(displayln (list output-regexp
                   (file->string f)
                   (regexp-match output-regexp
                                 (file->string f))))
  (match (file->string f)
    [(regexp output-regexp
             (list _ mutation-type outcome))
     (define type-error? (string=? outcome "type-error"))
     (values type-error? mutation-type)]
    [other-contents
     (raise-user-error @~a{
                           Unable to match against file contents:
                           @other-contents
                           })]))

(module+ main
  (require racket/cmdline)
  (define bench-to-run (make-parameter #f))
  (command-line
   #:once-each
   [("-b" "--benchmark")
    path
    "Path to benchmark to run."
    (bench-to-run path)]
   [("-o" "--output-dir")
    dir
    "Data output directory."
    (data-output-dir dir)]
   [("-n" "--process-limit")
    n
    "Number of processes to have running at once."
    (process-limit (string->number n))]
   [("-e" "--error-log")
    path
    "File to which to append mutant errors. Default: ./mutant-errors.txt"
    (mutant-error-log path)])
  (unless (bench-to-run)
    (error 'mutant-factory "Must provide benchmark to run."))
  (when (directory-exists? (data-output-dir))
    (eprintf "Output directory ~a already exists; remove? (y/n): "
             (data-output-dir))
    (match (read)
      [(or 'y 'yes) (delete-directory/files (data-output-dir))]
      [_ (eprintf "Not deleted.~n")]))
  (mutation-info-for-all-mutants (read-benchmark (bench-to-run))
                                 (process-limit)))
