#lang at-exp rscript

;; Input: a mutation analysis summaries DB

;; Output: a subset of the input DB containing only those mutants which:
;; 1. raise an error under erasure, and
;; 2. the error blames a component other than the buggy one

(require (prefix-in db: "../db/db.rkt")
         "mutation-analysis-summaries.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "../util/progress-log.rkt"
         "../util/mutant-util.rkt"
         "../process-q/interface.rkt"
         "../process-q/priority.rkt"
         "../runner/mutation-runner.rkt"
         racket/hash
         racket/random)

(define-runtime-paths
  [TR-config "../configurables/configs/TR.rkt"])

(define working-dir (make-parameter "find-mutant-dynamic-errors-scratch"))

(define-logger mutant-dynamic-errors)

;; (listof mutant?)
;; path-to-existant-directory?
;; ->
;; (listof mutant?)
(define (filter-for-dynamic-errors mutants-with-type-errors
                                   benchmark-path
                                   #:log-progress log-progress!
                                   #:logged-progress logged-progress
                                   #:process-limit process-limit)
  (define benchmark (read-benchmark benchmark-path))
  (define benchmark-name (benchmark->name benchmark))
  (log-mutant-dynamic-errors-info
   @~a{Starting @benchmark-name analysis})
  (define q
    (for/fold ([q (make-process-Q process-limit
                                  ; (listof mutant?)
                                  empty)])
              ([mutant (in-list mutants-with-type-errors)])
      (match (logged-progress benchmark-name mutant)
        ['? (process-Q-enq q
                           (dynamic-error-checker mutant benchmark
                                                  #:log-progress log-progress!)
                           2)]
        [#t (process-Q-update-data q (add-to-list mutant))]
        [#f q])))
  (define mutants-with-dynamic-errors
    (process-Q-get-data (process-Q-wait q)))
  (log-mutant-dynamic-errors-info
   @~a{
       @benchmark-name analysis complete: @;
       found @;
       @(length mutants-with-dynamic-errors) / @(length mutants-with-type-errors) @;
       mutants with dynamic errors.
       })
  mutants-with-dynamic-errors)

(define (blamed-is-interesting? blamed-list a-config mutant)
  (define at-least-1-blamed-in-program?
    (> (count (λ (blamed) (hash-has-key? a-config blamed)) blamed-list) 0))
  (define mutated-mod-not-blamed?
    (not (member (mutant-module mutant) blamed-list)))
  (log-mutant-dynamic-errors-debug
   @~a{
       @mutant blamed interesting?: @;
       in-program? @at-least-1-blamed-in-program? | not-mutated-mod? @mutated-mod-not-blamed?
       })
  (and at-least-1-blamed-in-program?
       mutated-mod-not-blamed?))

(struct dynamic-error ())
(struct other-outcome ())
(define (extract-outcome outfile mutant config)
  (define result (with-handlers ([exn:fail? (const #f)])
                   (file->value outfile)))
  (match result
    [(struct* run-status ([outcome (or 'runtime-error 'blamed)]
                          [blamed blamed-list]))
     #:when (blamed-is-interesting? blamed-list config mutant)
     (dynamic-error)]
    [(? run-status?)
     (other-outcome)]
    [else
     (log-mutant-dynamic-errors-error
      @~a{
          Unable to read result from mutant @mutant config @config
          Output file contents:
          "@(file->string outfile)"
          })
     (void)]))

(define (dynamic-error-checker mutant benchmark
                               #:log-progress log-progress!)
  (define benchmark-name (benchmark->name benchmark))
  (define max-config (make-max-bench-config benchmark))
  (define min-config (for/hash ([mod (in-hash-keys max-config)])
                       (values mod 'none)))

  (define ((mutant-spawner config will))
    (define configured-benchmark
      (configure-benchmark benchmark
                           config))
    (define outfile (make-temporary-file @~a{@(benchmark->name benchmark)-~a}
                                         #f
                                         (working-dir)))
    (define ctl
      (parameterize ([mutant-error-log outfile])
        (spawn-mutant-runner configured-benchmark
                             (mutant-module mutant)
                             (mutant-index mutant)
                             outfile
                             TR-config)))
    (process-info outfile ctl will))

  (define (will:record-outcome! q info)
    (match (extract-outcome (process-info-data info) mutant min-config)
      [(? dynamic-error?)
       (log-mutant-dynamic-errors-info @~a{@mutant => #t})
       (log-progress! benchmark-name mutant #t)
       (process-Q-update-data q (add-to-list mutant))]
      [(? other-outcome?)
       (log-mutant-dynamic-errors-info @~a{@mutant => #f})
       (log-progress! benchmark-name mutant #f)
       q]
      [else q]))

  (mutant-spawner min-config
                  will:record-outcome!))

(define (process-Q-update-data q f)
  (process-Q-set-data q (f (process-Q-get-data q))))

;; (hash/c mod-name? summary?) -> (listof mutant?)
(define (summaries->mutants summaries)
  (for*/list ([{mod-name summary} (in-hash summaries)]
              [{mutator indices} (in-hash (summary-valid-indices summary))]
              [index (in-list indices)])
    (mutant #f mod-name index)))

(define ((add-to-list x) l) (cons x l))

;; (hash/c mod-name? summary?) (listof mutant?) -> (hash/c mod-name? summary?)
(define (narrow-summaries summaries mutants)
  (define mutants-by-mod
    (for/fold ([mutants-by-mod (hash)])
              ([mutant (in-list mutants)])
      (hash-update mutants-by-mod
                   (mutant-module mutant)
                   (add-to-list mutant)
                   empty)))
  (for/hash ([{mod-name mutants} (in-hash mutants-by-mod)])
    (define old-mod-summary (hash-ref summaries mod-name #f))
    (define new-indices-by-mutator
      (for/hash ([{mutator indices} (in-hash (summary-valid-indices old-mod-summary))])
        (define indices-as-mutants (map (λ (i) (mutant #f mod-name i)) indices))
        (define intersection-with-mutants
          (set-intersect indices-as-mutants mutants))
        (values mutator
                (map mutant-index intersection-with-mutants))))
    (values mod-name
            (struct-copy summary
                         old-mod-summary
                         [valid-indices new-indices-by-mutator]))))

(main
 #:arguments ({(hash-table ['summaries-db summaries-db-path]
                           ['restricted-summaries-db restricted-summaries-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['progress-log progress-log-path]
                           ['working-dir _]
                           ['process-limit (app string->number process-limit)])
               args}
              #:once-each
              [("-s" "--summaries-db")
               'summaries-db
               ("Input: A database containing mutation analysis summaries for mutants to further"
                "analyze.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--restricted-summaries-db")
               'restricted-summaries-db
               ("Output: A database containing the subset of mutants from the summaries-db which"
                "are satisfactory.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-b" "--benchmarks")
               'benchmarks-dir
               ("Directory containing benchmarks referenced by the summaries-db.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-l" "--progress-log")
               'progress-log
               ("Record progress in the given log file."
                "If it exists and is not empty, resume from the point reached in the log.")
               #:collect {"path" take-latest #f}
               #:mandatory]

              [("-d" "--working-dir")
               'working-dir
               ("Set the working directory for storing temporary data."
                @~a{Default: @(working-dir)})
               #:collect {"path" (set-parameter working-dir) (working-dir)}]
              [("-j" "--process-limit")
               'process-limit
               ("How many parallel processes to use."
                "Default: 1")
               #:collect {"N" take-latest "1"}])

 #:check [(db:path-to-db? summaries-db-path)
          @~a{Can't find db at @summaries-db-path}]

 (define progress
   (match progress-log-path
     [(? file-exists? path)
      (log-mutant-dynamic-errors-info @~a{Pulling progress from log @path})
      (make-hash (file->list path))]
     [else (hash)]))
 (define-values {log-progress!/raw finalize-log!}
   (initialize-progress-log! progress-log-path
                             #:exists 'append))
 (define (log-progress! benchmark-name mutant dynamic-error?)
   (log-progress!/raw (cons (list benchmark-name mutant)
                            dynamic-error?)))
 (define (logged-progress benchmark-name mutant)
   (hash-ref progress
             (list benchmark-name mutant)
             '?))


 (define summaries-db (db:get summaries-db-path))

 (when (directory-exists? (working-dir))
   (log-mutant-dynamic-errors-info @~a{Deleting old working dir at @(working-dir)})
   (delete-directory/files (working-dir)))
 (make-directory* (working-dir))

 (unless (db:path-to-db? restricted-summaries-db-path)
   (log-mutant-dynamic-errors-info @~a{Creating db at @restricted-summaries-db-path})
   (db:new! restricted-summaries-db-path))
 (define restricted-summaries-db (db:get restricted-summaries-db-path))

 (define mutants-with-dynamic-errors-by-benchmark
   (for/hash ([benchmark-name (in-list (db:keys summaries-db))])
     (define benchmark-path (build-path benchmarks-dir benchmark-name))

     (define base-summaries (db:read summaries-db benchmark-name))
     (define benchmark-mutants (summaries->mutants base-summaries))
     (define mutants-with-dynamic-errors
       (filter-for-dynamic-errors benchmark-mutants
                                  benchmark-path
                                  #:log-progress log-progress!
                                  #:logged-progress logged-progress
                                  #:process-limit process-limit))
     (values benchmark-name mutants-with-dynamic-errors)))

 (define restricted-summaries-by-benchmark
   (for/hash ([{benchmark-name mutants-with-dynamic-errors} (in-hash mutants-with-dynamic-errors-by-benchmark)])
     (define base-summaries (db:read summaries-db benchmark-name))
     (values benchmark-name
             (narrow-summaries base-summaries mutants-with-dynamic-errors))))

 (log-mutant-dynamic-errors-info @~a{Writing database})
 (void (db:write! restricted-summaries-db restricted-summaries-by-benchmark))
 (log-mutant-dynamic-errors-info @~a{Cleaning up working dir})
 (delete-directory/files (working-dir))
 (log-mutant-dynamic-errors-info @~a{Analysis complete.}))

(module test racket/base)
