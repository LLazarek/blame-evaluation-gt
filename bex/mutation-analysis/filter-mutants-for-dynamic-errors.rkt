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
         "../runner/mutation-runner.rkt"
         process-queue/priority
         racket/hash
         racket/random)

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
  (unless benchmark
    (raise-argument-error 'filter-for-dynamic-errors
                          "path to a benchmark"
                          benchmark-path))
  (define benchmark-name (benchmark->name benchmark))
  (log-mutant-dynamic-errors-info
   @~a{Starting @benchmark-name analysis})
  (define q
    (for/fold ([q (make-process-queue process-limit
                                      ; (listof mutant?)
                                      empty)])
              ([mutant (in-list mutants-with-type-errors)])
      (match (logged-progress benchmark-name mutant)
        ['? (process-queue-enqueue q
                                   (dynamic-error-checker mutant benchmark
                                                          #:log-progress log-progress!)
                                   2)]
        [#t (process-queue-update-data q (add-to-list mutant))]
        [#f q])))
  (define mutants-with-dynamic-errors
    (process-queue-get-data (process-queue-wait q)))
  (log-mutant-dynamic-errors-info
   @~a{
       @benchmark-name analysis complete: @;
       found @;
       @(length mutants-with-dynamic-errors) / @(length mutants-with-type-errors) @;
       mutants with dynamic errors.
       })
  mutants-with-dynamic-errors)

(define (blamed-is-interesting? blamed-list a-config mutant)
  (match (current-mode)
    [(list _ _ #f) #t]
    [else
     (define blamed-mods-in-program
       (filter (λ (blamed) (hash-has-key? a-config blamed)) blamed-list))
     (define 3-unique-mods-on-stack?
       (>= (length (remove-duplicates blamed-mods-in-program)) 3))
     (log-mutant-dynamic-errors-debug
      @~a{
          @mutant blamed interesting?, blamed: @blamed-mods-in-program : @3-unique-mods-on-stack?
          })
     3-unique-mods-on-stack?]))

(struct dynamic-error ())
(struct other-outcome ())
(define (extract-outcome outfile mutant config)
  (define result (with-handlers ([exn:fail? (const #f)])
                   (file->value outfile)))
  (match result
    [(struct* run-status ([outcome (or 'runtime-error 'blamed)]
                          [blamed blamed-list]
                          [context-stack stack]))
     #:when (blamed-is-interesting? (or blamed-list stack) config mutant)
     (log-mutant-dynamic-errors-info @~a{@mutant => @(run-status-outcome result) : accepted})
     (dynamic-error)]
    [(? run-status?)
     (log-mutant-dynamic-errors-info @~a{@mutant => @(run-status-outcome result) : filtered})
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
  (define max-configuration (make-max-bench-config benchmark))
  (define min-configuration (for/hash ([mod (in-hash-keys max-configuration)])
                              (values mod 'none)))
  (define-values {run-configuration config-path}
    (match (current-mode)
      [(list experiment-config-path lattice-config-id _)
       (values
        (match lattice-config-id
         ['bot min-configuration]
         ['top max-configuration]
         ['top-less-mutated
          (hash-set max-configuration (mutant-module mutant) 'none)])
          experiment-config-path)]))

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
                             config-path)))
    (process-info outfile ctl will))

  (define (will:record-outcome! q info)
    (match (extract-outcome (process-info-data info) mutant run-configuration)
      [(? dynamic-error?)
       (log-progress! benchmark-name mutant #t)
       (process-queue-update-data q (add-to-list mutant))]
      [(? other-outcome?)
       (log-progress! benchmark-name mutant #f)
       q]
      [else q]))

  (mutant-spawner run-configuration
                  will:record-outcome!))

(define (process-queue-update-data q f)
  (process-queue-set-data q (f (process-queue-get-data q))))

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

(define current-mode (make-parameter #f))

(main
 #:arguments ({(hash-table ['summaries-db summaries-db-path]
                           ['restricted-summaries-db restricted-summaries-db-path]
                           ['benchmarks-dir benchmarks-dir]
                           ['progress-log progress-log-path]
                           ['working-dir _]
                           ['process-limit (app string->number process-limit)]
                           ['experiment-config-path experiment-config-path]
                           ['lattice-config (app string->symbol lattice-config-id)]
                           ['interesting filter-for-interesting?])
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
               #:collect {"N" take-latest "1"}]

              [("-c" "--config")
               'experiment-config-path
               ("The experiment config with which to execute mutants for filtering."
                "Mandatory.")
                #:mandatory
                #:collect {"path" take-latest #f}]
              [("-L" "--lattice-config")
               'lattice-config
               ("The lattice config to execute for filtering mutants. One of `bot, top, top-less-mutated`"
                "Mandatory.")
                #:mandatory
                #:collect {"bot|top|top-less-mutated" take-latest #f}]
              [("-I" "--interesting")
               'interesting
               "Filter for mutants with interesting dynamic errors."
                #:record])

 #:check [(db:path-to-db? summaries-db-path)
          @~a{Can't find db at @summaries-db-path}]
 #:check [(member lattice-config-id '(bot top top-less-mutated))
          @~a{@lattice-config-id is not a valid lattice config.}]

 (install-configuration! experiment-config-path)
 (current-mode (list experiment-config-path lattice-config-id filter-for-interesting?))

 (log-mutant-dynamic-errors-info @~a{Mode: @experiment-config-path, @(current-mode)})

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
