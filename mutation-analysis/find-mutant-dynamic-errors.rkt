#lang at-exp rscript

;; Input: a mutation analysis summaries DB

;; Output: a subset of the input DB, and a new DB with BT root samples for every
;; mutant in the former DB

(require (prefix-in db: "../db/db.rkt")
         "mutation-analysis-summaries.rkt"
         "../configurations/config.rkt"
         "../configurations/configure-benchmark.rkt"
         "../configurables/configurables.rkt"
         "../util/progress-log.rkt"
         "../util/mutant-util.rkt"
         "../process-q/interface.rkt"
         "../process-q/functional.rkt"
         "../runner/mutation-runner.rkt"
         racket/hash
         racket/random)

(define default-bt-root-sample-size 96)

(define dynamic-error-default-search-size 1000)
(define dynamic-error-default-cutoff 100)

(define working-dir (make-parameter "find-mutant-dynamic-errors-scratch"))

(define-logger mutant-dynamic-errors)

;; (listof mutant?)
;; path-to-existant-directory?
;; ->
;; (hash/c mutant? (listof config/c))
(define (filter-for-dynamic-errors mutants-with-type-errors
                                   benchmark-path
                                   #:search-size search-size
                                   #:minimum-cutoff cutoff
                                   #:log-progress log-progress!
                                   #:logged-progress logged-progress
                                   #:process-limit process-limit)
  (define roots-by-mutant
    (for/hash ([mutant (in-list mutants-with-type-errors)])
      (values mutant
              (search-for-mutant-dynamic-errors benchmark-path
                                                mutant
                                                search-size
                                                #:log-progress log-progress!
                                                #:logged-progress logged-progress
                                                #:process-limit process-limit))))
  (define roots-by-mutant/passing-minimum-threshold
    (for/hash ([{mutant roots} (in-hash roots-by-mutant)]
               #:when (>= (length roots) cutoff))
      (values mutant roots)))
  roots-by-mutant/passing-minimum-threshold)

(define (process-Q-update-data q f)
  (process-Q-set-data q (f (process-Q-get-data q))))

(struct dynamic-error ())
(struct other-outcome ())
;; benchmark-path? mutant? -> (listof config/c)
(define (search-for-mutant-dynamic-errors benchmark-path
                                          mutant
                                          search-size
                                          #:log-progress log-progress!
                                          #:logged-progress logged-progress
                                          #:process-limit process-limit)
  ;; parallelize here
  ;; treat each mutant as a synchronization point
  (log-mutant-dynamic-errors-info
   @~a{Starting configuration lattice search for @benchmark-path @mutant})

  (define benchmark (read-benchmark benchmark-path))
  (define max-config (make-max-bench-config benchmark))
  (define found-enough-configs? (box #f))

  (define (extract-outcome outfile config)
    (define result (with-handlers ([exn:fail? (const #f)])
                     (file->value outfile)))
    (match result
      [(struct* run-status ([outcome (or 'runtime-error 'blamed)]
                            [blamed blamed-list]))
       #:when (> (count (λ (blamed) (hash-has-key? config blamed)) blamed-list) 0)
       (dynamic-error)]
      [(? run-status?)
       (other-outcome)]
      [else
       (log-mutant-dynamic-errors-error
        @~a{
            Unable to read result from mutant @mutant config @config
            Output file contents:
            "@(file->string outfile)"
            })]))

  (define ((check-config-for-dynamic-error config search-index))
    (log-mutant-dynamic-errors-debug @~a{@mutant config @search-index = @~v[config]})
    (define (will:record-config-outcome! q info)
      (define outfile (process-info-data info))
      (match (extract-outcome outfile config)
        [(? dynamic-error?)
         (log-mutant-dynamic-errors-info @~a{@mutant config @search-index => #t})
         (log-progress! benchmark-path mutant search-index config #t)
         (process-Q-update-data (add-to-list config))]
        [(? other-outcome?)
         (log-mutant-dynamic-errors-info @~a{@mutant config @search-index => #f})
         (log-progress! benchmark-path mutant search-index config #f)
         q]
        [else q]))

    (cond [(unbox found-enough-configs?)
           noop-process-info]
          [else
           (define configured-benchmark
             (configure-benchmark benchmark
                                  config))
           (define outfile (make-temporary-file @~a{@(benchmark->name benchmark)-@|search-index|-~a}
                                                #f
                                                (working-dir)))
           (define ctl
             (parameterize ([mutant-error-log outfile])
               (spawn-mutant-runner configured-benchmark
                                    (mutant-module mutant)
                                    (mutant-index mutant)
                                    outfile
                                    (current-configuration-path))))
           (process-info outfile
                         ctl
                         will:record-config-outcome!)]))

  (define (mutant-passes-litmus-test?)
    (define config:all-max-but-mutated
      (for/hash ([{mod _} (in-hash max-config)])
        (values mod
                (if (equal? mod (mutant-module mutant))
                    'none
                    'types))))
    (match-define (process-info outfile ctl _)
      ((check-config-for-dynamic-error config:all-max-but-mutated 'litmus-test)))
    (ctl 'wait)
    (dynamic-error? (extract-outcome outfile config:all-max-but-mutated)))

  (cond [(mutant-passes-litmus-test?)
         (define q
           (for/fold ([q (make-process-Q process-limit
                                         ; (listof config/c)
                                         empty)])
                     ([config (in-list (sample-configs-with-replacement max-config search-size))]
                      [index (in-naturals)])
             (match (logged-progress benchmark-path mutant index)
               [#f (process-Q-enq q
                                  (check-config-for-dynamic-error config index))]
               [(list config #t)
                (process-Q-update-data (add-to-list config))]
               [(list config #f)
                q])))
         (define empty-q (process-Q-wait q))
         (define configs (process-Q-get-data empty-q))
         (log-mutant-dynamic-errors-info
          @~a{Search for @benchmark-path @mutant complete. Found @(length configs) dynamic errors.})
         configs]
        [else
         (log-mutant-dynamic-errors-info
          @~a{@benchmark-path @mutant fails litmus test. Skipping it.})
         empty]))

(define noop-process-info (process-info #f
                                        (match-lambda ['status 'done-ok]
                                                      ['wait #t]
                                                      [_ (void)])
                                        (λ (q info) q)))

(define (sample-configs-with-replacement max-config search-size)
  (define (random-config . _)
    (for/hash ([mod (in-hash-keys max-config)])
      (values mod (random-ref '(none types)))))
  (build-list search-size random-config))



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
        (define indices-as-mutants (map (λ (i) (mutant #f mod-name i))))
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
                           ['dynamic-error-configs-db dynamic-error-configs-db-path]
                           ['config _]
                           ['benchmarks-dir benchmarks-dir]
                           ['progress-log progress-log-path]
                           ['search-size (app string->number search-size)]
                           ['search-cutoff (app string->number search-cutoff)]
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
              [("-r" "--restricted-summaries-db")
               'restricted-summaries-db
               ("Output: A database containing the subset of mutants from the summaries-db which"
                "are satisfactory.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-o" "--dynamic-error-configs-db")
               'dynamic-error-configs-db
               ("Output: A database to populate with blame trail roots for every satisfactory"
                "mutant.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-c" "--config")
               'config
               ("The config with which to identify blame trail roots.")
               #:collect {"path" (set-parameter current-configuration-path) #f}
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

              [("-S" "--search-size")
               'search-size
               ("Maximum number of configurations to search before giving up on a mutant."
                @~a{Default: @dynamic-error-default-search-size})
               #:collect {"N" take-latest (~a dynamic-error-default-search-size)}]
              [("-C" "--search-cutoff")
               'search-cutoff
               ("Target number of configurations to find for each mutant."
                @~a{Default: @dynamic-error-default-cutoff})
               #:collect {"N" take-latest (~a dynamic-error-default-cutoff)}]
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
 (define (log-progress! benchmark-path mutant search-index config dynamic-error?)
   (log-progress!/raw (cons (list (~a benchmark-path)
                                  mutant
                                  search-index)
                            (list config
                                  dynamic-error?))))
 (define (logged-progress benchmark-path mutant search-index)
   (hash-ref progress
             (list (~a benchmark-path)
                   mutant
                   search-index)
             #f))


 (define summaries-db (db:get summaries-db-path))

 (when (directory-exists? (working-dir))
   (log-mutant-dynamic-errors-info @~a{Deleting old working dir at @(working-dir)})
   (delete-directory/files (working-dir)))
 (make-directory* (working-dir))

 (unless (db:path-to-db? dynamic-error-configs-db-path)
   (log-mutant-dynamic-errors-info @~a{Creating db at @dynamic-error-configs-db-path})
   (db:new! dynamic-error-configs-db-path))
 (define dynamic-error-configs-db (db:get dynamic-error-configs-db-path))

 (unless (db:path-to-db? restricted-summaries-db-path)
   (log-mutant-dynamic-errors-info @~a{Creating db at @restricted-summaries-db-path})
   (db:new! restricted-summaries-db-path))
 (define restricted-summaries-db (db:get restricted-summaries-db-path))

 (define mutant-dynamic-errors-by-benchmark
   (for/hash ([benchmark-name (in-list (db:keys summaries-db))])
     (define benchmark-path (build-path benchmarks-dir benchmark-name))

     (define base-summaries (db:read summaries-db benchmark-name))
     (define benchmark-mutants (summaries->mutants base-summaries))
     (define mutant-dynamic-errors (filter-for-dynamic-errors benchmark-mutants
                                                              benchmark-path
                                                              #:search-size search-size
                                                              #:minimum-cutoff search-cutoff
                                                              #:log-progress log-progress!
                                                              #:logged-progress logged-progress
                                                              #:process-limit process-limit))
     (values benchmark-name mutant-dynamic-errors)))

 (define restricted-summaries-by-benchmark
   (for/hash ([{benchmark-name mutant-dynamic-errors} (in-hash mutant-dynamic-errors-by-benchmark)])
     (define mutants-with-dynamic-errors (hash-keys mutant-dynamic-errors))
     (define base-summaries (db:read summaries-db benchmark-name))
     (values benchmark-name
             (narrow-summaries base-summaries mutants-with-dynamic-errors))))

 (log-mutant-dynamic-errors-info @~a{Writing databases})
 (void (db:write! restricted-summaries-db restricted-summaries-by-benchmark))
 (void (db:write! dynamic-error-configs-db mutant-dynamic-errors-by-benchmark))
 (log-mutant-dynamic-errors-info @~a{Cleaning up working dir})
 (delete-directory/files (working-dir))
 (log-mutant-dynamic-errors-info @~a{Analysis complete.}))

(module test racket/base)
