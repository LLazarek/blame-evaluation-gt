#lang at-exp rscript

;; Check if every type error for a given set of mutants identifies either of the following
;; *anywhere* in its message:
;; 1. The right module, or
;; 2. The right identifier (i.e. the mutated identifier)

(require (prefix-in db: "../db/db.rkt")
         "../process-q/interface.rkt"
         "../process-q/functional.rkt"
         "../configurations/configure-benchmark.rkt"
         (only-in "../runner/mutation-runner.rkt"
                  mutate-module
                  current-mutated-program-exn-recordor)
         "../runner/mutation-runner-data.rkt"
         "../util/path-utils.rkt"
         "../util/program.rkt"
         "../util/mutant-util.rkt"
         "../configurables/configurables.rkt"
         racket/logging)

(define-runtime-paths
  [TR-config "../configurables/configs/TR.rkt"]
  [benchmarks-dir "../../gtp-benchmarks/benchmarks"])

(define-logger type-error-verification)

(define ((add-to-list v) l) (cons v l))

(define (launch-type-error-message-checker q mutant benchmark)
  (define configured-benchmark
    (configure-benchmark benchmark
                         (make-max-bench-config benchmark)))
  (process-Q-enq
   q
   (thunk
    (define (will current-q info)
      (match-define (list outcome-file output-file) (process-info-data info))
      (match (file->value outcome-file)
        [(struct* run-status ([outcome 'type-error])) (void)]
        [else (log-type-error-verification-error @~a{@mutant doesn't produce a type error!?})])
      (define rs (file->value outcome-file))
      (define output (file->string output-file))
      (cond [(or (eof-object? rs) (eof-object? output))
             (log-type-error-verification-info @~a{Restarting failed @mutant})
             (launch-type-error-message-checker current-q mutant benchmark)]
            [else
             (define useful? (type-error-message-useful? mutant
                                                         configured-benchmark
                                                         rs
                                                         output))
             (log-type-error-verification-debug @~a{Is @mutant type error useful? @useful?})
             (delete-file outcome-file)
             (delete-file output-file)
             (process-Q-set-data current-q
                                 (hash-update (process-Q-get-data current-q)
                                              useful?
                                              (add-to-list mutant)
                                              empty))]))
    (define outcome-file
      (make-temporary-file @~a{@(benchmark->name benchmark)-~a}
                           #f
                           (working-dir)))
    (define output-file
      (make-temporary-file @~a{@(benchmark->name benchmark)-message-~a}
                           #f
                           (working-dir)))
    (define ctl
      (parameterize ([mutant-error-log output-file])
        (spawn-mutant-runner configured-benchmark
                             (mutant-module mutant)
                             (mutant-index mutant)
                             outcome-file
                             TR-config
                             #:save-output output-file)))
    (log-type-error-verification-info @~a{@mutant checker launched})
    (process-info (list outcome-file output-file) ctl will))))

(define (type-error-message-useful? mutant configured-benchmark run-status error-message)
  (log-type-error-verification-debug @~a{@mutant type error message: "@error-message"})
  (when (string=? error-message "")
    (exit 1))
  (define mutant-mutated-id (mutant->mutated-id mutant configured-benchmark))
  (cond [(string-contains? (~a (run-status-blamed run-status))
                           (mutant-module mutant))
         'direct-mod-ref]
        [(string-contains? error-message (mutant-module mutant))
         'mod-ref-in-msg]
        [(string-contains? error-message (~a mutant-mutated-id))
         'id-ref-in-msg]
        [else #f]))

(define (mutant->mutated-id mutant configured-benchmark)
  (define program (make-program (benchmark-configuration-main configured-benchmark)
                                (benchmark-configuration-others configured-benchmark)))
  (define mutated-mod (pick-file-by-name (program->mods program)
                                         (mutant-module mutant)
                                         #:key mod-path))
  (define-values {_ id} (mutate-module mutated-mod
                                       (mutant-index mutant)
                                       #:in program))
  (log-type-error-verification-debug @~a{@mutant mutated id: @id})
  id)

(define working-dir (make-parameter "verify-type-error-scratch"))
(main
 #:arguments ([(hash-table ['mutant-samples-db-path mutant-samples-db-path]
                           ['cpus (app string->number cpus)]
                           ['working-dir _])
               args]
              #:once-each
              [("-s" "--mutant-samples-db")
               'mutant-samples-db-path
               "Path to a database listing the mutants to check."
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-j" "--cpus")
               'cpus
               "Number of CPUs to use for the search. Default: 1"
               #:collect {"N" take-latest "1"}]
              [("-w" "--working-dir")
               'working-dir
               ("Set the working directory for storing temporary data."
                @~a{Default: @(working-dir)})
               #:collect {"path" (set-parameter working-dir) (working-dir)}])
 #:check [(db:path-to-db? mutant-samples-db-path)
          @~a{Can't find db at @mutant-samples-db-path}]

 (install-configuration! TR-config)

 (define mutant-samples-db (db:get mutant-samples-db-path))
 (define q
   (process-Q-wait
    (for*/fold ([q (make-process-Q cpus (hash))])
               ([benchmark-name (in-list (db:keys mutant-samples-db))]
                [benchmark (in-value (read-benchmark (build-path benchmarks-dir benchmark-name)))]
                [{mod indices} (in-hash (db:read mutant-samples-db benchmark-name))]
                [index (in-list indices)])
      (launch-type-error-message-checker q
                                         (mutant benchmark-name mod index)
                                         benchmark))))
 (log-type-error-verification-info
  @~a{
      Verification complete.
      Mutant type error information breakdown:
      @pretty-format[(process-Q-get-data q)]
      }))


