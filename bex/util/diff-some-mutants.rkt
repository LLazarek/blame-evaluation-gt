#lang at-exp rscript

(require "debug-mutant.rkt"
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../configurables/configurables.rkt"
         (prefix-in db: "../db/db.rkt")
         racket/random)

(define-runtime-paths
  [temp-dir "temp-benchmark-mutants"]
  [summaries "../mutation-analysis/summaries/default.rktdb"]
  [benchmarks-dir "../../gtp-benchmarks/benchmarks"]
  [outdir "mutant-diffs"])

(define summaries-db (db:get summaries))

(define (pick-mutants bench-name n)
  (define summaries-by-mod (db:read summaries-db bench-name))
  (define mods (hash-keys summaries-by-mod))
  (for/list ([i (in-range n)])
    (define mod (random-ref mods))
    (define summary (hash-ref summaries-by-mod mod))
    (define mutants-by-mutator (summary-valid-indices summary))
    (define mutators (summary-triggered-mutators summary))
    (define picked-mutator (random-ref mutators))
    (list bench-name
          mod
          (random-ref (hash-ref mutants-by-mutator picked-mutator)))))

(define ((diff-mutant real-diff?) a-mutant)
  (match-define (list bench-name mod index) a-mutant)
  (make-directory* outdir)
  (define outfile (build-path outdir
                              (~a bench-name "-" mod "-" index ".diff")))
  (cond [real-diff?
         (when (directory-exists? temp-dir)
           (delete-directory/files temp-dir))
         (define thd (thread (thunk
                              (debug-mutant bench-name
                                            mod
                                            index
                                            'none
                                            #:run? #t
                                            #:write-modules-to temp-dir))))
         (define dumped-mod-path (build-path temp-dir mod))
         (let wait ()
           (sleep 1)
           (if (file-exists? dumped-mod-path)
               (void)
               (wait)))
         (sleep 0.1)
         (kill-thread thd)
         (define real-mod-path (build-path benchmarks-dir
                                           bench-name
                                           "untyped"
                                           mod))
         (system @~a{diff -w @real-mod-path @dumped-mod-path > @outfile})
         (delete-directory/files temp-dir)]

        [else
         (with-output-to-file outfile
           #:exists 'replace
           (thunk (debug-mutant bench-name
                                mod
                                index
                                'none
                                #:diff-mutant? #t)))]))

(main
 #:arguments {[(hash-table ['n (app string->number n)]
                           ['real-diff? real-diff?]
                           ['config config-path])
               benchmark-names]
              #:once-each
              [("-c" "--config")
               'config
               "Path to the configuration to use for mutation."
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-n" "--number")
               'n
               "How many mutants to pick per benchmark."
               #:mandatory
               #:collect ["N" take-latest #f]]
              [("-d" "--diff")
               'real-diff?
               ("Generate reall diffs that can be applied by `diff`."
                "Default: generate diffs that are human-readable, but don't"
                "correspond directly to the source text (so can't be applied).")
               #:record]
              #:args benchmark-names}

 (install-configuration! config-path)
 (define mutants
   (apply append
          (for/list ([bench (in-list benchmark-names)])
            (pick-mutants bench n))))
 (for-each (diff-mutant real-diff?) mutants))

(module test racket)
