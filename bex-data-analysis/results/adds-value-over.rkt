#lang at-exp rscript

(provide adds-value-over
         adds-value-over->%
         simple-memoize)

(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         (only-in pict vc-append text)
         pict-util/file
         bex/mutation-analysis/mutation-analysis-summaries
         bex/experiment/blame-trail-data
         bex/configurables/configurables

         "plot-common.rkt"
         "read-data.rkt")

(define (string->value s)
  (with-input-from-string s read))
;; comparison? := (list*/c string? (non-empty-listof string?))
;; (listof string?) -> (listof comparison?)
(define (parse-comparisons strs)
  (map string->value strs))

(define (simple-memoize f #:on-disk [cache-path #f])
  (let ([cache (if (and cache-path (file-exists? cache-path))
                   (make-hash (hash->list (file->value cache-path)))
                   (make-hash))])
    (λ args
      (cond [(hash-ref cache args #f) => values]
            [else
             (define result (apply f args))
             (hash-set! cache args result)
             (when cache-path (write-to-file cache cache-path #:exists 'replace))
             result]))))
(define blame-trails-by-mutator-for
  (simple-memoize
   (λ (data-dir mutant-mutators)
     (add-missing-active-mutators
      (read-blame-trails-by-mutator/across-all-benchmarks data-dir mutant-mutators)))))

;; (hash/c mutator-name? (listof blame-trail?)) ...+
;; . -> .
;; (hash/c mutant? boolean?)
(define (adds-value-over top-mode-blame-trails-by-mutator . other-modes-blame-trails-by-mutator)
  #;(listof (hash/c (or/c "always" "sometimes" "never")
                    (listof listof-blame-trails-for-same-mutant?)))
  (define blame-reliability-breakdowns
    (for/list ([blame-trails-by-mutator (in-list (cons top-mode-blame-trails-by-mutator
                                                       other-modes-blame-trails-by-mutator))])
      (define all-trails (append* (hash-values blame-trails-by-mutator)))
      (blame-reliability-breakdown-for "yes" (hash "yes" all-trails))))
  (define (blame-reliability-breakdown->mutant-reliabilities breakdown)
    (for*/hash ([{reliability bts-for-each-mutant} (in-hash breakdown)]
                [bts-for-a-mutant (in-list bts-for-each-mutant)])
      (define a-bt-for-a-mutant (first bts-for-a-mutant))
      (define a-mutant (blame-trail-mutant-id a-bt-for-a-mutant))
      (values a-mutant reliability)))
  (match-define (list* top-mode-mutant-reliabilities other-mode-mutant-reliabilities*)
    (map blame-reliability-breakdown->mutant-reliabilities
         blame-reliability-breakdowns))
  (define top-avo-others?-by-mutant
    (for/hash ([{mutant top-reliability-class} (in-hash top-mode-mutant-reliabilities)])
      (define other-reliability-classes
        (for/list ([other-mutant-reliabilities (in-list other-mode-mutant-reliabilities*)])
          (hash-ref other-mutant-reliabilities
                    mutant
                    (thunk
                     (raise-user-error
                      'adds-value-over-%
                      @~a{
                          Mutant sets are not the same across all modes.
                          Top mode has mutant: @mutant
                          while another mode does not.
                          })))))
      (values mutant
              (apply reliability-class>? top-reliability-class other-reliability-classes))))
  top-avo-others?-by-mutant)

(define (adds-value-over->% avo?-by-mutant)
  (define mutant-count (hash-count avo?-by-mutant))
  (define mutants-that-avo (count values (hash-values avo?-by-mutant)))
  (/ mutants-that-avo mutant-count))

;; (listof (list/c comparison? real?)) -> pict?
(define (plot-avo-comparisons comparison-%s)
  (define (comparison->name comparison)
    (match-define (list* top others) comparison)
    (~a top " avo " (apply ~a (add-between others "/"))))
  (define (comparison-%-with-name comparison-%)
    (list (comparison->name (first comparison-%))
          (second comparison-%)))
  (define (comparison-%-annotation comparison% index)
    (define % (second comparison%))
    (point-label (list (+ index 0.5) %)
                 (~r % #:precision 3)
                 #:point-size 0
                 #:anchor 'bottom))
  (plot-pict (list (discrete-histogram (map comparison-%-with-name comparison-%s))
                   (for/list ([comparison% (in-list comparison-%s)]
                              [i (in-naturals)])
                     (comparison-%-annotation comparison% i)))
             #:y-max 1
             #:y-label (~a "Percentage of all mutants")))

(define (reliability-class>? x . others)
  (for/and ([other (in-list others)])
    (match* {x other}
      [{"always" (not "always")} #t]
      [{"sometimes" "never"} #t]
      [{_ _} #f])))

(main
 #:arguments {[(hash-table ['data-dir data-dir]
                           ['out-dir  out-dir]
                           ['name     name]
                           ['config   config-path]
                           ['dump-path dump-path]
                           ['comparisons (app parse-comparisons comparisons)])
               args]
              #:once-each
              [("-s" "--mutant-summaries")
               'summaries-db
               ("Path to the db containing summaries of the mutants in the data."
                @~a{Default: @(mutation-analysis-summaries-db)})
               #:collect ["path"
                          (set-parameter mutation-analysis-summaries-db)
                          (mutation-analysis-summaries-db)]]
              [("-o" "--out-dir")
               'out-dir
               ("Directory in which to place plots."
                "Default: .")
               #:collect ["path" take-latest "."]]
              [("-n" "--name")
               'name
               ("Name for the plots. This becomes the title of the plots,"
                "as well as a prefix of the plot file names.")
               #:collect ["name" take-latest ""]]
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              [("-D" "--dump-data")
               'dump-path
               "Dump data for generating the plot to the given file."
               #:collect ["path" take-latest #f]]
              [("-d" "--data-dir")
               'data-dir
               ("Path to the directory containing data sub-directories for each"
                "mode named by --compare.")
               #:collect ["path" take-latest #f]
               #:mandatory]
              #:multi
              [("-C" "--compare")
               'comparisons
               ("Adds-value-over comparisons to make. Each instance of this flag represents "
                "a spec to plot '<top-mode> adds value over (∧ <other-mode> ...)'."
                "Every such AVO spec will become a bar in the resulting chart.")
               #:collect ["(top-mode other-mode ...+)" cons empty]]}

 (install-configuration! config-path)

 (define dump-port (and dump-path
                        (open-output-file dump-path #:exists 'replace)))

 (define mutant-mutators
   (read-mutants-by-mutator (mutation-analysis-summaries-db)))

 (define (mode-name->blame-trails-by-mutator mode-name)
   (define mode-data-dir (build-path data-dir mode-name))
   (add-missing-active-mutators
    (read-blame-trails-by-mutator/across-all-benchmarks mode-data-dir mutant-mutators)))
 (define comparison-%s
   (for/list ([comparison (in-list comparisons)])
     (match-define (list* top-blame-trails-by-mutator other-modes-blame-trails-by-mutator)
       (map mode-name->blame-trails-by-mutator comparison))
     (define avo?-by-mutant (apply adds-value-over
                                   top-blame-trails-by-mutator
                                   other-modes-blame-trails-by-mutator))
     (when dump-port
       (displayln comparison dump-port)
       (pretty-write avo?-by-mutant dump-port)
       (newline dump-port))
     (list comparison
           (adds-value-over->% avo?-by-mutant))))

 (when dump-port (close-output-port dump-port))

 (define plot-pict (plot-avo-comparisons comparison-%s))

 (make-directory* out-dir)
 (define with-title
   (vc-append 20
              (text name)
              plot-pict))
 (pict->png! with-title (build-path out-dir (~a name ".png"))))
