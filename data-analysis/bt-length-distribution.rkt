#lang at-exp rscript

(require plot
         plot-util
         plot-util/quick/infer
         (except-in pict-util line)
         (only-in pict vc-append text)
         pict-util/file
         "../mutation-analysis/mutation-analysis-summaries.rkt"
         "../experiment/blame-trail-data.rkt"
         (prefix-in db: "../db/db.rkt")
         "../configurables/configurables.rkt"

         "read-data.rkt")

(define pict? any/c)
(define/contract (bt-length-distribution-plot-for mutator data
                                                  #:normalize? normalize?)
  (mutator-name?
   (and/c (hash/c mutator-name? (listof blame-trail-summary?))
          hash-with-all-active-mutator-names?)
   #:normalize? boolean?
   . -> .
   pict?)

  (define (trail-length trail)
    (define base-trail-length
      (sub1 (length (blame-trail-summary-mutants trail))))
    (cond [(and normalize?
                (> base-trail-length 0))
           (define ordered-mutants
             (sort (blame-trail-summary-mutants trail)
                   <
                   #:key mutant-summary-id))
           (define first-mutant-config
             (mutant-summary-config (first ordered-mutants)))
           (define last-mutant-config
             (mutant-summary-config (last ordered-mutants)))
           (define number-of-components-typed
             (for/sum ([{mod level} (in-hash first-mutant-config)]
                       #:when (not (equal? (hash-ref last-mutant-config mod)
                                           level)))
               1))
           number-of-components-typed]
          [else base-trail-length]))

  (define trails (hash-ref data mutator))
  (define trail-lengths
    (map trail-length trails))
  (define grouped-lengths
    (group-by identity trail-lengths))
  (define counts
    (for/list ([group (in-list grouped-lengths)])
      (list (first group) (/ (length group) (length trails)))))
  (define counts/0-if-empty
    (if (empty? counts)
        '((0 0))
        counts))
  (plot-pict (discrete-histogram counts/0-if-empty)
             #:x-min 0
             #:y-min 0
             #:x-label (~a "Blame trail length"
                           (if normalize? " (normalized)" ""))
             #:y-label "Percent"
             #:title mutator))

(define (add-missing-active-mutators data)
  todo
  (configured:active-mutator-names))

(main
 #:arguments {[(hash-table ['data-dir data-dir]
                           ['out-dir  out-dir]
                           ['name     name]
                           ['config   config-path]
                           _ ...)
               args]
              #:once-each
              [("-d" "--data-dir")
               'data-dir
               ("Path to the directory containing data sub-directories for each"
                "mode.")
               #:collect ["path" take-latest #f]
               #:mandatory]
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
               #:mandatory]}

 (install-configuration! config-path)

 (define mutant-mutators
   (read-mutants-by-mutator (mutation-analysis-summaries-db)))

 (define data
   (add-missing-active-mutators (read-data data-dir mutant-mutators)))

 (define all-mutator-names (mutator-names data))

 (match-define (list distributions-by-mutator/normalized
                     distributions-by-mutator/unnormalized)
   (for/list ([normalized? (in-list '(#t #f))])
     (for/hash ([mutator (in-list all-mutator-names)])
       (values mutator
               (bt-length-distribution-plot-for mutator
                                                data
                                                #:normalize? normalized?)))))

 (make-directory* out-dir)
 (define (write-distributions-image! distributions name)
   (define distributions/sorted
     (map cdr (sort (hash->list distributions) string<? #:key car)))
   (define all-together
     (table/fill-missing distributions/sorted
                         #:columns 3
                         #:column-spacing 5
                         #:row-spacing 5))
   (define all-together+title
     (vc-append 20
                (text name)
                all-together))
   (pict->png! all-together+title (build-path out-dir (~a name ".png"))))

 (write-distributions-image! distributions-by-mutator/normalized
                             (~a name '- "normalized"))
 (write-distributions-image! distributions-by-mutator/unnormalized
                             (~a name '- "unnormalized")))
