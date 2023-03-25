#lang at-exp rscript

(provide bt-length-distribution-histogram-for
         bt-length-distributions-for
         bt-length)

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

(define plot-tree? any/c)

(define (bt-length trail normalize?)
  (define base-trail-length
    (sub1 (length (blame-trail-mutant-summaries trail))))
  (cond [(and normalize?
              (> base-trail-length 0))
         (define ordered-configs
           (sort (blame-trail-mutant-summaries trail)
                 <
                 #:key mutant-summary-id))
         (define first-mutant-config
           (mutant-summary-config (first ordered-configs)))
         (define last-mutant-config
           (mutant-summary-config (last ordered-configs)))
         (define number-of-components-typed
           (for/sum ([{mod level} (in-hash first-mutant-config)]
                     #:when (not (equal? (hash-ref last-mutant-config mod)
                                         level)))
             1))
         number-of-components-typed]
        [else base-trail-length]))

(define/contract (bt-length-distributions-for key data
                                              #:normalize? normalize?
                                              #:dump-to [dump-port #f]
                                              #:partition-by-success? [partition-by-success? #f])
  ({string?
   (hash/c string? (listof blame-trail?))
   #:normalize? boolean?}
   {#:dump-to (or/c output-port? #f)
    #:partition-by-success? boolean?}
   . ->* .
   (listof (list/c natural? (list/c real? real?))))

  (define (trail-length trail)
    (bt-length trail normalize?))

  (define trails (hash-ref data key))
  (when dump-port
    (define grouped-by-length (group-by trail-length trails))
    (pretty-write (for/hash ([group (in-list grouped-by-length)])
                    (define a-trail (first group))
                    (values (trail-length a-trail)
                            group))
                  dump-port))

  (define partitioner
    (if partition-by-success?
        satisfies-BT-hypothesis?
        (const #t)))
  (define trails-grouped-by-length
    (group-by trail-length trails))
  (define trails-by-length
    (for/list ([group (in-list trails-grouped-by-length)])
      (define a-trail (first group))
      (list (trail-length a-trail)
            group)))
  (define partitioned-trail-proportions-by-length
    (for/list ([length+trails (in-list trails-by-length)])
      (define-values {group-a group-b} (partition partitioner (second length+trails)))
      (list (first length+trails)
            (list (/ (length group-a) (length trails))
                  (/ (length group-b) (length trails))))))
  (define partitioned-trail-proportions-by-length/sorted
    (sort partitioned-trail-proportions-by-length < #:key first))
  (define data-has-0-already?
    (match partitioned-trail-proportions-by-length/sorted
      [(list* (list 0 _) _) #t]
      [else #f]))
  (define partitioned-trail-proportions-by-length/sorted/with-0
    (if data-has-0-already?
        partitioned-trail-proportions-by-length/sorted
        (cons (list 0 '(0 0))
              partitioned-trail-proportions-by-length/sorted)))

  partitioned-trail-proportions-by-length/sorted/with-0)

(define/contract (bt-length-distribution-histogram-for key data
                                                       #:normalize? normalize?
                                                       #:dump-to [dump-port #f]
                                                       #:color-by-success? [color-by-success? #f]
                                                       #:colors [success-colors '("green" "red")])
  ({string?
   (hash/c string? (listof blame-trail?))
   #:normalize? boolean?}
   {#:dump-to (or/c output-port? #f)
    #:color-by-success? boolean?
    #:colors (listof any/c)}
   . ->* .
   plot-tree?)

  (define partitioned-trail-proportions-by-length/sorted/with-0
    (bt-length-distributions-for key data
                                 #:normalize? normalize?
                                 #:dump-to dump-port
                                 #:partition-by-success? color-by-success?))

  (stacked-histogram partitioned-trail-proportions-by-length/sorted/with-0
                     #:colors success-colors))

(define/contract (bt-length-distribution-plot-for key data
                                                  #:normalize? normalize?
                                                  #:dump-to [dump-port #f])
  ({string?
   (hash/c string? (listof blame-trail?))
   #:normalize? boolean?}
   {#:dump-to (or/c output-port? #f)}
   . ->* .
   pict?)

  (plot-pict (bt-length-distribution-histogram-for key data
                                                   #:normalize? normalize?
                                                   #:dump-to dump-port)
             #:x-min 0
             #:y-min 0
             #:y-max 1
             #:x-label (~a "Blame trail length"
                           (if normalize? " (normalized)" ""))
             #:y-label (~a "Percent of trails")
             #:title key))

(main
 #:arguments {[(hash-table ['data-dir data-dir]
                           ['out-dir  out-dir]
                           ['name     name]
                           ['config   config-path]
                           ['dump-path dump-path]
                           ['by breakdown-dimension])
               args]
              #:once-each
              [("-d" "--data-dir")
               'data-dir
               ("Path to the directory containing data sub-directories for each"
                "benchmark of a given mode.")
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
               #:mandatory]
              [("-D" "--dump-data")
               'dump-path
               "Dump data for generating the plot to the given file."
               #:collect ["path" take-latest #f]]
              [("-b" "--by")
               'by
               "Break down the data by either mutator or benchmark. Default: mutator"
               #:collect ["mutator or benchmark" take-latest "mutator"]]}
 #:check [(member breakdown-dimension '("mutator" "benchmark"))
          @~a{Invalid argument to --by: @breakdown-dimension}]

 (install-configuration! config-path)

 (match-define (list distributions/normalized
                     distributions/unnormalized)
   (for/list ([normalized? (in-list '(#t #f))])
     (make-distributions-table (λ (key data #:dump-to [dump-to #f])
                                 (bt-length-distribution-plot-for key
                                                                  data
                                                                  #:normalize? normalized?
                                                                  #:dump-to dump-to))
                               #:breakdown-by breakdown-dimension
                               #:summaries-db (mutation-analysis-summaries-db)
                               #:data-directory data-dir
                               #:dump-to-file dump-path)))

 (make-directory* out-dir)
 (define (write-distributions-image! distributions name)
   (define with-title
     (vc-append 20
                (text name)
                distributions))
   (pict->png! with-title (build-path out-dir (~a name '- breakdown-dimension ".png"))))

 (write-distributions-image! distributions/normalized
                             (~a name '- "normalized"))
 (write-distributions-image! distributions/unnormalized
                             (~a name '- "unnormalized")))
