#lang at-exp rscript

(provide (contract-out
          [directory->blame-trails-by-mutator/across-all-benchmarks
           (path-to-existant-directory?
            #:summaries-db db:path-to-db?
            . -> .
            (hash/c string? (listof blame-trail?)))]
          [make-distributions-plots
           ({({string?
               (hash/c string? (listof blame-trail?))}
              {#:dump-to (or/c output-port? #f)}
              . ->* .
              pict?)
             #:breakdown-by (or/c "mutator" "benchmark")
             #:summaries-db db:path-to-db?
             #:data-directory path-to-existant-directory?}
            {#:dump-to-file (or/c path-string? boolean?)}
            . ->* .
            (hash/c string? pict?))]
          [make-distributions-table
           ({({string?
               (hash/c string? (listof blame-trail?))}
              {#:dump-to (or/c output-port? #f)}
              . ->* .
              pict?)
             #:breakdown-by (or/c "mutator" "benchmark")
             #:summaries-db db:path-to-db?
             #:data-directory path-to-existant-directory?}
            {#:dump-to-file (or/c path-string? boolean?)}
            . ->* .
            pict?)]
          [blame-reliability-breakdown-for
           (string?
            (hash/c string? (listof blame-trail?))
            . -> .
            (hash/c (or/c "always" "sometimes" "never")
                    (listof listof-blame-trails-for-same-mutant?)))]
          [satisfies-BT-hypothesis? (blame-trail? . -> . boolean?)]
          [immediate-type-err-bt? (blame-trail? . -> . boolean?)]
          [add-missing-active-mutators
           ((hash/c mutator-name? (listof blame-trail?))
            . -> .
            (hash/c mutator-name? (listof blame-trail?)))]
          [two-sided-histogram
           ({(listof (list/c any/c real? real?))}
            {#:top-color any/c
             #:bot-color any/c
             #:gap any/c
             #:skip any/c
             #:add-ticks? boolean?}
            . ->* .
            list?)]
          [absolute-value-format (ticks? . -> . ticks?)])
         pict?
         add-to-list)

(require plot
         (except-in pict-util line)
         bex/experiment/blame-trail-data
         (prefix-in db: bex/db/db)
         bex/configurables/configurables
         bex/runner/mutation-runner-data
         bex/configurables/program-instrumentation/type-interface-module-names
         (only-in bex/mutation-analysis/mutation-analysis-summaries
                  mutator-name?)

         "read-data.rkt")

(define pict? any/c)

(define (directory->blame-trails-by-mutator/across-all-benchmarks
         data-dir
         #:summaries-db mutation-analysis-summaries-db)
  (define mutant-mutators
    (read-mutants-by-mutator mutation-analysis-summaries-db))

  (add-missing-active-mutators
   (read-blame-trails-by-mutator/across-all-benchmarks data-dir mutant-mutators)))

(define (make-distributions-plots make-distribution-plot
                                  #:breakdown-by breakdown-dimension
                                  #:summaries-db mutation-analysis-summaries-db
                                  #:data-directory data-dir
                                  #:dump-to-file [dump-path #f])
  (define blame-trails-by-mutator/across-all-benchmarks
    (directory->blame-trails-by-mutator/across-all-benchmarks
     data-dir
     #:summaries-db mutation-analysis-summaries-db))

  (define all-mutator-names (mutator-names blame-trails-by-mutator/across-all-benchmarks))

  (define dump-port (and dump-path
                         (open-output-file dump-path #:exists 'replace)))
  (define distributions
    (match breakdown-dimension
      ["mutator"
       (for/hash ([mutator (in-list all-mutator-names)])
         (when dump-port (newline dump-port) (displayln mutator dump-port))
         (define plot
           (make-distribution-plot mutator
                                   blame-trails-by-mutator/across-all-benchmarks
                                   #:dump-to dump-port))
         (values mutator plot))]
      ["benchmark"
       (define blame-trails-by-benchmark/across-all-mutators
         (for*/fold ([bts-by-benchmark (hash)])
                    ([{mutator bts} (in-hash blame-trails-by-mutator/across-all-benchmarks)]
                     [bt (in-list bts)])
           (define benchmark (mutant-benchmark (blame-trail-mutant-id bt)))
           (hash-update bts-by-benchmark
                        benchmark
                        (add-to-list bt)
                        empty)))
       (for/hash ([benchmark (in-hash-keys blame-trails-by-benchmark/across-all-mutators)])
         (when dump-port (newline dump-port) (displayln benchmark dump-port))
         (define plot
           (make-distribution-plot benchmark
                                   blame-trails-by-benchmark/across-all-mutators
                                   #:dump-to dump-port))
         (values benchmark plot))]))
  (when dump-port (close-output-port dump-port))
  distributions)

(define (make-distributions-table make-distribution-plot
                                  #:breakdown-by breakdown-dimension
                                  #:summaries-db mutation-analysis-summaries-db
                                  #:data-directory data-dir
                                  #:dump-to-file [dump-path #f])
  (define distributions
    (make-distributions-plots make-distribution-plot
                              #:breakdown-by breakdown-dimension
                              #:summaries-db mutation-analysis-summaries-db
                              #:data-directory data-dir
                              #:dump-to-file dump-path))
  (define distributions/sorted
    (map cdr (sort (hash->list distributions) string<? #:key car)))
  (table/fill-missing distributions/sorted
                      #:columns 3
                      #:column-spacing 5
                      #:row-spacing 5))

(define (add-missing-active-mutators blame-trails-by-mutator/across-all-benchmarks)
  (for/fold ([data+missing blame-trails-by-mutator/across-all-benchmarks])
            ([mutator-name (in-list (configured:active-mutator-names))])
    (hash-update data+missing
                 mutator-name
                 values
                 empty)))

(define-logger bt-hypoth-options)
(define (satisfies-BT-hypothesis? bt)
  (define end-mutant-summary (first (blame-trail-mutant-summaries bt)))
  (define mode (blame-trail-mode-config-name bt))
  (define type-interface-mod?
    (match-lambda [(or (== type-interface-file-name)
                       (== type-interface-file-rename)) #t]
                  [else #f]))
  (match end-mutant-summary
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome 'type-error]
                                          [blamed blamed]))
                     config)
     ;; Original definition, changed to any type error because any type error
     ;; should be good enough: see justification in notes
     #;(and (equal? (hash-ref config mutated-mod-name) 'types)
          (list? blamed)
          (member mutated-mod-name blamed))
     #t]
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome 'blamed]
                                          [blamed blamed]))
                     config)
     #:when (member mode '("TR.rkt" "transient-newest.rkt" "transient-oldest.rkt"))
     (match* {mode blamed}
       [{_ (list (? type-interface-mod?))}
        #t]
       [{"transient-newest.rkt" (or (list* (? type-interface-mod?) _)
                                    (list* _ (? type-interface-mod?) _))}
        #t]
       [{"transient-oldest.rkt" (or (list _ ... (? type-interface-mod?))
                                    (list _ ... (? type-interface-mod?) _))}
        #t]
       [{_ other-blamed-list}
        (when (ormap type-interface-mod? other-blamed-list)
          (log-bt-hypoth-options-info
           @~a{mode @mode, blamed contains interface somewhere in middle}))
        #f])]
    [(mutant-summary _
                     (struct* run-status ([mutated-module mutated-mod-name]
                                          [outcome (and outcome
                                                        (or 'runtime-error
                                                            'blamed))]
                                          [context-stack stack]))
                     config)
     #:when (or (member mode '("TR-stack-first.rkt"
                               "transient-stack-first.rkt"
                               "erasure-stack-first.rkt"))
                (and (member mode '("TR.rkt" "transient-newest.rkt" "transient-oldest.rkt"))
                     (equal? outcome 'runtime-error)))
     (define stack/no-typed-mods
       (filter (λ (m) (equal? (hash-ref config m 'untyped) 'untyped))
               stack))
     (match stack/no-typed-mods
       [(list* (? type-interface-mod?) _) #t]
       [else #f])]
    [else #f]))

(module+ test
  (require ruinit
           "data-adapter.rkt")
  (test-begin
    #:name satisfies-BT-hypothesis?
    (satisfies-BT-hypothesis?
     (blame-trail '#s(mutant "acquire" "board.rkt" 5659)
                  87
                  "TR.rkt"
                  (map adapt-mutant-summary
                       '(#s(mutant-summary 26565 #s(run-status "board.rkt" 5659 what-kind-of-spot type-error ("board.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . types) ("basics.rkt" . types) ("board.rkt" . types) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26454 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("board.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . types) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26345 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("auxiliaries.rkt") #f) #hash(("admin.rkt" . types) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26256 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("admin.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26167 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("state.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26093 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("basics.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                         #s(mutant-summary 26003 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("tree.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . none)))
                         #s(mutant-summary 6350 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("strategy.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . none) ("tree.rkt" . none)))))))
    (not (satisfies-BT-hypothesis?
          (blame-trail '#s(mutant "acquire" "board.rkt" 5659)
                       87
                       "TR.rkt"
                       (map adapt-mutant-summary
                            '(#s(mutant-summary 26256 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("admin.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . types) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26167 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("state.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . types) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26093 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("basics.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . types)))
                              #s(mutant-summary 26003 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("tree.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . types) ("tree.rkt" . none)))
                              #s(mutant-summary 6350 #s(run-status "board.rkt" 5659 what-kind-of-spot runtime-error ("strategy.rkt") #f) #hash(("admin.rkt" . none) ("auxiliaries.rkt" . none) ("basics.rkt" . none) ("board.rkt" . none) ("main.rkt" . types) ("player.rkt" . types) ("state.rkt" . none) ("strategy.rkt" . none) ("tree.rkt" . none))))))))
    (satisfies-BT-hypothesis? (blame-trail
                               #s(mutant "quadU" "type-interface.rkt" 5)
                               34
                               "transient-oldest.rkt"
                               '(#s(mutant-summary
                                    4560
                                    #s(run-status
                                       "type-interface.rkt"
                                       5
                                       Quad
                                       blamed
                                       ("type-interface.rkt"
                                        "type-interface.rkt"
                                        "quick-sample.rkt"
                                        "type-interface.rkt"
                                        "type-interface.rkt")
                                       ("main.rkt" "main.rkt" "main.rkt")
                                       ("main.rkt" "main.rkt")
                                       #f)
                                    #hash(("hyphenate.rkt" . types)
                                          ("main.rkt" . types)
                                          ("measure.rkt" . types)
                                          ("ocm-struct.rkt" . none)
                                          ("ocm.rkt" . types)
                                          ("penalty-struct.rkt" . none)
                                          ("quad-main.rkt" . types)
                                          ("quads.rkt" . none)
                                          ("quick-sample.rkt" . types)
                                          ("render.rkt" . none)
                                          ("sugar-list.rkt" . none)
                                          ("utils.rkt" . none)
                                          ("world.rkt" . types)
                                          ("wrap.rkt" . none)))
                                 #s(mutant-summary
                                    323
                                    #s(run-status
                                       "type-interface.rkt"
                                       5
                                       Quad
                                       blamed
                                       ("type-interface.rkt"
                                        "type-interface.rkt"
                                        "quick-sample.rkt"
                                        "original-type-interface.rkt")
                                       ("main.rkt" "main.rkt" "main.rkt")
                                       ("main.rkt" "main.rkt")
                                       #f)
                                    #hash(("hyphenate.rkt" . types)
                                          ("main.rkt" . types)
                                          ("measure.rkt" . types)
                                          ("ocm-struct.rkt" . none)
                                          ("ocm.rkt" . types)
                                          ("penalty-struct.rkt" . none)
                                          ("quad-main.rkt" . types)
                                          ("quads.rkt" . none)
                                          ("quick-sample.rkt" . none)
                                          ("render.rkt" . none)
                                          ("sugar-list.rkt" . none)
                                          ("utils.rkt" . none)
                                          ("world.rkt" . types)
                                          ("wrap.rkt" . none))))))))

(define ((add-to-list v) l) (cons v l))

;; string? (hash/c string? (listof blame-trail?))
;; ->
;; (hash/c "always"    (listof (listof blame-trail?))
;;         "sometimes" ^
;;         "never"     ^)
(define (blame-reliability-breakdown-for key
                                         blame-trail-map)
  (define trails (hash-ref blame-trail-map key))
  (define total-trail-count (length trails))
  (define trails-grouped-by-mutant (group-by blame-trail-mutant-id trails))

  (define (categorize-trail-set-reliability trail-set)
    (define bt-success-count (count satisfies-BT-hypothesis? trail-set))
    (match* {bt-success-count (length trail-set)}
      [{     0  (not 0)}                 "never"]
      [{s       n      } #:when (= s n)  "always"]
      [{(not 0) (not 0)}                 "sometimes"]))

  (for/fold ([breakdown (hash "always" empty
                              "sometimes" empty
                              "never" empty)])
            ([mutant-trails (in-list trails-grouped-by-mutant)])
    (define mutant-category (categorize-trail-set-reliability mutant-trails))
    (hash-update breakdown
                 mutant-category
                 (add-to-list mutant-trails))))

(define (listof-blame-trails-for-same-mutant? l)
  (and (list? l)
       (andmap blame-trail? l)
       (cond [(empty? l) #t]
             [else
              (define mutant (blame-trail-mutant-id (first l)))
              (for/and ([bt (in-list l)])
                (equal? (blame-trail-mutant-id bt)
                        mutant))])))

; See ctc above
(define (two-sided-histogram data
                             #:top-color [top-color (rectangle-color)]
                             #:bot-color [bot-color (rectangle-color)]
                             #:gap [gap (discrete-histogram-gap)]
                             #:skip [skip (discrete-histogram-skip)]
                             #:add-ticks? [add-ticks? #t])
  (list (discrete-histogram (map (match-lambda [(list name top _) (list name top)])
                                 data)
                            #:color top-color
                            #:gap gap
                            #:skip skip
                            #:add-ticks? add-ticks?)
        ;; lltodo: this is a workaround for a bug with discrete-histogram
        ;; This doesn't work:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2) (b -4)))
                         (x-axis))
                   #:y-min -5)
        ;; while this does:
        #;(plot (list (discrete-histogram '((a 5) (b 3)))
                         (discrete-histogram '((a -2)) #:x-min 0)
                         (discrete-histogram '((b -4)) #:x-min 1)
                         (x-axis))
                   #:y-min -5)
        (for/list ([bar-sides (in-list data)]
                   [i (in-naturals)])
          (match-define (list name _ bottom) bar-sides)
          (discrete-histogram `((,name ,(- bottom)))
                              #:x-min i
                              #:color bot-color
                              #:gap gap
                              #:skip skip
                              #:add-ticks? add-ticks?))))

; See ctc above
(define (absolute-value-format original-ticks)
  (ticks (ticks-layout original-ticks)
         (λ (min max ticks)
           (define absolute-value-ticks
             (for/list ([tick (in-list ticks)])
               (struct-copy pre-tick tick
                            [value (abs (pre-tick-value tick))])))
           ((ticks-format original-ticks) min max absolute-value-ticks))))

(define immediate-type-err-bt?
  (match-lambda
    [(struct* blame-trail
              ([mutant-summaries
                (list
                 (mutant-summary
                  _
                  (struct* run-status ([outcome 'type-error]))
                  _))]))
     #t]
    [else #f]))
