#lang at-exp rscript

(require racket/random
         "../util/mutant-util.rkt"
         "../util/debug-mutant.rkt"
         syntax/parse/define)

(define-runtime-paths
  [benchmarks-dir "../../../gtp-benchmarks/benchmarks"]
  [checked-log "../../../tmp/spot-checked.rktd"])

(define (clear-log!)
  (with-output-to-file checked-log newline))
(define (log! d-m)
  (with-output-to-file checked-log
    #:exists 'append
    (thunk (display "\"")
           (d-m 'none
                #:diff-mutant? #t)
           (displayln "\""))))
(define (logged? d-m)
  (define diff (with-output-to-string (thunk (d-m 'none
                                                  #:diff-mutant? #t))))
  (and (file-exists? checked-log)
       (member diff
               (file->list checked-log))))

(define-simple-macro (user-prompt!*/dispatch {~optional lead-msg}
                                             {~alt {~once [default-key
                                                                     #:default
                                                                     default-description
                                                                     default-action ...]}
                                                            [key
                                                             description
                                                             action ...]} ...)
  (match (user-prompt!* @~a{
                            @(string-append {~? {~@ lead-msg "\n"}})@;
                            @default-key or empty : @default-description
                            @string-join[(list @~a{@key : @description} ...) "\n"]

                            }
                        (list default-key key ...)
                        #:normalize values)
    [default-key default-action ...]
    [key action ...]
    ...))

(define (interactively-spot-check-all! logged)
  (let loop ([groups-todo (group-by (compose1 first car) logged)]
             [mutants-todo empty])
    (cond [(and (empty? groups-todo)
                (empty? mutants-todo))
           (displayln "All done!")]
          [(empty? mutants-todo)
           (loop (rest groups-todo)
                 (first groups-todo))]
          [else
           (define a-mutant (car (random-ref mutants-todo)))
           (define benchmark (first a-mutant))
           (user-prompt!*/dispatch
            @~a{
                There are @(length mutants-todo) @benchmark mutants without dynamic errors. @;
                Check out a mutant?
                }

            ['y #:default
                @~a{Inspect @a-mutant}
                (interactively-spot-check! a-mutant)
                (loop groups-todo
                      (remove (cons a-mutant #f) mutants-todo))]
            ['n @~a{Move on to the next benchmark}
                (loop groups-todo
                      empty)]
            ['x "Exit" (void)])])))

(define (interactively-spot-check! a-mutant)
  (define (emacs-open! path)
    (void
     (system/string
      @~a{emacsclient --eval '(switch-to-buffer-other-frame (find-file-noselect "@path"))'})))

  (match-define (list benchmark (mutant _ mod index)) a-mutant)
  (displayln @~a{
                 @benchmark @mod @"@" @index

                 })
  (define d-m (curry debug-mutant benchmark
                     mod
                     index))
  (d-m 'none
       #:diff-mutant? #t
       #:stop-diff-early? #t)
  (unless (logged? d-m)
    (let loop ()
      (newline)
      (newline)
      (user-prompt!*/dispatch
       "Choose next action..."

       ['D #:default "Done with this mutant, record it as checked" (log! d-m)]
       ['DD "Done with this mutant, but don't record it as checked" (void)]
       ['f "Show the full diff of the mutant"
           (d-m 'none
                #:diff-mutant? #t)
           (loop)]
       ['r "Run the bottom config of this mutant"
           (d-m 'none #:run? #t)
           (loop)]
       ['R "Run the top config of this mutant"
           (d-m 'types #:run? #t)
           (loop)]
       ['d "Dump this mutant to disk and open it for inspection"
           (d-m 'none
                #:run? #t
                #:write-modules-to (build-path benchmarks-dir "debug"))
           (emacs-open! (build-path benchmarks-dir "debug"))
           (loop)]
       ['o "Open the benchmark for inspection"
           (emacs-open! (build-path benchmarks-dir benchmark))
           (loop)]

       ['C "Clear the checked-mutant log"
           (clear-log!)]))))

(main
 #:arguments ({(hash-table)
               (list dyn-err-summaries-progress-log-path)}
              #:args [dyn-err-summaries-progress-log-path])
 #:check [(path-to-existant-file? dyn-err-summaries-progress-log-path)
          @~a{@dyn-err-summaries-progress-log-path not found}]

 (interactively-spot-check-all!
  (filter-not cdr (file->list dyn-err-summaries-progress-log-path))))
