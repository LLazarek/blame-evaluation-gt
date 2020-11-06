#lang at-exp rscript

(define (extract-experiment-status log-path)
  (define last-2-lines (system/string @~a{tail -n 2 '@log-path'}))
  (match last-2-lines
    [(regexp "^factory:.*\nfactory:.*$")
     (values 'incomplete
             #f)]
    [(regexp "Experiment complete, and basic sanity checks pass")
     (values 'complete
             #f)]
    [(regexp "Experiment complete, but with failing sanity checks")
     (values 'complete
             #t)]
    ;; An error was thrown that crashed the experiment
    ;; lltodo: this is wrong. The experiment can get stuck, and the last log just happened to
    ;; be multiple lines.
    [else
     (values 'complete
             #t)]))

(main
 #:arguments {[(hash-table ['readable? readable?])
               (list data-dir)]
              #:once-each
              [("-w" "--write-output")
               'readable?
               ("Output results in a `read`able format (ie machine parseable)"
                "instead of human-readable.")
               #:record]
              #:args [data-dir]}

 (define statuses
   (for/hash ([bench-dir (in-list (directory-list data-dir #:build? #t))]
              #:when (path-to-existant-directory? bench-dir))

     (define bench-dir-name (basename bench-dir))
     (define contents (directory-list bench-dir #:build? #t))
     (define-values {log-path status ended-with-err? config}
       (match (findf (λ (p) (and (regexp-match? @regexp{^[A-Za-z0-9]+\.log$}
                                                (basename p))
                                 (not (equal? (basename p)
                                              "errs.log"))))
                     contents)
         [(? path-to-existant-file? log-path)
          (define-values {status ended-with-err?} (extract-experiment-status log-path))
          (define config
            (with-input-from-file log-path
              (thunk
               (match (regexp-match #px"Running experiment with config [^ ]+/([^/]+).rkt"
                                    (current-input-port))
                 [(list _ config-name) (~a config-name)]
                 [else '?]))))
          (values log-path status ended-with-err? config)]
         [else (values #f '? '? '?)]))

     (define errs?
       (cond [(equal? status 'complete)
              ended-with-err?]
             [else
              (match (findf (λ (p) (regexp-match? @regexp{errs.log$}
                                                  (basename p)))
                            contents)
                [(? path-string? err-log-path) (not (<= (file-size err-log-path) 1))]
                [else '?])]))

     (values bench-dir-name
             (list status errs? config))))

 (define (format-name name)
   (cond [readable? name]
         [else
          (define len (string-length name))
          (~a name (make-string (- 15 len) #\space))]))

 (define complete/no-errors
   (for/list ([{name status} (in-hash statuses)]
              #:when (match status
                       [(list 'complete #f config) #t]
                       [else #f]))
     (list (format-name name) (third status))))
 (define complete/errors
   (for/list ([{name status} (in-hash statuses)]
              #:when (match status
                       [(list 'complete #t config) #t]
                       [else #f]))
     (list (format-name name) (third status))))
 (define incomplete/no-errors
   (for/list ([{name status} (in-hash statuses)]
              #:when (match status
                       [(list 'incomplete #f config) #t]
                       [else #f]))
     (list (format-name name) (third status))))
 (define incomplete/other
   (for/list ([{name status} (in-hash statuses)]
              #:when (match status
                       [(list 'incomplete (not #f) config) #t]
                       [else #f]))
     (list (format-name name) (third status))))

 (define all-others
   (set-subtract (hash-keys statuses)
                 complete/no-errors
                 complete/errors
                 incomplete/no-errors
                 incomplete/other))

 (define (compute-missing needed)
   (set-subtract needed
                 (map (compose1 string-trim first)
                      complete/no-errors)))
 (define needed-benchmarks/14
   '("dungeon"
     "jpeg"
     "zordoz"
     "lnm"
     "suffixtree"
     "kcfa"
     "snake"
     "take5"
     "acquire"
     "tetris"
     "synth"
     "gregor"
     "quadT"
     "quadU"))
 (define needed-benchmarks/10
   (drop needed-benchmarks/14 4))
 (define needed-benchmarks/5
   '("dungeon" "quadU" "kcfa" "acquire" "tetris"))

 (if readable?
     (writeln (hash 'completed complete/no-errors
                    'errored complete/errors
                    'incomplete incomplete/no-errors
                    'other incomplete/other))
     (displayln
      @~a{
          Summary of results in @data-dir

          Completed
          =========
          @(string-join (map ~a complete/no-errors) "\n")

          Completed with errors
          ---------------------
          @(string-join (map ~a complete/errors) "\n")

          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          Missing completed benchmarks:
          14: @(compute-missing needed-benchmarks/14)
          10: @(compute-missing needed-benchmarks/10)
          5:  @(compute-missing needed-benchmarks/5)
          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          Incomplete
          ==========
          Without errors (yet)
          --------------------
          @(string-join (map ~a incomplete/no-errors) "\n")

          Others (with errors and unknown)
          --------------------------------
          @(string-join (map ~a incomplete/other) "\n")
          })))
