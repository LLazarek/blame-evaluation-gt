#lang at-exp rscript

(require "../configurables/configurables.rkt"
         (submod "../experiment/mutant-factory.rkt" test)
         "../experiment/mutant-factory-data.rkt"
         "../configurations/configure-benchmark.rkt"
         "mutant-util.rkt"
         "option.rkt")

(define-runtime-paths
  [configs-dir "../configurables"]
  [experiment-launch-dir "../../.."])

(define (guess-path #:fail-thunk fail-f . parts)
  (define path (apply build-path parts))
  (if (or (path-to-existant-directory? path)
          (path-to-existant-file? path))
      path
      (fail-f path)))

(define (infer-benchmark log-path #:fail-thunk fail-thunk)
  (define benchmark-path
    (match (system/string @~a{grep -E 'Running on benchmark' @log-path})
      [(regexp #px"(?m:Running on benchmark (.+)$)" (list _ path))
       #:when (path-to-existant-directory? path)
       path]
      [(regexp #px"(?m:Running on benchmark #s\\(benchmark (.+)$)" (list _ benchmark-parts-str))
       (define benchmark-parts (call-with-input-string (~a "(" benchmark-parts-str) read))
       (define a-mod-path-symbol (first (first benchmark-parts)))
       (simple-form-path (build-path (~a a-mod-path-symbol) ".." ".."))]
      [other
       (fail-thunk)]))
  (read-benchmark benchmark-path))

(define (infer-configuration log-path #:fail-thunk fail-thunk)
  (match (system/string @~a{grep -E 'Running experiment with config' @log-path})
    [(regexp #rx"(?m:config (.+)$)" (list _ path))
     #:when (path-to-existant-file? path)
     path]
    [(regexp #rx"(?m:config (.+)$)" (list _ path))
     #:when (path-to-existant-file? (build-path experiment-launch-dir path))
     (build-path experiment-launch-dir path)]
    [(regexp #rx"(?m:config .+/(.+)$)" (list _ config-name))
     #:when (path-to-existant-file? (build-path configs-dir config-name))
     (build-path configs-dir config-name)]
    [else
     (fail-thunk)]))

(define (all-mutants-for bench)
  (define select-mutants (configured:select-mutants))
  (for*/list ([module-to-mutate-name (in-list (benchmark->mutatable-modules bench))]
              [mutation-index (select-mutants module-to-mutate-name bench)])
    (mutant module-to-mutate-name mutation-index #t)))

(define (check-progress-percentage progress-log-path all-mutants)
  (define progress (file->list progress-log-path))
  (/ (length progress)
     (* (sample-size) (length all-mutants))))

(define (progress-bar-string % #:width width)
  (define head-pos (inexact->exact (round (* % width))))
  (define before-head (make-string head-pos #\=))
  (define after-head (make-string (- width head-pos) #\space))
  (~a before-head ">" after-head))

(define (format-updating-display #:width width . parts)
  (define full (apply ~a parts))
  (define len (string-length full))
  (define remaining (- width len))
  (if (negative? remaining)
      (~a (substring full 0 (- width 3)) "...")
      (~a full (make-string remaining #\space))))

(define (truncate-string-to str chars)
  (if (<= (string-length str) chars)
      str
      (substring str 0 chars)))

(main
 #:arguments {[(hash-table ['watch watch-mode?]
                           ['log-name log-names]
                           ['readable-output? readable-output?])
               benchmark-dirs]
              #:once-each
              [("-w" "--watch")
               'watch
               ("Interactively show a progress bar that updates every 5 sec."
                "Only works with a single benchmark-dir.")
               #:record]
              [("-r" "--readable")
               'readable-output?
               ("Output information in a `read`able format.")
               #:record]
              #:multi
              [("-l" "--log-name")
               'log-name
               ("Explicitly provide the log file name. (one per benchmark-dir)")
               #:collect {"path" cons empty}]
              #:args benchmark-dirs}
 #:check [(andmap path-to-existant-directory? benchmark-dirs)
          @~a{Unable to find @(filter-not path-to-existant-directory? benchmark-dirs)}]
 #:check [(not (and watch-mode? (not (= (length benchmark-dirs) 1))))
          @~a{Watch mode can only be specified with a single benchmark-dir.}]

 (define %s
   (for/list ([benchmark-dir (in-list benchmark-dirs)]
              [log-name (in-sequences log-names (in-cycle (in-value #f)))])
     (option-let*
      ([log-path
        (guess-path benchmark-dir (or log-name (~a (basename benchmark-dir) ".log"))
                    #:fail-thunk
                    (λ (path)
                      (if readable-output?
                          absent
                          (raise-user-error
                           'guess-path
                           @~a{Unable to infer log path. Guessed: @path}))))]

       [bench
        (infer-benchmark
         log-path
         #:fail-thunk (thunk
                       (if readable-output?
                           absent
                           (raise-user-error
                            'check-experiment-progress
                            @~a{
                                Unable to infer benchmark path. @;
                                Are you running from the same directory the experiment was run?
                                }))))]

       [config-path
        (infer-configuration
         log-path
         #:fail-thunk (thunk
                       (if readable-output?
                           absent
                           (raise-user-error
                            'check-experiment-progress
                            "Unable to infer path to config for this experiment."))))]

       [_ (begin
            (install-configuration! config-path)
            (unless readable-output?
              (displayln @~a{
                             Inferred benchmark @(benchmark->name bench) @;
                             and config @(find-relative-path (simple-form-path configs-dir)
                                                             (simple-form-path config-path))
                             })))]

       [all-mutants (all-mutants-for bench)]

       [progress-log-path
        (guess-path (path-replace-extension log-path "-progress.log")
                    #:fail-thunk
                    (λ (path)
                      (if readable-output?
                          absent
                          (raise-user-error
                           'guess-path
                           @~a{Unable to infer progress log path. Guessed: @path}))))])

      (cond [watch-mode?
             (define period 5)
             (define start-time (current-inexact-milliseconds))
             (define start-% (check-progress-percentage progress-log-path
                                                        all-mutants))
             (let loop ()
               (define % (check-progress-percentage progress-log-path
                                                    all-mutants))
               (define pretty-%
                 (truncate-string-to (~a (* (/ (truncate (* % 1000)) 1000.0) 100))
                                     4))
               (define completion-rate:%/s
                 (/ (- % start-%)
                    (/ (match (- (current-inexact-milliseconds)
                                 start-time)
                         [0 +inf.0]
                         [not-0 not-0])
                       1000.0)))
               (define remaining-% (- 1 %))
               (define remaining-time-estimate
                 (if (zero? completion-rate:%/s)
                     0
                     (inexact->exact (round (/ (/ remaining-% completion-rate:%/s) 60.0)))))
               (display
                (format-updating-display
                 #:width 80
                 @~a{[@(progress-bar-string % #:width 50)] @|pretty-%|%}
                 " "
                 remaining-time-estimate
                 " min left"))
               (display "\r")
               (unless (= % 1)
                 (sleep period)
                 (loop)))]
            [else
             (define % (exact->inexact (check-progress-percentage progress-log-path
                                                                  all-mutants)))
             (if readable-output?
                 %
                 (displayln %))]))))
 (when readable-output?
   (writeln (for/list ([maybe-% (in-list %s)])
              (if (absent? maybe-%)
                  'absent
                  maybe-%)))))
