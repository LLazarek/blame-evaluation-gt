#lang at-exp rscript

(require syntax/parse/define
         racket/date)

(struct present (value) #:transparent)
(define-values {absent absent?}
  (let ()
    (struct absent ())
    (values (absent) absent?)))

(define (in-option o)
  (in-list (match o
             [(present v) (list v)]
             [(? absent?) empty]
             [else (list o)])))
(define-simple-macro (for*/option clauses body ...)
  (let/ec return
    (for* clauses
      (return (present (let () body ...))))
    absent))
(define-simple-macro (option-let* ([name maybe-option] ...) body ...)
  (for*/option ([name (in-option maybe-option)] ...) body ...))
(define (try-unwrap o)
  (match o
    [(present v) (try-unwrap v)]
    [else o]))


(define-runtime-paths
  [store-path "../../experiment-data/experiment-manager"])

;; if it's not running?, it's pending
(struct job (id running?) #:transparent)
(define host<%> (interface (writable<%>)
                  [get-jobs (->*m {} {boolean?} (listof (or/c absent? (list/c string? string?))))]
                  [submit-job! (->*m {string?
                                      string?}
                                     {#:mode (or/c 'check 'record)
                                      #:cpus (or/c "decide" natural?)
                                      #:name string?}
                                     any)]
                  [cancel-job! (->m string? string? any)]
                  [system/host/string (unconstrained-domain-> string?)]))
(define host% (class object%
                (super-new)
                (init-field hostname host-project-path host-jobdir-path)
                (field [host-jobfile-path (build-path host-jobdir-path "job.sub")]
                       [host-racket-path (build-path host-project-path "racket" "bin" "racket")]
                       [host-utilities-path (build-path host-project-path "blame-evaluation-gt" "util")]
                       [host-data-path (build-path host-project-path "experiment-output")])
                (define/public (custom-write port) (write hostname port))
                (define/public (custom-display port) (display hostname port))))
(define condor-host%
  (class* host% (writable<%>)
    (super-new)
    (inherit-field hostname
                   host-project-path
                   host-jobdir-path
                   host-jobfile-path
                   host-racket-path
                   host-utilities-path
                   host-data-path)
    (init-field [data-store (build-path store-path (~a "condor-" hostname ".rktd"))])

    (define/public (system/host/string . parts)
      ;; lltodo: implement a persistent connection here to prevent being blocked
      ;; by zythos for opening too many connections too quickly
      (system/string @~a{ssh @hostname @(apply ~a (add-between parts " "))}))

    (define/public (get-jobs [active? #t])
      (define info (all-job-info))
      (if (equal? active? 'both)
          (let-values ([{active not} (partition job-running? info)]
                       [{find-job*} (match-lambda [(job id _) (find-job id)])])
            (values (map find-job* active)
                    (map find-job* not)))
          (filter-map (match-lambda [(job id (== active?)) (find-job id)]
                                    [else #f])
                      info)))
    (define/private (all-job-info)
      ;; lltodo: this should not wipe the database if the internet breaks!
      (define condor-dump (system/host/string "condor_q"))
      (define raw-info
        (regexp-match* @pregexp{(?m:^(\S+\s+){6}([1_])\s+([1_])\s+1\s+([\d.]+)$)}
                       condor-dump
                       #:match-select cddr))
      (define all-jobs
        (for/list ([parts (in-list raw-info)])
          (job (third parts) (string=? (first parts) "1"))))
      (expunge-old-jobs! all-jobs)
      all-jobs)

    (define/private (expunge-old-jobs! current-job-infos)
      (ensure-store!)
      (define filtered
        (for/list ([{bench+config id} (in-dict (read-data-store))]
                   #:when (findf (match-lambda [(job (== id) _) #t]
                                               [else #f])
                                 current-job-infos))
          (cons bench+config id)))
      (write-data-store! filtered))

    (define/public (submit-job! benchmark
                                config-name ; without .rkt
                                #:mode [record/check-mode 'check]
                                #:cpus [cpus "decide"]
                                #:name [name benchmark])
      (define job-uploaded?
        (with-temp-file job.sub
          (display-to-file
           ;; lltodo: remove dependency on run-experiment.sh
           @~a{
               # Set the universe
               Universe = vanilla

               # Describe the target machine
               Requirements = ((Machine == "allagash.cs.northwestern.edu") @;
                               || (Machine == "piraat.cs.northwestern.edu") @;
                               || (Machine == "fix.cs.northwestern.edu"))

               Rank = TARGET.Mips
               Copy_To_Spool = False

               # Notification
               Notification = error

               # Set the environment
               Getenv = True

               Arguments = "'@benchmark' '@|config-name|.rkt' '@record/check-mode' '@cpus' '@name'"
               Executable = /project/blgt/run-experiment.sh
               Error = condor-output.txt
               Output = condor-output.txt
               Log = condor-log.txt

               +IsWholeMachineJob = true
               +IsSuspensionJob = false

               Queue

               }
           job.sub
           #:exists 'replace)
          (match (system/exit-code @~a{scp -q '@job.sub' @|hostname|:@host-jobfile-path})
            [0 #t]
            [else absent])))
      (option-let* ([_ job-uploaded?]
                    [id (match (system/host/string
                                @~a{condor_submit -verbose @host-jobfile-path})
                          [(regexp @pregexp{\*\* Proc ([\d.]+):} (list _ id)) id]
                          [else absent])])
        (save-job! benchmark config-name id)))

    (define/public (cancel-job! benchmark config-name)
      (option-let* ([id (read-job benchmark config-name)])
                   (void (system/host/string @~a{condor_rm '@id'}))
                   (write-data-store! (dict-remove (read-data-store)
                                                   (list benchmark config-name)))))

    (define/private (ensure-store!)
      (make-directory* (path-only data-store))
      (unless (file-exists? data-store)
        (system @~a{touch '@data-store'})))
    (define/private (save-job! benchmark config-name id)
      (ensure-store!)
      (with-output-to-file data-store #:exists 'append
        (thunk (writeln (cons (list benchmark config-name) id)))))
    (define/private (read-job benchmark config-name) ; -> (option/c id?)
      (dict-ref (read-data-store)
                (list benchmark config-name)
                absent))
    (define/private (find-job target-id) ; -> (option/c (list/c benchmark config-name))
      (ensure-store!)
      (try-unwrap
       (for*/option ([{bench+config id} (in-dict (file->list data-store))]
                     #:when (string=? id target-id))
                    bench+config)))
    (define/private (read-data-store)
      (file->list data-store))
    (define/private (write-data-store! data)
      (display-lines-to-file (map ~s data)
                             data-store
                             #:exists 'replace))))

(define-simple-macro (with-temp-file name body ...)
  (call-with-temp-file (λ (name) body ...)))
(define (call-with-temp-file f)
  (define temp (make-temporary-file))
  (begin0 (f temp)
    (when (file-exists? temp) (delete-file temp))))

(define zythos (new condor-host%
                    [hostname "zythos"]
                    [host-project-path "./blgt"]
                    [host-jobdir-path "./proj/jobctl"]))
(define hosts (list zythos))

;; host<%> -> (option/c results?)
(define (get-results a-host)
  (define info-str
    (send a-host
          system/host/string
          (get-field host-racket-path a-host)
          (build-path (get-field host-utilities-path a-host) "check-experiment-results.rkt")
          "-w"
          (get-field host-data-path a-host)))
  (match info-str
    [(regexp "^#hash")
     (present (call-with-input-string info-str read))]
    [else
     (eprintf @~a{
                  Unable to get experiment results summary for host @a-host, @;
                  found: @~v[info-str]

                  })
     absent]))

;; host? string? -> (option/c (and/c real? (between/c 0 1)))
(define (get-progress a-host benchmark)
  (define progress-str
    (send a-host
          system/host/string
          (get-field host-racket-path a-host)
          (build-path (get-field host-utilities-path a-host) "check-experiment-progress.rkt")
          (build-path (get-field host-data-path a-host) benchmark)))
  (match progress-str
    [(regexp (pregexp @~a{[^@"\n"]+@"\n"([\d.]+)}) (list _ (app string->number %)))
     (present %)]
    [other
     (eprintf @~a{
                  Unable to get experiment progress for @benchmark on host @a-host, @;
                  found: @~v[other]

                  })
     absent]))

;; summary/c :=
;; (hash (or/c 'completed 'errored 'other) (listof (list/c string? string?))
;;       'incomplete                       (listof (list/c string? string? (option/c real?))))

;; host<%> -> (option/c summary/c)
(define (summarize-experiment-status a-host)
  (define (with-progress incomplete-bench)
    (match-define (list name config) incomplete-bench)
    (printf "Retrieving ~a progress ...\r" name)
    (list name config (try-unwrap (get-progress a-host name))))
  (printf "Checking ~a...\r" a-host)
  (option-let* ([results (get-results a-host)])
               (hash-update results
                            'incomplete
                            (λ (benches) (map with-progress benches)))))

(define (stuck-jobs active-jobs maybe-summary)
  (for*/list ([summary (in-option maybe-summary)]
              [incomplete-jobs (in-value (hash-ref summary 'incomplete))]
              [benchmark (in-list active-jobs)]
              [incomplete-job (in-list incomplete-jobs)]
              #:when (match incomplete-job
                       [(list (== (first benchmark))
                              (== (second benchmark))
                              (? (>/c 0.99)))
                        #t]
                       [else #f]))
    benchmark))

(define (restart-job! a-host job-info)
  (match-define (list benchmark config) job-info)
  (option-let* ([_ (send a-host cancel-job! benchmark config)]
                [_ (send a-host submit-job! benchmark config)])
               (void)))

(define (restart-stuck-jobs! a-host
                             [active-jobs (send a-host get-jobs #t)]
                             [maybe-summary (summarize-experiment-status a-host)])
  (for* ([summary (in-option maybe-summary)]
         [job-info (in-list (stuck-jobs active-jobs summary))])
    (displayln @~a{@(date->string (current-date) #t) Restarting stuck job: @job-info})
    (restart-job! a-host job-info)))

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

(define (download-completed-benchmarks! a-host summary)
  ;; lltodo: this
  (raise-user-error 'download-completed-benchmarks! "Not implemented"))


(define (string->value s)
  (with-input-from-string s read))
(define (string->benchmark-spec s)
  (match (string->value s)
    [(list host-sym config-name-sym benchmark-syms ...)
     (list (host-by-name (~a host-sym))
           (~a config-name-sym)
           (map ~a benchmark-syms))]
    [else (raise-user-error 'experiment-manager
                            @~a{Bad benchmark spec: @~v[s]})]))
(define (host-by-name name)
  (match (findf (λ (h) (string=? name (get-field hostname h))) hosts)
    [#f (raise-user-error 'experiment-manager @~a{No host known with name @name})]
    [host host]))
(define-simple-macro (pick-values f e)
  (call-with-values (thunk e)
                    (λ vals (f vals))))
(define ((mapper f) l) (map f l))

(main
 #:arguments {[(hash-table ['status? status?]
                           ['download (app (mapper host-by-name) download-targets)]
                           ['launch (app (mapper string->benchmark-spec) launch-targets)]
                           ['cancel (app (mapper string->benchmark-spec) cancel-targets)]
                           ['watch-for-stuck-jobs watch-for-stuck-jobs?])
               args]
              #:once-each
              [("-s" "--status")
               'status?
               "Check the experiment status on all active hosts."
               #:record]
              #:multi
              [("-d" "--download-results")
               'download
               ("Download the results from a host.")
               #:collect ["host" cons empty]]
              [("-l" "--launch")
               'launch
               ("Launch the specified benchmarks on a host for a mode.")
               #:collect ["(host mode benchmark ...)" cons empty]]
              [("-c" "--cancel")
               'cancel
               ("Cancel the specified benchmarks on a host for a mode.")
               #:collect ["(host mode benchmark ...)" cons empty]]
              [("-w" "--watch-for-stuck-jobs")
               'watch-for-stuck-jobs
               "Watch for stuck jobs on all hosts and restart them as they get stuck."
               #:record]}

 (define (for-each-target targets action name)
   (for ([target (in-list targets)])
     (match-define (list a-host config-name benchmarks) target)
     (for ([benchmark (in-list benchmarks)])
       (if (absent? (action a-host benchmark config-name))
           (displayln @~a{Failed @name @(list a-host config-name benchmark)})
           (displayln @~a{Successful @name @(list a-host config-name benchmark)})))))
 (cond [status?
        (for ([a-host (in-list hosts)])
          (define maybe-summary
            (summarize-experiment-status a-host))
          (displayln @~a{---------- @a-host ----------})
          (define-values {active-jobs pending-jobs} (send a-host get-jobs 'both))
          (displayln @~a{Active: @active-jobs})
          (displayln @~a{Pending: @pending-jobs})
          (pretty-display (try-unwrap maybe-summary))
          (newline))]
       [(not (empty? launch-targets))
        (for-each-target launch-targets
                         (λ (a-host benchmark config-name)
                           (send a-host submit-job! benchmark config-name))
                         "submit")]
       [(not (empty? cancel-targets))
        (for-each-target cancel-targets
                         (λ (a-host benchmark config-name)
                           (send a-host cancel-job! benchmark config-name))
                         "cancel")]
       [watch-for-stuck-jobs?
        (let loop ()
          (for-each restart-stuck-jobs! hosts)
          (printf "Sleeping                    \r")
          (sleep (* 5 60))
          (loop))]
       [(not (empty? download-targets))
        (for ([a-host (in-list download-targets)])
          (option-let* ([summary (summarize-experiment-status a-host)])
                       (download-completed-benchmarks! a-host summary)))]))
