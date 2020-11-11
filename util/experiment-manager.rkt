#lang at-exp rscript

(require syntax/parse/define
         racket/date)

(struct present (value) #:transparent)
(define-values {absent absent?}
  (let ()
    (struct absent ())
    (values (absent) absent?)))
(define (option/c inner/c)
  (or/c absent? (struct/c present inner/c) inner/c))

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
  [store-path "../../experiment-data/experiment-manager"]
  [data-path "../../experiment-data/results/code-mutations"]
  [dbs-path "../../experiment-data/dbs/code-mutations.tar.gz"])

;; if it's not running?, it's pending
(struct job (id running?) #:transparent)
(define host<%> (interface (writable<%>)
                  [get-jobs
                   (let ([job-descr/c (listof (option/c (list/c string? string?)))])
                     (->*m {} {(or/c boolean? 'both)}
                           (option/c
                            (or/c job-descr/c
                                  (list/c job-descr/c job-descr/c)))))]
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

    (define/public (system/host #:interactive? [interactive? #f] . parts)
      ;; lltodo: implement a persistent connection here to prevent being blocked
      ;; by zythos for opening too many connections too quickly
      (system @~a{ssh @(if interactive? "-t" "") @hostname "@(apply ~a (add-between parts " "))"}))
    (define/public (system/host/string #:interactive? [interactive? #f] . parts)
      (define out-str (open-output-string))
      (parameterize ([current-output-port out-str]
                     [current-error-port out-str])
        (send this system/host #:interactive? interactive? . parts))
      (get-output-string out-str))

    (define/public (scp #:from-host [from-host-path #f]
                        #:to-local [to-local-path #f]
                        #:from-local [from-local-path #f]
                        #:to-host [to-host-path #f])
      (system/exit-code
       @~a{scp -q @(match* {from-host-path
                            to-local-path
                            from-local-path
                            to-host-path}
                     [{from-remote to-local #f #f}
                      @~a{@|hostname|:'@from-remote' '@to-local'}]
                     [{#f #f from-local to-remote}
                      @~a{'@from-local' @|hostname|:'@to-remote'}]
                     [{_ _ _ _} (raise-user-error 'scp "Bad argument combination")])}))

    (define/public (get-jobs [active? #t])
      (option-let*
       ([info (all-job-info)])
       (if (equal? active? 'both)
           (let-values ([{active not} (partition job-running? info)]
                        [{find-job*} (match-lambda [(job id _) (find-job id)])])
             (list (map find-job* active)
                   (map find-job* not)))
           (filter-map (match-lambda [(job id (== active?)) (find-job id)]
                                     [else #f])
                       info))))
    (define/private (all-job-info)
      (define condor-dump (system/host/string "condor_q"))
      (match condor-dump
        [(regexp "-- Schedd: peroni.cs.northwestern.edu")
         (define raw-info
           (regexp-match* @pregexp{(?m:^(\S+\s+){6}([1_])\s+([1_])\s+1\s+([\d.]+)$)}
                          condor-dump
                          #:match-select cddr))
         (define all-jobs
           (for/list ([parts (in-list raw-info)])
             (job (third parts) (string=? (first parts) "1"))))
         (expunge-old-jobs! all-jobs)
         (present all-jobs)]
        [else
         absent]))

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
          (match (scp #:from-local job.sub #:to-host host-jobfile-path)
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
                   (void (system/host @~a{condor_rm '@id'}))
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
;; (hash 'completed                         (listof (list/c string? string?))
;;       (or/c 'incomplete 'errored 'other) (listof (list/c string? string? (option/c real?))))

;; host<%> -> (option/c summary/c)
(define (summarize-experiment-status a-host)
  (define (with-progress incomplete-bench)
    (match-define (list name config) incomplete-bench)
    (printf "Retrieving ~a progress ...\r" name)
    (list name config (try-unwrap (get-progress a-host name))))
  (printf "Checking ~a...\r" a-host)
  (option-let*
   ([results (get-results a-host)])
   (for/fold ([results+progress results])
             ([result-kind (in-list '(incomplete errored other))])
     (hash-update results+progress
                  result-kind
                  (λ (benches) (map with-progress benches))))))

(define (stuck-jobs active-jobs maybe-summary)
  (for*/list ([summary (in-option maybe-summary)]
              [incomplete-jobs (in-value (append (hash-ref summary 'incomplete)
                                                 ;; lltodo: this is to handle a
                                                 ;; bug in
                                                 ;; check-experiment-progress.rkt
                                                 (hash-ref summary 'errored)))]
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
                             [maybe-active-jobs (send a-host get-jobs #t)]
                             [maybe-summary (summarize-experiment-status a-host)])
  (for* ([summary (in-option maybe-summary)]
         [active-jobs (in-option maybe-active-jobs)]
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

(define (summary-empty? summary)
  (match summary
    [(hash-table [_ '()] ...) #t]
    [else #f]))
(define (summary-has-errors? summary)
  (match summary
    [(hash-table ['completed _]
                 ['incomplete _]
                 [(or 'errored 'other) '()] ...)
     #f]
    [else #t]))
(define (config/mode-complete? summary)
  (define completed (hash-ref summary 'completed))
  (match completed
    [(list (list benchmarks config-names) ..1)
     (and (= (set-count (apply set config-names)) 1)
          (set=? benchmarks needed-benchmarks/10))]
    [else
     #f]))
(define (download-completed-benchmarks! a-host summary)
  (define completed (hash-ref summary 'completed))
  (match summary
    [(hash-table ['completed (list (list _ config-name) _ ...)]
                 [_ '()] ...)
     #:when (or (config/mode-complete? summary)
                (user-prompt!
                 @~a{
                     Not all benchmarks are completed on @a-host
                     @(pretty-format summary)
                     Do you want to download the results anyway? 
                     }))
     (define projdir (get-field host-project-path a-host))
     (void (send a-host system/host @~a{
                                        cd @projdir && @;
                                        ./pack.sh experiment-output @config-name @;
                                        }))
     (define archive-name (~a config-name ".tar.gz"))
     (match (send a-host scp
                  #:from-host (build-path projdir archive-name)
                  #:to-local data-path)
       [0
        (displayln @~a{Data from @a-host downloaded at @(build-path data-path archive-name)})
        (present (void))]
       [else
        (displayln @~a{Something went wrong downloading data for @a-host})
        absent])]
    [else
     (displayln @~a{Aborting download of inconsistent results:})
     (pretty-display summary)
     absent]))


;; host? (host? (listof jobinfo?) summary? -> any) -> (or/c 'complete 'empty 'error)
(define (wait-for-current-jobs-to-finish host [periodic-action! void])
  (displayln @~a{Waiting for current jobs to finish on @host ...})
  (let loop ()
    (option-let*
     ([summary (summarize-experiment-status host)]
      [jobs (send host get-jobs 'both)])

     (match-define (list active-jobs pending-jobs) jobs)
     (periodic-action! host active-jobs summary)
     (define no-jobs? (and (empty? active-jobs) (empty? pending-jobs)))
     (cond [(and no-jobs? (config/mode-complete? summary))
            'complete]
           [(and no-jobs? (summary-empty? summary))
            'empty]
           [(and no-jobs? (summary-has-errors? summary))
            (notify-phone! @~a{@host has errors})
            (if (user-prompt! @~a{
                                  Found errors on @host, summary:
                                  @(pretty-format summary)
                                  Resume waiting for finish? (no means abort): 
                                  })
                (loop)
                'error)]
           [else
            (printf "Sleeping                    \r")
            (sleep (* 15 60))
            (loop)]))))

(define (help!:continue? notification prompt)
  (notify-phone! notification)
  (user-prompt! prompt))

(define (ensure-host-empty! host)
  (let loop ()
    (unless (equal? (try-unwrap (wait-for-current-jobs-to-finish host restart-stuck-jobs!))
                    'empty)
      (unless (help!:continue?
               @~a{Host @host was not left in a clean state, stuck}
               @~a{
                   Unexpected dirty state on @host, summary:
                   @(pretty-format (summarize-experiment-status host))
                   Is it fixed now? (Say no to abort)
                   })
        (displayln "Aborting.")
        (exit 1))
      (loop))))

(define (update-host! a-host)
  (define host-dbs-dir
    (build-path (get-field host-project-path a-host)
                "blame-evaluation-gt"
                "dbs"))
  (define archive-name (basename dbs-path))
  (define unpacked-db-dir-name
    (for/fold ([name archive-name])
              ([_ (in-range 2)])
      (path-replace-extension name "")))
  (define host-unpacked-db-dir-path
    (build-path host-dbs-dir unpacked-db-dir-name))
  (define host-repo-path
    (build-path (get-field host-project-path a-host)
                "blame-evaluation-gt"))
  (define host-project-raco.rkt-path
    (build-path (get-field host-utilities-path a-host)
                "project-raco.rkt"))
  (define host-setup.rkt-path
    (build-path (get-field host-utilities-path a-host)
                "setup.rkt"))
  (displayln "Uploading latest dbs...")
  (unless (zero? (send a-host scp
                       #:from-local dbs-path
                       #:to-host (~a (build-path host-dbs-dir archive-name))))
    (raise-user-error 'update-host!
                      @~a{Failed to upload db archive to @a-host}))
  (displayln "Done.")
  (for ([msg (in-list '("Unpacking dbs..."
                        "Updating implementation..."
                        "Recompiling and checking status..."))]
        [cmd (in-list (list @~a{
                                rm -r '@host-unpacked-db-dir-path' && @;
                                cd '@host-dbs-dir' && @;
                                tar -xzvf '@archive-name' && @;
                                echo "Done."
                                }
                            @~a{
                                cd '@host-repo-path' && @;
                                git pull && @;
                                echo "Done."
                                }
                            @~a{
                                @(get-field host-racket-path a-host) '@host-project-raco.rkt-path' -Cc && @;
                                @(get-field host-racket-path a-host) '@host-setup.rkt-path' -v && @;
                                echo "Done."
                                }))])
    (displayln msg)
    (unless (send a-host
                  system/host
                  cmd
                  #:interactive? #t)
      (raise-user-error 'update-host!
                        "Update failed."))))

(define (notify-phone! msg)
  (system @~a{fish -c "notify-phone '@msg'"}))

(define (string->value s)
  (with-input-from-string s read))
(define (list->benchmark-spec l)
  (match l
    [(list host-sym config-name-sym benchmark-syms ...)
     (list (host-by-name (~a host-sym))
           (~a config-name-sym)
           (map ~a benchmark-syms))]
    [else (raise-user-error 'experiment-manager
                            @~a{Bad benchmark spec: @~v[l]})]))
(define (string->benchmark-spec s)
  (list->benchmark-spec (string->value s)))
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
                           ['watch-for-stuck-jobs watch-for-stuck-jobs?]
                           ['queue Q-path]
                           ['update (app (mapper host-by-name) update-targets)])
               args]
              #:once-each
              [("-s" "--status")
               'status?
               "Check the experiment status on all active hosts."
               #:record]
              [("-q" "--queue")
               'queue
               ("Oversee the execution of a queue of jobs, downloading the results after each one."
                "The queue is specified in the given file.")
               #:collect ["spec path" take-latest #f]]
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
               #:record]
              [("-u" "--update")
               'update
               "Update the given hosts implementation of the experiment."
               #:collect ["host" cons empty]]}
 #:check [(or (not Q-path) (path-to-existant-file? Q-path))
          @~a{Unable to find queue spec at @Q-path}]

 (define (for-each-target targets action name)
   (for ([target (in-list targets)])
     (match-define (list a-host config-name benchmarks) target)
     (define target-benchmarks (match benchmarks
                                 ['("<all>") needed-benchmarks/10]
                                 [else benchmarks]))
     (for ([benchmark (in-list target-benchmarks)]
           [i (in-naturals)])
       (when (= i 5) (sleep (* 6 60)))
       (if (absent? (action a-host benchmark config-name))
           (displayln @~a{Failed @name @(list a-host config-name benchmark)})
           (displayln @~a{Successful @name @(list a-host config-name benchmark)})))))
 (cond [status?
        (for ([a-host (in-list hosts)])
          (define maybe-summary
            (summarize-experiment-status a-host))
          (displayln @~a{---------- @a-host ----------})
          (match (try-unwrap (send a-host get-jobs 'both))
            [(list active-jobs pending-jobs)
             (displayln @~a{Active: @active-jobs})
             (displayln @~a{Pending: @pending-jobs})]
            [else (void)])
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
          (sleep (* 15 60))
          (loop))]
       [(not (empty? download-targets))
        (for ([a-host (in-list download-targets)])
          (option-let* ([summary (summarize-experiment-status a-host)])
                       (download-completed-benchmarks! a-host summary)))]
       [Q-path
        (define (dequeue-target!)
          (match (file->list Q-path)
            [(list* front remaining)
             (display-lines-to-file (map ~s remaining)
                                    Q-path
                                    #:exists 'replace)
             front]
            [else #f]))
        (let launch-next-target ([retry #f])
          (match (or retry (dequeue-target!))
            [#f (void)]
            [launch-spec-list
             (define launch-spec (list->benchmark-spec launch-spec-list))
             (define host (first launch-spec))
             (ensure-host-empty! host)
             (for-each-target (list launch-spec)
                              (λ (a-host benchmark config-name)
                                (send a-host submit-job! benchmark config-name))
                              "submit")
             (match (try-unwrap (wait-for-current-jobs-to-finish host restart-stuck-jobs!))
               ['complete
                (option-let*
                 ([complete-summary (summarize-experiment-status host)]
                  [_ (download-completed-benchmarks! host complete-summary)])
                 (launch-next-target #f))]
               [else
                #:when (help!:continue?
                        @~a{Unexpected results on @host}
                        @~a{
                            Unexpected results on @host while waiting on @launch-spec-list, summary:
                            @(pretty-format (summarize-experiment-status host))
                            Retry this item?
                            })
                (launch-next-target launch-spec-list)]
               [else (void)])]))]
       [(not (empty? update-targets))
        (for ([a-host (in-list update-targets)])
          (update-host! a-host))]))
