#lang at-exp rscript

(require syntax/parse/define
         racket/date
         "option.rkt")

(define-runtime-paths
  [store-path "../../experiment-data/experiment-manager"]
  [data-path "../../experiment-data/results/code-mutations"]
  [dbs-path "../../experiment-data/dbs/code-mutations.tar.gz"])

;; if it's not running?, it's pending
(struct job (id running?) #:transparent)
(define job-info? (or/c (list/c string? string?)
                        (list/c string? string? real?)))
(define host<%> (interface (writable<%>)
                  ;; provided by host%
                  [system/host (unconstrained-domain-> boolean?)]
                  [system/host/string (unconstrained-domain-> string?)]
                  [scp (->*m {}
                             {#:from-host (or/c path-string? #f)
                              #:to-local (or/c path-string? #f)
                              #:from-local (or/c path-string? #f)
                              #:to-host (or/c path-string? #f)}
                             natural?)]

                  ;; must be implemented
                  [get-jobs
                   (let ([job-descr/c (listof (option/c (list/c string? string?)))])
                     (->*m {}
                           {(or/c boolean? 'both)}
                           (option/c
                            (or/c job-descr/c
                                  (list/c job-descr/c job-descr/c)))))]
                  [submit-job! (->*m {string?
                                      string?}
                                     {#:mode (or/c 'check 'record)
                                      #:cpus (or/c "decide" natural?)
                                      #:name string?}
                                     any)]
                  [cancel-job! (->m string? string? any)]))
(define host% (class object%
                (super-new)
                (init-field hostname
                            host-project-path)
                (field [data-store-path (build-path store-path (~a hostname ".rktd"))]
                       [host-racket-path (build-path host-project-path "racket" "bin" "racket")]
                       [host-utilities-path (build-path host-project-path "blame-evaluation-gt" "util")]
                       [host-data-path (build-path host-project-path "experiment-output")])
                (define/public (custom-write port) (write hostname port))
                (define/public (custom-display port) (display hostname port))

                (define/public (system/host #:interactive? [interactive? #f] . parts)
                  ;; lltodo: implement a persistent connection here to prevent being blocked
                  ;; by zythos for opening too many connections too quickly
                  ;; > Tried this and gave up after a few hours. It's hard.
                  ;; Instead, I should think about batching these calls higher up in the logic.
                  (define cmd-str (apply ~a (add-between parts " ")))
                  (define cmd-str-escaped (string-replace cmd-str "\"" "\\\""))
                  (system @~a{ssh @(if interactive? "-t" "") @hostname "@cmd-str-escaped"}))

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
                                 [{_ _ _ _} (raise-user-error 'scp "Bad argument combination")])}))))
(define condor-host%
  (class* host% (writable<%> host<%>)
    (super-new)
    (inherit system/host
             system/host/string
             scp)
    (inherit-field hostname
                   host-project-path
                   data-store-path
                   host-racket-path
                   host-utilities-path
                   host-data-path)
    (init-field [host-jobdir-path "."])
    (field [host-jobfile-path (build-path host-jobdir-path "job.sub")])

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
         all-jobs]
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

    (define/private (save-job! benchmark config-name id)
      (ensure-store!)
      (with-output-to-file data-store-path #:exists 'append
        (thunk (writeln (cons (list benchmark config-name) id)))))
    (define/private (read-job benchmark config-name) ; -> (option/c id?)
      (dict-ref (read-data-store)
                (list benchmark config-name)
                absent))
    (define/private (find-job target-id) ; -> (option/c (list/c benchmark config-name))
      (ensure-store!)
      (for*/option ([{bench+config id} (in-dict (file->list data-store-path))]
                    #:when (string=? id target-id))
                   bench+config))

    (define/private (ensure-store!)
      (make-directory* (path-only data-store-path))
      (unless (file-exists? data-store-path)
        (system @~a{touch '@data-store-path'})))
    (define/private (read-data-store)
      (file->list data-store-path))
    (define/private (write-data-store! data)
      (display-lines-to-file (map ~s data)
                             data-store-path
                             #:exists 'replace))))
(define (bool->option v)
  (or v absent))
(define direct-access-host%
  (class* host% (writable<%> host<%>)
    (super-new)
    (inherit system/host
             system/host/string
             scp)
    (inherit-field hostname
                   data-store-path
                   host-project-path
                   host-racket-path
                   host-utilities-path
                   host-data-path)
    (init-field [cpu-count 1])
    (field [run-screen-name "experiment-run"]
           [management-screen-name "experiment-manage"]
           [queueing-thd (make-direct-access-host-queue-manager)])

    (define/public (get-jobs [active? #t] #:with-pid? [with-pid? #f])
      (option-let*
       ([active (match (system/host/string "ps -ef | grep run-experiment.sh")
                  [(regexp @pregexp{(?m:^\S+\s+(\d+)\s+(\S+\s+){5}/bin/bash .*run-experiment.sh (\S+) (\S+).rkt)}
                           (list _ pid _ benchmark config-name))
                   (list (list* benchmark config-name (if with-pid? (list pid) empty)))]
                  [(regexp #px"^llazarek") empty]
                  [else absent])])

       (match active?
         [#t active]
         ['both (list active (get-queued-jobs))]
         [else (get-queued-jobs)])))

    (define/private (get-queued-jobs)
      (map (match-lambda [(list* benchmark config-name _) (list benchmark config-name)])
           (read-data-store)))

    (define/public (submit-job! benchmark
                                config-name ; without .rkt
                                #:mode [record/check-mode 'check]
                                #:cpus [cpus cpu-count]
                                #:name [name benchmark])
      (option-let*
       ([_ (thread-send queueing-thd
                        `(submit ,(list benchmark config-name record/check-mode cpus name))
                        (thunk absent))])
       (void)))
    (define/public (cancel-job! benchmark config-name)
      (option-let*
       ([_ (thread-send queueing-thd
                        `(cancel ,(list benchmark config-name))
                        (thunk absent))])
       (void)))

    (define/private (launch-job! benchmark
                                 config-name ; without .rkt
                                 record/check-mode
                                 cpus
                                 name)
      (define run-cmd
        @~a{
            cd ~/blgt; @;
            ./run-experiment.sh "@benchmark" "@|config-name|.rkt" @record/check-mode @cpus "@name"
            })
      (option-let*
       ([_ (ensure-screen-setup!)]
        [_ (bool->option
            (system/host @~a{
                             screen -S @run-screen-name -p 0 -X stuff '\'@|run-cmd|\r\''
                             }))])
       (void)))
    (define/private (cancel-currently-running-job!)
      (option-let*
       ([active-jobs (get-jobs #t #:with-pid? #t)]
        [_ (bool->option (not (empty? active-jobs)))]
        [pid (match active-jobs
               [`((,_ ,_ ,pid)) pid]
               [else absent])]
        [_ (bool->option (system/host @~a{kill @pid}))])
       (void))
      #;(option-let*
       ([_ (ensure-screen-setup!)]
        [_ (bool->option
            (system/host @~a{
                             screen -S @run-screen-name -p 0 -X stuff '\'^C\''
                             }))]
        [active-jobs (get-jobs #t)]
        [_ (bool->option (empty? active-jobs))])
       (void)))

    (define/private (ensure-screen-setup!)
      (define screens-output (system/host/string "screen -ls"))
      (define (session-exists? name)
        (regexp-match? @~a{[0-9]+\.@name} screens-output))
      (define (launch-screen! name)
        (system/host @~a{screen -dmS @name}))
      (define run-ok?
        (unless (session-exists? run-screen-name) (launch-screen! run-screen-name)))
      (define management-ok?
        (unless (session-exists? management-screen-name) (launch-screen! management-screen-name)))
      (cond [(and run-ok? management-ok?) (void)]
            [else
             (displayln @~a{Failed to obtain necessary screen sessions})
             absent]))

    (define/private (make-direct-access-host-queue-manager)
      (thread
       (thunk
        (define message-evt (thread-receive-evt))

        (define (enqueue-job! spec)
          (with-data-store-lock
            (thunk (write-data-store! (append (read-data-store)
                                              (list spec))))
            (thunk (displayln @~a{Failed to enqueu job @spec, couldn't get data store lock}))))
        (define (cancel-job! id)
          (match-define (list benchmark config-name) id)
          (with-data-store-lock
            (thunk
             (match (get-jobs #t)
               [(list running-job-id)
                #:when (equal? running-job-id id)
                (cancel-currently-running-job!)]
               [(? list?)
                (define current-q (read-data-store))
                (define new-q (remf (match-lambda [(list* (== benchmark) (== config-name) _) #t]
                                                  [else #f])
                                    current-q))
                (write-data-store! new-q)]
               [other
                (displayln @~a{Failed to cancel job @id, current-jobs: @other})]))
            (thunk (displayln @~a{Failed to cancel job @id, couldn't get data store lock}))))

        (define (current-job-done?)
          (empty? (get-jobs #t)))

        (define (queue-empty?)
          (empty? (read-data-store)))

        (define (launch-next-job!)
          (with-data-store-lock
            (thunk (define current-q (read-data-store))
                   (define new-q (rest current-q))
                   (define job-info (first current-q))
                   (send this launch-job! . job-info))
            (thunk (displayln @~a{
                                  Warning: couldn't launch next job @;
                                  because couldn't get data store lock
                                  }))))

        (let loop ()
          (cond [(thread-try-receive)
                 => (match-lambda [`(submit ,job-spec) (enqueue-job! job-spec)]
                                  [`(cancel ,job-id)   (cancel-job! job-id)])]
                [else
                 (if (and (current-job-done?)
                          (not (queue-empty?)))
                     (launch-next-job!)
                     (sync/timeout (* 5 60) message-evt))])
          (loop)))))

    (define/private (ensure-store!)
      (make-directory* (path-only data-store-path))
      (unless (file-exists? data-store-path)
        (system @~a{touch '@data-store-path'})))
    ;; -> (listof any/c)
    (define/private (read-data-store)
      (ensure-store!)
      (file->list data-store-path))
    ;; (listof any/c) -> void
    (define/private (write-data-store! data-list)
      (display-lines-to-file (map ~s data-list)
                             data-store-path
                             #:exists 'replace))
    (define/private (with-data-store-lock thunk fail-thunk)
      (call-with-file-lock/timeout data-store-path
                                   'exclusive
                                   thunk
                                   fail-thunk
                                   #:max-delay 1))))

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
(define benbox (new direct-access-host%
                    [hostname "benbox"]
                    [host-project-path "./blgt"]))
(define hosts (list zythos benbox))

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
     (call-with-input-string info-str read)]
    [else
     (eprintf @~a{
                  Unable to get experiment results summary for host @a-host, @;
                  found: @~v[info-str]

                  })
     absent]))

(define (try-infer-benchmark-from-data-name benchmark-data-name)
  (define ((prefix-or-suffix-of str) maybe-pre-or-suffix)
    (or (string-prefix? str maybe-pre-or-suffix)
        (string-suffix? str maybe-pre-or-suffix)))
  (findf (prefix-or-suffix-of benchmark-data-name)
         needed-benchmarks/10))

;; host? (listof string?) -> (option/c (listof (option/c (and/c real? (between/c 0 1)))))
(define (get-progress a-host . benchmarks)
  #;(define benchmark
    (if (member benchmark needed-benchmarks/10)
        benchmark
        (try-infer-benchmark-from-data-name benchmark)))
  (define benchmark-paths (for/list ([benchmark (in-list benchmarks)])
                            (build-path (get-field host-data-path a-host) benchmark)))
  (define progress-str
    (send a-host
          system/host/string
          (get-field host-racket-path a-host)
          (build-path (get-field host-utilities-path a-host) "check-experiment-progress.rkt")
          "-r"
          .
          benchmark-paths))
  (define progresses (string->value progress-str))
  (if (list? progresses)
      (for/list ([% (in-list progresses)])
        (match %
          [(? number? n) n]
          [else absent]))
      absent))

;; summary/c :=
;; (hash 'completed                         (listof (list/c string? string?))
;;       (or/c 'incomplete 'errored 'other) (listof (list/c string? string? (option/c real?))))

;; host<%> -> (option/c summary/c)
(define (summarize-experiment-status a-host)
  (define (add-progress incomplete-benchs)
    (match-define (list (list names _) ...) incomplete-benchs)
    (define progresses (apply get-progress a-host names))
    (for/list ([job-id (in-list incomplete-benchs)]
               [progress (in-list (if (absent? progresses)
                                      (make-list (length incomplete-benchs) absent)
                                      progresses))])
      (append job-id (list progress))))
  (printf "Checking ~a...\r" a-host)
  (option-let*
   ([results (get-results a-host)])
   (for/fold ([results+progress results])
             ([result-kind (in-list '(incomplete errored other))])
     (hash-update results+progress
                  result-kind
                  add-progress))))

(define (stuck-jobs active-jobs maybe-summary)
  (for*/list ([summary (in-option maybe-summary)]
              [incomplete-jobs (in-value (append (hash-ref summary 'incomplete)
                                                 ;; lltodo: this is to handle a
                                                 ;; bug in
                                                 ;; check-experiment-progress.rkt
                                                 (hash-ref summary 'errored)))]
              [maybe-benchmark (in-list active-jobs)]
              [benchmark (in-option maybe-benchmark)]
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
(define (missing-completed-benchmarks summary)
  (define completed (hash-ref summary 'completed))
  (define completed-benchmarks (map first completed))
  (set-subtract needed-benchmarks/10 completed-benchmarks))
(define (config/mode-complete? summary)
  (define completed (hash-ref summary 'completed))
  (match completed
    [(list (list _ config-names) ..1)
     (and (= (set-count (apply set config-names)) 1)
          (empty? (missing-completed-benchmarks summary)))]
    [else #f]))
(define (download-completed-benchmarks! a-host summary)
  (define (download-results! archive-name)
    (when (string=? archive-name "")
      (raise-user-error 'download-completed-benchmarks!
                        "Can't download results with empty archive name"))
    (define projdir (get-field host-project-path a-host))
    (void (send a-host system/host @~a{
                                       cd @projdir && @;
                                       ./pack.sh experiment-output @archive-name @;
                                       }))
    (define archive-name+ext (~a archive-name ".tar.gz"))
    (match (send a-host scp
                 #:from-host (build-path projdir archive-name+ext)
                 #:to-local data-path)
      [0
       (displayln @~a{Data from @a-host downloaded at @(build-path data-path archive-name+ext)})
       (void)]
      [else
       (displayln @~a{Something went wrong downloading data for @a-host})
       absent]))
  (match summary
    [(hash-table ['completed (list (list* _ config-name _) _ ...)]
                 [_ '()] ...)
     #:when (or (config/mode-complete? summary)
                (user-prompt!
                 @~a{
                     Not all benchmarks are completed on @a-host
                     @(format-status a-host summary)
                     Missing: @(missing-completed-benchmarks summary)
                     Do you want to download the results anyway? 
                     }))
     (download-results! config-name)]
    [else
     #:when (user-prompt!
             @~a{
                 @a-host results are not all successfully completed:
                 @(format-status a-host summary)
                 Missing: @(missing-completed-benchmarks summary)
                 Do you want to download the results anyway? 
                 })
     (displayln "Enter the desired archive name (no extension):")
     (download-results! (read-line))]
    [else
     (displayln @~a{Aborting download of inconsistent results:})
     (displayln (format-status a-host summary))
     absent]))


;; host? (host? (listof jobinfo?) summary? -> any) -> (or/c 'complete 'empty 'error)
(define (wait-for-current-jobs-to-finish host
                                         [periodic-action! void]
                                         #:period [sleep-period 15])
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
            (if (help!:continue? @~a{@host has errors}
                                 @~a{
                                     Found errors on @host, summary:
                                     @(format-status host summary)
                                     Resume waiting for finish? (no means abort): 
                                     })
                (loop)
                'error)]
           [else
            (printf "~a Sleeping for ~a min                   \r"
                    (date->string (current-date) #t)
                    sleep-period)
            (sleep (* sleep-period 60))
            (loop)]))))

(define (help!:continue? notification prompt)
  (notify-phone! notification)
  (user-prompt! prompt))

(define (ensure-host-empty! host)
  (let loop ()
    (unless (equal? (wait-for-current-jobs-to-finish host restart-stuck-jobs!)
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

(define (format-status a-host
                       [summary* (summarize-experiment-status a-host)]
                       [active+pending-jobs* (send a-host get-jobs 'both)])
  (option-let*
   ([summary summary*]
    [active+pending-jobs active+pending-jobs*])

   (with-output-to-string
     (thunk
      (match-define (list active-job-ids pending-job-ids) active+pending-jobs)
      ;; listof(job-id, active?, progress, errorflag?)
      (define all-job-ids
        (set-union active-job-ids
                   pending-job-ids
                   (for*/list ([{_ jobs} (in-hash summary)]
                               [job-id* (in-list jobs)])
                     (match job-id*
                       [(list* bench mode _) (list bench mode)]))))
      (define all-job-info
        (for*/list ([maybe-job-id (in-list all-job-ids)]
                    [job-id (in-option maybe-job-id)])
          (define active? (member job-id active-job-ids))
          (define pending? (member job-id pending-job-ids))
          (define status (cond [active?  "R"]
                               [pending? "W"]
                               [else     "-"]))
          (match-define (list bench mode) job-id)
          (define-values {progress check-for-errors?}
            (match summary
              [(hash-table ['completed (list-no-order (== job-id) _ ...)]
                           _ ...)
               (values 1 #f)]
              [(hash-table [(and (or 'errored 'incomplete) category)
                            (list-no-order (list (== bench) (== mode) %) _ ...)]
                           _ ...)
               (values % (equal? category 'errored))]
              [(hash-table ['other (list-no-order (== job-id) _ ...)]
                           _ ...)
               (values 0 #t)]
              [else (values 0 #f)]))
          (list job-id status progress check-for-errors?)))
      (define (render-jobs jobs)
        (for ([job (in-list jobs)]
              [i (in-naturals)])
          (define prefix (if (zero? i) "" "\n         "))
          (match-define (list job-id status progress check-for-errors?) job)
          (display (~a prefix
                       (fixed-width-format job-id 40)
                       "  "
                       (match* {status progress}
                         [{"-" 1} "✓"]
                         [{"-" _} "?"]
                         [{"R" _} "R"]
                         [{"W" _} "W"])
                       "  "
                       (if (absent? progress)
                           "<no progress available>"
                           (render-progress progress status))
                       "  "
                       (if check-for-errors? "⚠" " ")))))
      (define (render-progress % status)
        (define bar-char (match status
                           ["R" #\▬]
                           [else #\▭]))
        (define end-char (match status
                           ["R" #\▶]
                           [else #\▭]))
        (define bar-size 20)
        (define progress-chars (inexact->exact
                                (round (* % bar-size))))
        (~a "▕"
            (fixed-width-format
             (match progress-chars
               [0 ""]
               [else (~a (make-string (sub1 progress-chars) bar-char) end-char)])
             bar-size)
            "▏"
            (if (= % 1)
                ""
                (~a (~r (* % 100) #:precision 1 #:min-width 4) "%"))))
      (define (fixed-width-format v width)
        (define content (~a v))
        (~a content (make-string (max 0 (- width (string-length content))) #\space)))
      (display "Active   ")
      (render-jobs (filter (match-lambda [(list _ "R" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)
      (display "Pending  ")
      (render-jobs (filter (match-lambda [(list _ "W" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)
      (display "Inactive ")
      (render-jobs (filter (match-lambda [(list _ "-" _ _) #t]
                                         [else #f])
                           all-job-info))
      (newline)))))

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
                           ['resume-queue resume-Q/host]
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
              [("-r" "--resume")
               'resume-queue
               ("Resume overseeing the queue specified by -q, which see."
                "Only has an effect with -q.")
               #:collect ["host" take-latest #f]]
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
       ;; lltodo: the submission here can be batched
       (when (= i 5) (sleep (* 6 60)))
       (if (absent? (action a-host benchmark config-name))
           (displayln @~a{Failed @name @(list a-host config-name benchmark)})
           (displayln @~a{Successful @name @(list a-host config-name benchmark)})))))
 (cond [status?
        (for ([a-host (in-list hosts)])
          (displayln @~a{---------- @a-host ----------})
          (displayln (format-status a-host)))]
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
        (define (wait-for-jobs-to-finish+download-results host waiting-job)
          (match (wait-for-current-jobs-to-finish host restart-stuck-jobs!)
            ['complete
             (option-let*
              ([complete-summary (summarize-experiment-status host)]
               [_ (download-completed-benchmarks! host complete-summary)])
              'ok)]
            [else
             #:when (help!:continue?
                     @~a{Unexpected results on @host}
                     @~a{
                         Unexpected results on @host while waiting on @waiting-job
                         Current status:
                         @(format-status host)
                         Retry this item?
                         })
             'retry-job]
            [else 'stop]))
        (when resume-Q/host
          (define host (host-by-name resume-Q/host))
          (define status (wait-for-jobs-to-finish+download-results host
                                                                   (~a "resuming " Q-path)))
          (unless (or (equal? status 'ok)
                      (help!:continue?
                       @~a{Unexpected results on @host}
                       @~a{
                           Unexpected results on @host while resuming @Q-path, summary:
                           @(format-status host)
                           Continue with the rest of the Q? (no means abort)
                           }))
            (displayln "Aborting")
            (exit 1)))
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
             (match (wait-for-jobs-to-finish+download-results host launch-spec-list)
               ['ok (launch-next-target #f)]
               ['retry-job (launch-next-target launch-spec-list)]
               [else
                (displayln "Aborting queue.")
                (void)])]))]
       [(not (empty? update-targets))
        (for ([a-host (in-list update-targets)])
          (update-host! a-host))]))
