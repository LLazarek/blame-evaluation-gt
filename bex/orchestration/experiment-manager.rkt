#lang at-exp rscript

(provide update-host!
         check-host-empty!
         launch-benchmarks!
         wait-for-current-jobs-to-finish
         download-results!
         format-status
         help!:continue?
         notify-phone!

         zythos
         zythos-ssh/one-job-per-mutant
         zythos-local/one-job-per-mode
         zythos-local/one-job-per-mutant/batched
         benbox
         local)

(require syntax/parse/define
         racket/date
         "../util/option.rkt"
         "experiment-info.rkt")

(define-runtime-paths
  [store-path "../../../experiment-data/experiment-manager"]
  [std-experiment-runner-template "./standard-experiment-runner-script-template.sh"]
  [experiment-info.rkt "experiment-info.rkt"]
  [project-path "../../.."])

(define-logger experiment-manager)

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

                  ;; may be implemented, must be called before submitting or canceling jobs
                  ;; is idempotent
                  [setup-job-management! (->m any)]

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
(define host%
  (class object%
    (super-new)
    (init-field hostname
                host-project-path)
    (field [data-store-path (build-path store-path (~a hostname ".rktd"))]
           [host-racket-path (build-path host-project-path "racket" "bin" "racket")]
           [host-utilities-path
            (build-path host-project-path "blame-evaluation-gt" "bex" "util")]
           [host-data-path (build-path host-project-path "experiment-output")]
           [host-experiment-runner-script-path
            (build-path host-project-path "generated-run-experiment.sh")]
           [host-experiment-runner-script-uploaded? #f])
    (define/public (custom-write port) (write hostname port))
    (define/public (custom-display port) (display hostname port))

    (define/public (setup-job-management!) (void))

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
                     [{_ _ _ _} (raise-user-error 'scp "Bad argument combination")])}))
    (define/public (upload-experiment-script!)
      (or host-experiment-runner-script-uploaded?
          (with-temp-file run-experiment.sh
            (display-to-file
             (string-replace (file->string std-experiment-runner-template)
                             "<<project-path>>"
                             (~a host-project-path))
             run-experiment.sh
             #:exists 'replace)
            (and/option
             (check-success
              (send this scp
                    #:from-local run-experiment.sh
                    #:to-host host-experiment-runner-script-path))
             (check-success
              (send this system/host @~a{chmod u+x @host-experiment-runner-script-path}))
             (set! host-experiment-runner-script-uploaded? #t)))))

    (define/public (make-experiment-runner-script-args benchmark
                                                       config-name ; without .rkt
                                                       record/check-mode
                                                       cpus
                                                       name)
      @~a{
          '@benchmark' @;
          '@|config-name|.rkt' @;
          '@record/check-mode' @;
          '@(current-remote-host-db-installation-directory-name)' @;
          '@cpus' @;
          '@name'
          })

    (define/public (ensure-store!)
      (make-directory* (path-only data-store-path))
      (unless (file-exists? data-store-path)
        (system @~a{touch '@data-store-path'})))
    (define/public (read-data-store)
      (ensure-store!)
      (file->list data-store-path))
    (define/public (write-data-store! data)
      (display-lines-to-file (map ~s data)
                             data-store-path
                             #:exists 'replace))))
;; runs each mode as a job, submitting all modes at once
(define condor-host%
  (class* host% (writable<%> host<%>)
    (super-new)
    (inherit system/host
             system/host/string
             scp
             upload-experiment-script!
             make-experiment-runner-script-args
             ensure-store!
             read-data-store
             write-data-store!)
    (inherit-field hostname
                   host-project-path
                   data-store-path
                   host-racket-path
                   host-utilities-path
                   host-data-path
                   host-experiment-runner-script-path)
    (init-field [host-jobdir-path "."])
    (field [host-jobfile-path (build-path host-jobdir-path "job.sub")]
           [enabled-machines '("fix" "allagash" "piraat")])

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
           (regexp-match* @pregexp{([1_])\s+[1_]\s+1 (\d{7}\.0)}
                          condor-dump
                          #:match-select rest))
         (define all-jobs
           (for/list ([parts (in-list raw-info)])
             (job (second parts) (string=? (first parts) "1"))))
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
           @~a{
               # Set the universe
               Universe = vanilla

               # Describe the target machine
               Requirements = (@(string-join (for/list ([name (in-list enabled-machines)])
                                               @~a{(Machine == "@|name|.cs.northwestern.edu")})
                                             " || "))

               Rank = TARGET.Mips
               Copy_To_Spool = False

               # Notification
               Notification = never

               # Set the environment
               Getenv = True

               Arguments = "@(make-experiment-runner-script-args benchmark config-name record/check-mode cpus name)"
               Executable = @host-experiment-runner-script-path
               Error = condor-output.txt
               Output = condor-output.txt
               Log = condor-log.txt

               +IsWholeMachineJob = true
               +IsSuspensionJob = false

               Queue

               }
           job.sub
           #:exists 'replace)
          (check-success (scp #:from-local job.sub #:to-host host-jobfile-path))))
      (option-let* ([_ job-uploaded?]
                    [_ (upload-experiment-script!)]
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
                   bench+config))))
(define (check-success exit-code/bool)
  (match exit-code/bool
    [(or 0 #t) #t]
    [else absent]))
(define (bool->option v)
  (or v absent))
;; runs one mode of the experiment at a time, each having all `cpu-count` cpus
(define direct-access-host%
  (class* host% (writable<%> host<%>)
    (super-new)
    (inherit system/host
             system/host/string
             scp
             upload-experiment-script!
             make-experiment-runner-script-args
             ensure-store!
             read-data-store
             write-data-store!)
    (inherit-field hostname
                   data-store-path
                   host-project-path
                   host-racket-path
                   host-utilities-path
                   host-data-path
                   host-experiment-runner-script-path)
    (init-field [cpu-count 1]
                [env-vars ""])
    (field [run-screen-name "experiment-run"]
           [management-screen-name "experiment-manage"]
           [queueing-thd #f])

    (define/override (setup-job-management!)
      (unless (thread? queueing-thd)
        (when (and (not (empty? (read-data-store)))
                   (user-prompt!
                    @~a{
                        Found existing job data in persistent queue @;
                        (at @data-store-path). @;
                        Do you want to clear it before continuing? @;
                        (Answering no means the old job data will be executed *before* any new.)
                        }))
          (write-data-store! empty))
        (set! queueing-thd (make-direct-access-host-queue-manager))))

    (define experiment-script-name (basename host-experiment-runner-script-path))
    (define/public (get-jobs [active? #t] #:with-pid? [with-pid? #f])
      (option-let*
       ([active (match (system/host/string @~a{ps -ef | grep @experiment-script-name})
                  [(regexp (pregexp @~a{(?m:^\S+\s+(\d+)\s+(\S+\s+){5}/bin/bash .*@experiment-script-name (\S+) (\S+).rkt)})
                           (list _ script-pid _ benchmark config-name))
                   (list (list* benchmark
                                config-name
                                (if with-pid?
                                    ;; get the mutant-factory pid, since that's what actually needs to be killed to cancel the job
                                    (regexp-match* (pregexp @~a{\s(\d+)\s+@script-pid .*mutant-factory.rkt})
                                                   (system/host/string @~a{ps -ef | grep @script-pid})
                                                   #:match-select cadr)
                                    empty)))]
                  [(regexp (pregexp @~a{grep[^@"\n"]+@experiment-script-name})) empty]
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
      (setup-job-management!)
      (option-let*
       ([_ (thread-send queueing-thd
                        `(submit ,(list benchmark config-name record/check-mode cpus name))
                        (thunk absent))])
       (thread-receive)))
    (define/public (cancel-job! benchmark config-name)
      (setup-job-management!)
      (option-let*
       ([_ (thread-send queueing-thd
                        `(cancel ,(list benchmark config-name))
                        (thunk absent))])
       (thread-receive)))

    (define/private (launch-job! benchmark
                                 config-name ; without .rkt
                                 record/check-mode
                                 cpus
                                 name)
      (define run-cmd
        @~a{
            @env-vars @;
            '@host-experiment-runner-script-path' @;
            @(make-experiment-runner-script-args benchmark config-name record/check-mode cpus name)
            })
      (log-experiment-manager-debug @~a{launching job with cmd: @run-cmd})
      (option-let*
       ([_ (ensure-screen-setup!)]
        [_ (upload-experiment-script!)]
        [_ (check-success
            (system/host @~a{screen -S @run-screen-name -p 0 -X stuff "@|run-cmd|\r"}))])
       (void)))
    (define/private (cancel-currently-running-job!)
      (option-let*
       ([active-jobs (get-jobs #t #:with-pid? #t)]
        [_ (bool->option (not (empty? active-jobs)))]
        [pid (match active-jobs
               [`((,_ ,_ ,pid)) pid]
               [else
                (log-experiment-manager-error
                 @~a{unexpected shape of active jobs: @~s[active-jobs]})
                absent])]
        [_ (log-experiment-manager-debug @~a{killing active job with pid @pid})]
        [_ (check-success (system/host @~a{kill @pid}))])
       (void)))

    (define/private (ensure-screen-setup!)
      (define screens-output (system/host/string "screen -ls"))
      (define (session-exists? name)
        (regexp-match? @~a{[0-9]+\.@name} screens-output))
      (define (launch-screen! name)
        (system/host @~a{screen -dmS @name bash}))
      (define run-ok?
        (unless (session-exists? run-screen-name) (launch-screen! run-screen-name)))
      (define management-ok?
        (unless (session-exists? management-screen-name) (launch-screen! management-screen-name)))
      (cond [(and run-ok? management-ok?) (void)]
            [else
             (displayln @~a{Failed to obtain necessary screen sessions})
             absent]))

    (define/private (make-direct-access-host-queue-manager)
      (define main-thd (current-thread))
      (thread
       (thunk
        (define message-evt (thread-receive-evt))

        (define (enqueue-job! spec)
          (with-data-store-lock
            (thunk (write-data-store! (append (read-data-store)
                                              (list spec))))
            (thunk (displayln @~a{Failed to enqueue job @spec, couldn't get data store lock}))))
        (define (cancel-job! id)
          (log-experiment-manager-debug @~a{canceling job @id})
          (match-define (list benchmark config-name) id)
          (with-data-store-lock
            (thunk
             (match (get-jobs #t)
               [(list running-job-id)
                #:when (equal? running-job-id id)
                (log-experiment-manager-debug @~a{... which is currently running})
                (cancel-currently-running-job!)]
               [(? list? running-jobs)
                (log-experiment-manager-debug @~a{... which is still in the queue (running: @running-jobs)})
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
          (log-experiment-manager-debug @~a{@hostname launching next job})
          (with-data-store-lock
            (thunk (define current-q (read-data-store))
                   (define new-q (rest current-q))
                   ;; This unpacking is necessary because apparently there's no
                   ;; way to do an `apply`-type application of a private method.
                   (match-define (list benchmark config-name record/check-mode cpus name)
                     (first current-q))
                   (launch-job! benchmark config-name record/check-mode cpus name)
                   (write-data-store! new-q))
            (thunk (displayln @~a{
                                  Warning: couldn't launch next job @;
                                  because couldn't get data store lock
                                  }))))

        (log-experiment-manager-debug @~a{@hostname direct-access queue thread launched})
        (let loop ()
          (match (thread-try-receive)
            [`(submit ,job-spec)
             (enqueue-job! job-spec)
             (thread-send main-thd #t)
             (log-experiment-manager-debug @~a{@hostname received @job-spec})]
            [`(cancel ,job-id)
             (cancel-job! job-id)
             (thread-send main-thd #t)
             (log-experiment-manager-debug @~a{@hostname canceled @job-id})]
            [else (void)])
          (if (and (current-job-done?)
                   (not (queue-empty?)))
              (launch-next-job!)
              (sync/timeout (* 5 60) message-evt))
          (loop)))))

    (define/private (with-data-store-lock thunk fail-thunk)
      (call-with-file-lock/timeout data-store-path
                                   'exclusive
                                   thunk
                                   fail-thunk
                                   #:max-delay 1))))

(define (local-version-mixin c)
  (class c
    (super-new)
    (define/override (system/host #:interactive? [interactive? #f] . parts)
      (define cmd-str (apply ~a (add-between parts " ")))
      (system cmd-str))
    (define/override (system/host/string #:interactive? [interactive? #f] . parts)
      (define out-str (open-output-string))
      (parameterize ([current-output-port out-str]
                     [current-error-port out-str])
        (system/host #:interactive? interactive? . parts))
      (get-output-string out-str))
    (define/override (scp #:from-host [from-host-path #f]
                          #:to-local [to-local-path #f]
                          #:from-local [from-local-path #f]
                          #:to-host [to-host-path #f])
      (match (list from-host-path
                   to-local-path
                   from-local-path
                   to-host-path)
        [(or (list from to #f #f)
             (list #f #f from to))
         (system/exit-code @~a{cp -r '@from' '@to'})]
        [else (raise-user-error 'scp "Bad argument combination")]))))

(define local-direct-host% (local-version-mixin direct-access-host%))
(define local-condor-host% (local-version-mixin condor-host%))

(define-simple-macro (with-temp-file name body ...)
  (call-with-temp-file (λ (name) body ...)))
(define (call-with-temp-file f)
  (define temp (make-temporary-file))
  (begin0 (f temp)
    (when (file-exists? temp) (delete-file temp))))

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
         experiment-benchmarks))

;; host? (listof string?) -> (option/c (listof (option/c (and/c real? (between/c 0 1)))))
(define (get-progress a-host . benchmarks)
  #;(define benchmark
    (if (member benchmark experiment-benchmarks)
        benchmark
        (try-infer-benchmark-from-data-name benchmark)))
  (cond [(empty? benchmarks)
         empty]
        [else
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
             absent)]))

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
                              (? (>/c 0.97)))
                        #t]
                       [else #f]))
    benchmark))

(define (restart-job! a-host job-info)
  (match-define (list benchmark config) job-info)
  (option-let* ([_ (send a-host cancel-job! benchmark config)]
                [_ (send a-host submit-job! benchmark config)])
               (void)))

(define job-restart-history (make-hash))
(define (restart-stuck-jobs! a-host
                             [maybe-jobs (send a-host get-jobs 'both)]
                             [maybe-summary (summarize-experiment-status a-host)])
  (define (should-restart? job-info)
    (match-define (list restart-count skips-so-far)
      (hash-ref job-restart-history
                job-info
                (list 0 0)))
    (= restart-count skips-so-far))
  (define (restart+record! job-info)
    (displayln @~a{@(date->string (current-date) #t) Restarting stuck job: @job-info})
    (restart-job! a-host job-info)
    (hash-update! job-restart-history
                  job-info
                  (match-lambda [(list restart-count _) (list (add1 restart-count) 0)])
                  (list 0 0)))
  (define (record-skipped-start! job-info)
    (hash-update! job-restart-history
                  job-info
                  (match-lambda [(list restart-count skips) (list restart-count (add1 skips))])
                  (list 0 0)))

  (for* ([summary (in-option maybe-summary)]
         [jobs (in-option maybe-jobs)]
         [job-info (in-list (stuck-jobs (first jobs) summary))])
    (if (should-restart? job-info)
        (restart+record! job-info)
        (record-skipped-start! job-info))))

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
(define (missing-completed-benchmarks summary expected-benchmarks)
  (define completed (hash-ref summary 'completed))
  (define completed-benchmarks (map first completed))
  (set-subtract expected-benchmarks completed-benchmarks))
(define (config/mode-complete? summary expected-benchmarks)
  (define completed (hash-ref summary 'completed))
  (match completed
    [(list (list _ config-names) ..1)
     (and (= (set-count (apply set config-names)) 1)
          (empty? (missing-completed-benchmarks summary expected-benchmarks)))]
    [else #f]))
(define (download-completed-benchmarks! a-host summary download-directory
                                        #:name [name #f]
                                        #:expected-benchmarks [expected-benchmarks experiment-benchmarks])
  (define (download-results! archive-name
                             #:include-configuration-outcomes? [include-configuration-outcomes? #f])
    (when (string=? archive-name "")
      (raise-user-error 'download-completed-benchmarks!
                        "Can't download results with empty archive name"))
    (define projdir (get-field host-project-path a-host))
    (when (and include-configuration-outcomes?
               (false? (current-remote-host-db-installation-directory-name)))
      (raise-user-error
       'download-completed-benchmarks!
       "No experiment config selected, `current-remote-host-db-installation-directory-name` is not configured."))
    (void (send a-host
                system/host
                @~a{
                    cd @projdir && @;
                    @(if include-configuration-outcomes?
                         @~a{
                             cp -r @;
                             ./blame-evaluation-gt/bex/dbs/@(current-remote-host-db-installation-directory-name)/configuration-outcomes @;
                             ./experiment-output/configuration-outcomes &&@" "
                             }
                         "") @;
                    ./pack.sh experiment-output @archive-name @;
                    }))
    (define archive-name+ext (~a archive-name ".tar.gz"))
    (match (send a-host scp
                 #:from-host (build-path projdir archive-name+ext)
                 #:to-local download-directory)
      [0
       (displayln @~a{Data from @a-host downloaded at @(build-path download-directory archive-name+ext)})
       (void)]
      [else
       (displayln @~a{Something went wrong downloading data for @a-host})
       absent]))
  (match summary
    [(hash-table ['completed (list (list* _ config-name _) _ ...)]
                 [_ '()] ...)
     #:when (or (config/mode-complete? summary expected-benchmarks)
                (user-prompt!
                 @~a{
                     Not all benchmarks are completed on @a-host
                     @(format-status a-host summary)
                     Missing: @(missing-completed-benchmarks summary expected-benchmarks)
                     Do you want to download the results anyway? 
                     }))
     (download-results! (or name config-name)
                        #:include-configuration-outcomes? (equal? config-name "TR"))]
    [else
     #:when (user-prompt!
             @~a{
                 @a-host results are not all successfully completed:
                 @(format-status a-host summary)
                 Missing: @(missing-completed-benchmarks summary expected-benchmarks)
                 Do you want to download the results anyway? 
                 })
     (displayln "Enter the desired archive name (no extension):")
     (download-results! (read-line))]
    [else
     (displayln @~a{Aborting download of inconsistent results:})
     (displayln (format-status a-host summary))
     absent]))


;; host?
;; [(host? (list/c (listof jobinfo?) x2) summary? -> any)]
;; ->
;; (or/c 'complete 'empty 'error)
(define (wait-for-current-jobs-to-finish host
                                         [periodic-action! void]
                                         #:period [sleep-period 15]
                                         #:print? [print? #f]
                                         #:expected-benchmarks [expected-benchmarks experiment-benchmarks])
  (when print?
    (displayln @~a{Waiting for current jobs to finish on @host ...}))
  (let loop ()
    (define current-status
      (option-let*
       ([summary (summarize-experiment-status host)]
        [jobs (send host get-jobs 'both)])

       (match-define (list active-jobs pending-jobs) jobs)
       (periodic-action! host jobs summary)
       (define no-jobs? (and (empty? active-jobs) (empty? pending-jobs)))
       (cond [(and no-jobs? (config/mode-complete? summary expected-benchmarks))
              'complete]
             [(and no-jobs? (summary-empty? summary))
              'empty]
             [(and no-jobs? (summary-has-errors? summary))
              (if (help!:continue? @~a{@host has errors}
                                   @~a{
                                       Found errors on @host, summary:
                                       @(format-status host summary)
                                       Resume waiting for finish? (No means abort): 
                                       })
                  (loop)
                  'error)]
             [no-jobs?
              (if (help!:continue? @~a{@host lost jobs}
                                   @~a{
                                       @host seems to have lost jobs. @;
                                       Expected to find jobs for all of @expected-benchmarks;
                                       Summary:
                                       @(format-status host summary)
                                       Re-run the missing jobs manually, and then continue.
                                       Resume waiting for finish? (No means abort): 
                                       })
                  (loop)
                  'error)]
             [else
              (when print?
                (printf "~a Sleeping for ~a min~nCurrent status:~n~a~n"
                        (date->string (current-date) #t)
                        sleep-period
                        (format-status host summary jobs)))
              (sleep (* sleep-period 60))
              (loop)])))
    (match current-status
      ['continue (loop)]
      [(? absent?)
       (displayln
        @~a{
            Unable to get summary or jobs, likely due to missing internet connection, @;
            just continuing to wait
            })
       (sleep (* sleep-period 60))
       (loop)]
      [other other])))

(define (help!:continue? notification prompt)
  (notify-phone! notification)
  (user-prompt! prompt))

(define (check-host-empty! host
                           [handle-not-empty!
                            (λ _
                              (unless (help!:continue?
                                       @~a{Host @host was not left in a clean state, stuck}
                                       @~a{
                                           Unexpected dirty state on @host, summary:
                                           @(pretty-format (summarize-experiment-status host))
                                           Is it fixed now? (Say no to abort)
                                           })
                                (raise-user-error 'check-host-empty! "Aborted."))
                              (check-host-empty! host))])
  (option-let*
   ([summary (summarize-experiment-status host)])
   (unless (summary-empty? summary)
     (handle-not-empty!))))

(define (update-host! a-host dbs-dir setup-config-name ; assumed to be in bex/setup/
                      [handle-failure! (λ (reason)
                                         (raise-user-error 'update-host! reason))])
  (define host-dbs-destination
    (build-path (get-field host-project-path a-host)
                "blame-evaluation-gt"
                "bex"
                "dbs"))
  (displayln "Zipping up dbs archive...")
  (define-values {dbs-dir-parent dbs-dir-name} (basename dbs-dir #:with-directory? #t))
  (define archive-name (~a dbs-dir-name ".tar.gz"))
  (parameterize ([current-directory dbs-dir-parent])
    (unless (system @~a{tar -czhf @archive-name @dbs-dir-name})
      (handle-failure! "Failed to zip dbs directory, giving up.")))
  (displayln "Uploading dbs archive...")
  (define host-db-archive-upload-path (build-path host-dbs-destination archive-name))
  (send a-host system/host @~a{mkdir -p @host-dbs-destination})
  (unless (zero? (send a-host scp
                       #:from-local (~a (build-path dbs-dir-parent archive-name))
                       #:to-host (~a host-db-archive-upload-path)))
    (handle-failure! @~a{Failed to upload db archive to @a-host}))

  (define host-dbs-unpacked-dir-path (build-path host-dbs-destination
                                                 (current-remote-host-db-installation-directory-name)))
  (define host-repo-path
    (build-path (get-field host-project-path a-host)
                "blame-evaluation-gt"))
  (define host-benchmarks-path
    (build-path (get-field host-project-path a-host)
                "gtp-benchmarks"))
  (define host-setup-config-path
    (build-path (get-field host-project-path a-host)
                "blame-evaluation-gt"
                "bex"
                "setup"
                setup-config-name))
  (for ([step (in-list '("Stashing current dbs..."
                         "Unpacking dbs..."
                         "Updating implementation..."
                         "Updating benchmarks..."
                         "Recompiling and checking status..."))]
        [cmd (in-list
              (list
               @~a{
                   rm -r '@host-dbs-destination'/last-dbs ; @;
                   mkdir -p '@host-dbs-unpacked-dir-path' ; @; in case there weren't any before
                   mv '@host-dbs-unpacked-dir-path' '@host-dbs-destination'/last-dbs
                   }
               @~a{
                   mkdir -p '@host-dbs-unpacked-dir-path' && @;
                   tar -xzvf '@host-db-archive-upload-path' -C '@host-dbs-unpacked-dir-path' --strip-components=1 && @;
                   echo "Done."
                   }
               @~a{
                   cd '@host-repo-path' && @;
                   git pull && @;
                   echo "Done."
                   }
               @~a{
                   cd '@host-benchmarks-path' && @;
                   git pull && @;
                   echo "Done."
                   }
               @~a{
                   @(get-field host-racket-path a-host) -l bex/util/project-raco -- -Cc && @;
                   @(get-field host-racket-path a-host) -l bex/setup/setup -- -c '@host-setup-config-path' -v && @;
                   echo "Done."
                   }))])
    (displayln step)
    (unless (send a-host
                  system/host
                  cmd
                  #:interactive? #t)
      (handle-failure! @~a{Update failed on step '@step'}))))

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
                       (fixed-width-format job-id 60)
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

(define (launch-benchmarks! a-host config-name benchmark-names
                            [handle-failure! (λ (benchmark)
                                               (displayln
                                                @~a{
                                                    Failed to submit job for @;
                                                    @benchmark @config-name on @a-host
                                                    }))]
                            #:outcome-checking-mode [outcome-checking-mode 'check])
  (for ([benchmark (in-list benchmark-names)]
        [i         (in-naturals)])
    ;; lltodo: the submission here can be batched
    ;; > This is (slightly) harder than the progress checks, just because of the job files.
    (when (and (not (zero? i))
               (zero? (modulo i 3)))
      (sleep (* 2 60)))
    (when (absent? (send a-host submit-job! benchmark config-name
                         #:mode outcome-checking-mode))
      (handle-failure! benchmark))))

(define (download-results! a-host download-directory
                           #:name [name #f]
                           #:expected-benchmarks [benchmark-names experiment-benchmarks])
  (option-let* ([summary (summarize-experiment-status a-host)])
    (download-completed-benchmarks! a-host summary download-directory
                                    #:name name
                                    #:expected-benchmarks benchmark-names)))


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


;; ----------------------------------------------------------------
;; ---------- Host options for orchestrating experiments ----------
;; ----------------------------------------------------------------

;; ----- these hosts should be used locally -----
(define zythos (new condor-host%
                    [hostname "zythos"]
                    [host-project-path "/project/blgt"]
                    [host-jobdir-path "./proj/jobctl"]))
;; orchestrates/manages the experiment from the local machine, ssh'ing into zythos to
;; run the experiment, one mode at a time, on peroni -- offloading all mutants to condor
(define zythos-ssh/one-job-per-mutant
  (new direct-access-host%
       [hostname "zythos-direct"]
       [host-project-path "/project/blgt"]
       [cpu-count 150]
       [env-vars "BEX_CONDOR_MACHINES='fix allagash piraat'"]))
(define benbox (new direct-access-host%
                    [hostname "benbox"]
                    [host-project-path "./blgt"]))
(define local (new local-direct-host%
                   [cpu-count 2]
                   [hostname "local"]
                   [host-project-path (simple-form-path project-path)]))

;; ----- these hosts should be used on peroni directly -----
;; runs the experiment with each mode as a whole machine condor job, all modes submitted at once
(define zythos-local/one-job-per-mode (new local-condor-host%
                          [hostname "zythos-local"]
                          [host-project-path (simple-form-path project-path)]
                          [host-jobdir-path (expand-user-path "~/proj/jobctl")]))
;; runs the experiment, one mode at a time, on peroni, offloading all mutants to condor
;; cpu-count is how many cpus the mode-experiment gets to use
;; (i.e. `cpu-count / batch-size` condor jobs)
(define zythos-local/one-job-per-mutant/batched
  (new local-direct-host%
       [cpu-count 10]
       [hostname "zythos-local-batch"]
       [host-project-path (simple-form-path project-path)]
       [env-vars "BEX_CONDOR_MACHINES='fix allagash piraat maudite tremens guldendraak' BEX_CONDOR_BATCH_SIZE=5"]))


(define hosts (list zythos-local/one-job-per-mutant/batched))



(main
 #:arguments {[(hash-table ['status? status?]
                           ['download (app (mapper host-by-name) download-targets)]
                           ['download-directory download-directory-override]
                           ['launch (app (mapper string->benchmark-spec) launch-targets)]
                           ['cancel (app (mapper string->benchmark-spec) cancel-targets)]
                           ['watch-for-stuck-jobs watch-for-stuck-jobs?]
                           ['queue Q-path]
                           ['resume-queue resume-Q/host]
                           ['update (app (mapper host-by-name) update-targets)]
                           ['dbs-path dbs-path-override]
                           ['launch-outcome-checking-mode (app string->symbol outcome-checking-mode)]
                           ['orchestration-config maybe-orchestration-config-id])
               args]
              #:once-each
              [("-c" "--orchestration-config")
               'orchestration-config
               ("Obtain db, download/data locations, and current db set name from"
                "the specified experiment config in `experiment-info.rkt`."
                "Mandatory for most operations.")
               #:collect ["name" take-latest #f]
               #:mandatory-unless (λ (flags) (member flags '((status?) (cancel))))]
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
              [("-D" "--download-destination")
               'download-directory
               ("Download results to the given directory."
                "Overrides the information obtained from the experiment config flag if specified."
                "Only has an effect when -d or -q supplied."
                @~a{Default: whatever the experiment config identified by -c specifies})
               #:collect ["path" take-latest #f]]
              [("-B" "--dbs")
               'dbs-path
               ("Install the given db set when updating a host's implementation."
                "Overrides the information obtained from the experiment config flag if specified."
                "Only has an effect when -u is supplied."
                @~a{Default: whatever the experiment config identified by -c specifies})
               #:collect ["path" take-latest #f]]
              [("-L" "--launch-outcome-checking-mode")
               'launch-outcome-checking-mode
               ("Outcome checking mode for benchmarks to be launched."
                "Only has an effect when -l is supplied."
                "Default: check")
               #:collect ["record/check" take-latest "check"]]
              [("-w" "--watch-for-stuck-jobs")
               'watch-for-stuck-jobs
               "Watch for stuck jobs on all hosts and restart them as they get stuck."
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
              [("-C" "--cancel")
               'cancel
               ("Cancel the specified benchmarks on a host for a mode.")
               #:collect ["(host mode benchmark ...)" cons empty]]
              [("-u" "--update")
               'update
               "Update the given hosts implementation of the experiment."
               #:collect ["host" cons empty]]}
 #:check [(or (not Q-path) (path-to-existant-file? Q-path))
          @~a{Unable to find queue spec at @Q-path}]

 (define orchestration-config
   (and maybe-orchestration-config-id
        (dynamic-require experiment-info.rkt
                         (string->symbol maybe-orchestration-config-id)
                         (λ _ (raise-user-error
                               @~a{
                                   No experiment config found in experiment-info.rkt @;
                                   named @maybe-orchestration-config-id
                                   })))))
 (define download-directory
   (or download-directory-override
       (and orchestration-config
            (orchestration-config-download-dir orchestration-config))))
 (define dbs-path
   (or dbs-path-override
       (and orchestration-config
            (orchestration-config-dbs-dir orchestration-config))))
 (when orchestration-config
   (current-remote-host-db-installation-directory-name
    (orchestration-config-dbs-dir-name orchestration-config)))

 (define (for-each-target targets action name)
   (for ([target (in-list targets)])
     (match-define (list a-host config-name benchmarks) target)
     (send a-host setup-job-management!)
     (define target-benchmarks (match benchmarks
                                 ['("<all>") experiment-benchmarks]
                                 [else benchmarks]))
     (for ([benchmark (in-list target-benchmarks)]
           [i (in-naturals)])
       ;; lltodo: the submission here can be batched
       ;; > This is (slightly) harder than the progress checks, just because of the job files.
       (when (and (not (zero? i))
                  (zero? (modulo i 3)))
         (sleep (* 2 60)))
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
                           (send a-host submit-job! benchmark config-name
                                 #:mode outcome-checking-mode))
                         "submit")
        ;; in case it's a direct/local host, the other thread needs a chance to do the job
        (sleep 1)]
       [(not (empty? cancel-targets))
        (for-each-target cancel-targets
                         (λ (a-host benchmark config-name)
                           (send a-host cancel-job! benchmark config-name))
                         "cancel")
        ;; in case it's a direct/local host, the other thread needs a chance to do the job
        (sleep 1)]
       [watch-for-stuck-jobs?
        (for ([host (in-list hosts)])
          (send host setup-job-management!))
        (let loop ()
          (for-each restart-stuck-jobs! hosts)
          (printf "Sleeping                    \r")
          (sleep (* 15 60))
          (loop))]
       [(not (empty? download-targets))
        (for ([a-host (in-list download-targets)])
          (download-results! a-host download-directory))]
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
          (match (wait-for-current-jobs-to-finish host #:print? #t)
            ['complete
             (option-let*
              ([complete-summary (summarize-experiment-status host)]
               [_ (download-completed-benchmarks! host complete-summary download-directory)])
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
          (send host setup-job-management!)
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
             (send host setup-job-management!)
             (check-host-empty! host)
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
        (raise-user-error
         "unimplemented update to this cli interface: updating now needs a setup config")
        (for ([a-host (in-list update-targets)])
          (update-host! a-host dbs-path))]))
