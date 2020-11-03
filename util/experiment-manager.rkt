#lang at-exp rscript

(require syntax/parse/define)

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

(struct host (name ; hostname?
              project-path ; string?
              active-jobs? ; hostname? -> boolean?
              submit-job ; hostname? benchmark-name? mode-name? -> job-id?
              cancel-job ; job-id? -> void
              )
  #:transparent
  #:methods gen:custom-write
  {(define (write-proc v port mode)
     ((if mode write display) (host-name v) port))})
(define host<%> (interface (writable<%>)
                  [active-jobs? (->m boolean?)]
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
      (system/string @~a{ssh @hostname @(apply ~a (add-between parts " "))}))

    (define/public (active-jobs?)
      (match (system/string @~a{ssh @hostname condor_q})
        [(regexp @pregexp{(?m:^(\S+\s+){6}1)}) #t]
        [else #f]))

    (define/public (submit-job! benchmark
                                config-name
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

               Arguments = "'@benchmark' '@config-name' '@record/check-mode' '@cpus' '@name'"
               Executable = /project/blgt/run-experiment.sh
               Error = condor-output.txt
               Output = condor-output.txt
               Log = condor-log.txt

               +IsWholeMachineJob = true
               +IsSuspensionJob = false

               Queue

               }
           job.sub)
          (match (system/exit-code @~a{scp job.sub @|hostname|:@host-jobfile-path})
            [0 #t]
            [else absent])))
      (for* ([_ (in-option job-uploaded?)]
             [id (in-option (match (system/string
                                    @~a{ssh @hostname condor_submit -verbose @host-jobfile-path})
                              [(regexp @pregexp{\*\* Proc ([\d.]+):} (list _ id)) id]
                              [else absent]))])
        (save-job! id)))

    (define/public (cancel-job! benchmark config-name)
      (option-let* ([id (read-job! benchmark config-name)])
                   (void (system/string @~a{ssh @hostname condor_rm '@id'}))))

    (define/private (ensure-store!)
      (make-directory* (path-only data-store))
      (unless (file-exists? data-store)
        (system @~a{touch '@data-store'})))
    (define/private (save-job! benchmark config-name id)
      (ensure-store!)
      (with-output-to-file data-store #:exists 'append
        (thunk (writeln (cons (list benchmark config-name) id)))))
    (define/private (read-job! benchmark config-name) ; -> (option/c id?)
      (dict-ref (file->list data-store)
                (list benchmark config-name)
                absent))))

(define-simple-macro (with-temp-file name body ...)
  (call-with-temp-file (λ (name) body ...)))
(define (call-with-temp-file f)
  (define temp (make-temporary-file))
  (begin0 (f temp)
    (when (file-exists? temp) (delete-file temp))))

(define hosts
  (list (new condor-host%
             [hostname "zythos"]
             [host-project-path "./blgt"]
             [host-jobdir-path "./proj/jobctl"])))

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

;; host<%> -> (values boolean? (option/c summary/c))
(define (summarize-experiment-status a-host)
  (define active-jobs? (send a-host active-jobs?))
  (define (with-progress incomplete-bench)
    (match-define (list name config) incomplete-bench)
    (printf "Retrieving ~a progress ...\r" name)
    (list name config (try-unwrap (get-progress a-host name))))
  (printf "Checking ~a...\r" a-host)
  (values active-jobs?
          (option-let* ([results (get-results a-host)])
                       (hash-update results
                                    'incomplete
                                    (λ (benches) (map with-progress benches))))))

(define (stuck-jobs summary)
  (for*/list ([maybe-benchmark (in-list (hash-ref summary 'incomplete))]
              [benchmark (in-option maybe-benchmark)]
              #:when (> (third benchmark) 0.99))
    benchmark))

(define (download-completed-benchmarks! a-host summary)
  (raise-user-error 'download-completed-benchmarks! "Not implemented"))

(define (string->value s)
  (with-input-from-string s read))
(define (host-by-name name)
  (match (findf (λ (h) (string=? name (host-name h))) hosts)
    [#f (raise-user-error 'experiment-manager @~a{No host known with name @name})]
    [host host]))
(define ((make-map f) l) (map f l))
(define-simple-macro (pick-values f e)
  (call-with-values (thunk e)
                    (λ vals (f vals))))

(main
 #:arguments {[(hash-table ['status? status?]
                           ['download (app (make-map host-by-name) download-targets)]
                           ['launch (app (make-map string->value) launch-targets)])
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
               #:collect ["(host mode benchmark ...)" cons empty]]}

 (cond [status?
        (for ([a-host (in-list hosts)])
          (define-values {active-jobs? maybe-summary}
            (summarize-experiment-status a-host))
          (displayln @~a{---------- @a-host ----------})
          (displayln @~a{Active: @(if active-jobs? 'yes 'no)})
          (pretty-display (try-unwrap maybe-summary))
          (newline))]
       [(not (empty? download-targets))
        (for ([a-host (in-list download-targets)])
          (option-let* ([summary (pick-values second (summarize-experiment-status a-host))])
                       (download-completed-benchmarks! a-host summary)))]))
