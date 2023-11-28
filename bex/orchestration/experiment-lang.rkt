#lang at-exp racket

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         define-runtime-path
         with-configuration

         (all-from-out "experiment-manager.rkt"))

(require syntax/parse/define
         racket/stxparam
         racket/runtime-path
         racket/date
         "experiment-manager.rkt"
         "experiment-info.rkt")

(begin-for-syntax
  (require racket/runtime-path
           racket/format
           racket/path
           (rename-in (only-in "experiment-info.rkt" experiment-benchmarks)
                      [experiment-benchmarks all-benchmarks]))
  (define-syntax-class benchmark-id
    #:description "a benchmark name"
    #:commit
    #:attributes [str]
    [pattern name:id
             #:when (member (symbol->string (syntax->datum #'name)) all-benchmarks)
             #:with str (datum->syntax this-syntax (symbol->string (syntax->datum #'name)))])

  (define-runtime-path configs-dir "../configurables/configs")
  (define-syntax-class mode-id
    #:description "a mode name, corresponding to a configuration file name"
    #:commit
    #:attributes [str compile-time-name-str]
    [pattern name:id
             #:do [(define name-str (symbol->string (syntax->datum #'name)))]
             #:fail-unless (file-exists? (build-path configs-dir (~a name-str ".rkt")))
                           (~a "Configuration `"
                               name-str
                               ".rkt` does not exist in "
                               (simple-form-path configs-dir))
             #:attr compile-time-name-str name-str
             #:with str (datum->syntax this-syntax name-str)])
  (define-syntax-class (run-mode-spec implicit-record-outcomes?)
    #:description "a run-mode spec"
    #:commit
    #:attributes [implementation compile-time-mode-name-str]
    [pattern (run-mode mode:mode-id
                       {~optional {~seq #:only specific-benchmark:benchmark-id ...}}
                       {~optional {~seq #:relocate download-dir}}
                       {~optional {~seq #:name name}}
                       {~optional {~and #:record-outcomes record-outcomes-kw}})
             #:attr compile-time-mode-name-str (attribute mode.compile-time-name-str)
             #:with [benchmark-name ...] (if (attribute specific-benchmark)
                                             #'(specific-benchmark.str ...)
                                             (datum->syntax this-syntax all-benchmarks))
             #:with record-outcomes? (if (or implicit-record-outcomes?
                                             (attribute record-outcomes-kw))
                                         #'#t
                                         #'#f)
             #:with implementation #'(run-one-mode current-host
                                                   current-dbs
                                                   mode.str
                                                   (list benchmark-name ...)
                                                   {~? download-dir current-download-dir}
                                                   {~? name #f}
                                                   current-status-file
                                                   record-outcomes?)]))

(define-syntax-parameter current-host
  (λ _ (raise-syntax-error 'run-mode
                           "can only be used inside a `with-configuration` form")))
(define-syntax-parameter current-dbs
  (λ _ (raise-syntax-error 'run-mode
                           "can only be used inside a `with-configuration` form")))
(define-syntax-parameter current-download-dir
  (λ _ (raise-syntax-error 'run-mode
                           "can only be used inside a `with-configuration` form")))

(define-syntax-parameter current-status-file
  (λ _ #'#f))

(define-simple-macro (module-begin top-level-e ...)
  (#%module-begin
   (module test racket/base) ;; no testing launching experiments...
   top-level-e ...))

(define-simple-macro (with-configuration [host configuration]
                       {~alt
                        {~optional {~seq #:status-in status-file-path}}
                        {~optional {~and #:skip-setup skip-setup-kw}}
                        {~optional {~and #:manual-outcome-recording skip-record-outcomes-kw}}} ...
                       {~var first-mode (run-mode-spec (not (attribute skip-record-outcomes-kw)))}
                       {~var more-modes (run-mode-spec #f)} ...)
  #:fail-when (not (or (attribute skip-record-outcomes-kw)
                       (string=? (attribute first-mode.compile-time-mode-name-str) "TR")))
              "Unless manually managing outcome recording with #:manual-outcome-recording, the first mode run must be TR so that later modes can perform outcome parity checks."
  #:with maybe-host-update (if (attribute skip-setup-kw)
                               #'(void)
                               #'(update-host! the-host
                                               the-dbs
                                               the-setup-config
                                               (handle-host-update-failure! the-dbs)))
  (module+ main
    (let ([the-host host]
          [the-dbs (orchestration-config-dbs-dir configuration)]
          [the-download-dir (orchestration-config-download-dir configuration)]
          [the-setup-config (orchestration-config-setup-config configuration)]
          [the-status-file {~? status-file-path #f}])
      (parameterize ([current-remote-host-db-installation-directory-name
                      (orchestration-config-dbs-dir-name configuration)])
        maybe-host-update
        (syntax-parameterize ([current-host (syntax-id-rules () [_ the-host])]
                              [current-dbs  (syntax-id-rules () [_ the-dbs])]
                              [current-download-dir (syntax-id-rules () [_ the-download-dir])]
                              [current-status-file (syntax-id-rules () [_ the-status-file])])
          first-mode.implementation
          more-modes.implementation ...)))))

(define ((handle-host-update-failure! dbs-path) msg)
  (unless (help!:continue? msg
                           @~a{
                               @msg
                               Currently the host is setup with @dbs-path
                               Fix the problem and manually setup the host @;
                               (e.g. with `experiment-manager.rkt`).
                               Done? (No means abort.)
                               })
      (raise-user-error 'handle-host-update-failure! "Aborted.")))
(define ((handle-launch-benchmarks-failure! dbs-path) benchmark)
  (unless (help!:continue? @~a{Benchmark launch failed}
                           @~a{
                               Failed to launch benchmark @benchmark
                               Currently the host is setup with @dbs-path
                               Fix the problem and launch the benchmark before continuing.
                               Done? (No means abort.)
                               })
    (raise-user-error 'handle-launch-benchmarks-failure! "Aborted.")))

(define (handle-job-data-disappeared-failure! host dbs-path)
  (unless (help!:continue? @~a{Jobs disappeared?}
                           @~a{
                               Jobs have disappeared on @host, which is setup with @dbs-path
                               Continue with the rest of the experiment?
                               If the results are there, download them manually before continuing.
                               })
    (raise-user-error 'handle-job-data-disappeared-failure! "Aborted.")))

(define (run-one-mode host
                      dbs
                      mode-name
                      benchmark-names
                      download-dir
                      name
                      status-file
                      record-outcomes?)
  (displayln @~a{Running mode @mode-name on @host @(if record-outcomes?
                                                       "with outcome recording"
                                                       "with outcome checking")})
  (displayln @~a{Checking that @host is in a clean state...})
  (check-host-empty! host
                     (λ _
                       (unless (help!:continue?
                                @~a{Host @host was not left in a clean state, stuck}
                                @~a{
                                    Unexpected dirty state on @host, summary:
                                    @(format-status host)
                                    You can go clean it up manually now and then continue, @;
                                    or go ahead anyway now.
                                    Continue to run the mode? (Say no to abort)
                                    })
                         (raise-user-error 'check-host-empty! "Aborted."))))
  (send host setup-job-management!)
  (displayln @~a{Submitting benchmark jobs...})
  (launch-benchmarks! host mode-name benchmark-names
                      (handle-launch-benchmarks-failure! dbs)
                      #:outcome-checking-mode (if record-outcomes? 'record 'check))
  (displayln @~a{Waiting for benchmarks to finish...})
  (match (wait-for-current-jobs-to-finish host
                                          (λ (host jobs summary)
                                            (when status-file
                                              (display-to-file
                                               @~a{
                                                   @(format-status host summary jobs)

                                                   Updated @(date->string (current-date) #t)
                                                   }
                                               status-file
                                               #:exists 'truncate)))
                                          #:expected-benchmarks benchmark-names)
    ['complete
     (displayln @~a{Downloading results...})
     (download-results! host download-dir
                        #:name name
                        #:expected-benchmarks benchmark-names)
     (displayln @~a{@mode-name finished})]
    ['empty
     (handle-job-data-disappeared-failure! host dbs)]
    ['error
     (unless (help!:continue? @~a{Something went wrong waiting for data}
                              @~a{
                                  Something went wrong waiting for the data on @host
                                  You can manually clean it up and then continue with
                                  the rest of the experiment, or abort (by saying no).
                                  Continue now?
                                  })
       (raise-user-error 'run-one-mode "Aborted."))]))

