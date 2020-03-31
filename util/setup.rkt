#lang at-exp rscript

(define repo-parent-path "../../")
(define repo-path "..")

(define (pretty-path p)
  (find-relative-path (simple-form-path (current-directory))
                      (simple-form-path p)))

(define (string*-join l)
  (string-join
   (map (match-lambda [(? string? s) s]
                      [other (~a other)])
        l)))

(define dry-run? (make-parameter #t))
(define (shell* . args)
  (cond [(dry-run?)
         (displayln @~a{
                        Would have run command in @(pretty-path (current-directory)):
                        $ @(string*-join args)
                        })
         #t]
        [else
         (displayln @~a(Executing $ @(string*-join args)))
         (apply system* args)]))

(define (install-racket racket-dir)
  (define-values {parent _}
    (basename racket-dir #:with-directory? #t))
  (parameterize ([current-directory parent])
    (displayln "Downloading Racket...")
    (shell* "wget"
            "https://mirror.racket-lang.org/installers/7.6/racket-7.6-x86_64-linux.sh")
    (shell* "chmod"
            "u+x"
            "racket-7.6-x86_64-linux.sh")
    (displayln "Done.")

    (displayln "Installing Racket...")
    (define installer-outfile "./racket-installer-output.txt")
    (define installer-out (open-output-file installer-outfile
                                            #:exists 'replace))
    (define install-status
      (cond [(dry-run?)
             (shell* "./racket-7.6-x86_64-linux.sh")
             'done-ok]
            [else
             (match (process*/ports installer-out #f 'stdout
                                    "./racket-7.6-x86_64-linux.sh")
               [(list #f installer-stdin _ #f installer-ctl)
                ;; shamelessly ripped off from
                ;; https://github.com/greghendershott/travis-racket/blob/master/install-racket.sh
                (displayln "no" installer-stdin)
                (displayln (~a racket-dir) installer-stdin)
                (newline installer-stdin)
                (close-output-port installer-stdin)
                (installer-ctl 'wait)
                (close-output-port installer-out)
                (installer-ctl 'status)]
               [else
                (close-output-port installer-out)
                'done-error])]))
    (displayln "Done.")
    (match install-status
      ['done-error
       (eprintf
        @~a{


            Something went wrong installing racket.
            Installer output:
            @(file->string installer-outfile)


            Giving up.

            })
       (exit 1)]
      ['done-ok
       (shell* "rm" "racket-7.6-x86_64-linux.sh")
       (install-pkg-dependencies (build-path racket-dir "bin" "raco"))])))

(define (install-pkg-dependencies raco-path)
  (displayln "Installing dependencies...")
  (begin0 (shell* raco-path
                  "pkg"
                  "install"
                  "require-typed-check"
                  "custom-load"
                  "ruinit")
    (displayln "Done.")))

(define (download-TR raco-path TR-dir)
  (displayln "Downloading typed-racket...")
  (define-values {parent _}
    (basename TR-dir #:with-directory? #t))
  (define installed?
    (parameterize ([current-directory parent])
      (and (shell* raco-path
                   "pkg"
                   "update"
                   "--no-setup"
                   "--catalog"
                   "--batch"
                   "https://pkgs.racket-lang.org"
                   "typed-racket")
           (shell* raco-path
                   "pkg"
                   "update"
                   "--clone"
                   "--batch"
                   "typed-racket"))))
  (when installed?
    (parameterize ([current-directory TR-dir])
      (shell* "git"
              "checkout"
              "v7.6")))
  (displayln "Done.")
  (and installed?
       (modify-TR TR-dir)))

(define (modify-TR TR-dir)
  (displayln "Modifying typed-racket...")
  (unless (dry-run?)
    (replace-in-file! (build-path TR-dir
                                  "typed-racket-lib"
                                  "typed-racket"
                                  "utils"
                                  "require-contract.rkt")
                      (regexp-quote @~a{'(interface for #,(syntax->datum #'nm.nm))})
                      @~a{'(interface for #,(syntax->datum #'nm.nm) from #,(syntax->datum #'lib))}))
  (displayln "Done."))

(define (install-gtp-repo gtp-dir)
  (define-values {parent _}
    (basename gtp-dir #:with-directory? #t))
  (parameterize ([current-directory parent])
    (displayln "Installing gtp-benchmarks...")
    (begin0 (shell* "git"
                    "clone"
                    "https://github.com/bennn/gtp-benchmarks.git")
      (displayln "Done."))))

(main
 #:arguments {[flags args]
              #:once-each
              [("--dry-run")
               'dry-run
               ("Perform a dry run, printing commands instead of running them.")
               #:record]
              [("-r" "--root")
               'root-path
               ("The root path, where dependencies reside or should be installed."
                @~a{Default: @(pretty-path repo-parent-path)})
               #:collect ["path" take-latest repo-parent-path]]
              [("-D" "--deps-only")
               'deps-only
               ("Only install pkg dependencies.")
               #:record]
              [("-t" "--tr-path")
               'tr-path
               ("Path to the typed-racket installation to use."
                "If not specified, and none found under the root path,"
                "then a new one will be downloaded in the root."
                @~a{Default: <root>/typed-racket})
               #:collect ["path" take-latest #f]]
              [("-g" "--gtp-path")
               'gtp-path
               ("Path to the gtp-benchmarks repository."
                "If not specified, and none found under the root path,"
                "then a new one will be downloaded in the root."
                @~a{Default: <root>/gtp-benchmarks})
               #:collect ["path" take-latest #f]]}

 #:check [(path-to-existant-directory? (hash-ref flags 'root-path))
          @~a{@(pretty-path (hash-ref flags 'root-path)) does not exist.}]

 (dry-run? (hash-ref flags 'dry-run))
 (define root (hash-ref flags 'root-path))
 (define racket-dir (build-path root "racket"))
 (define TR-dir (or (hash-ref flags 'tr-path)
                    (build-path root "typed-racket")))
 (define gtp-dir (or (hash-ref flags 'gtp-path)
                     (build-path root "gtp-benchmarks")))

 (unless (or (directory-exists? racket-dir)
             (install-racket racket-dir))
   (eprintf "Something went wrong installing and setting up Racket.~n")
   (exit 1))

 (define raco-path (build-path racket-dir "bin" "raco"))

 (when (hash-ref flags 'deps-only)
   (install-pkg-dependencies raco-path)
   (exit 0))

 (cond [(directory-exists? TR-dir)
        (cond [(user-prompt!
                @~a{

                    @TR-dir already exists, but we require a modified
                    version of TR. Do you want to modify that copy as we
                    need?
                    Modification is idempotent: if you have already done
                    it to that copy, trying to do it again has no effect.

                    })
               (modify-TR TR-dir)]
              [else
               (displayln "Skipping modification.")])]
       [(download-TR raco-path TR-dir) (void)]
       [else
        (eprintf "Something went wrong installing TR.~n")
        (exit 1)])

 (unless (or (directory-exists? gtp-dir)
             (install-gtp-repo gtp-dir))
   (eprintf "Something went wrong installing gtp-benchmarks.~n")
   (exit 1))

 (when (user-prompt!
        @~a{

            Setup complete.

            Do you want to verify that everything is in working order
            by running the system tests?
            })
   (void (shell* raco-path
                 "test"
                 repo-path))))
