#lang at-exp rscript

;; ==================================================
;;  Modify these to configure setup
;; ==================================================

(define PKG-DEPENDENCIES
  '("require-typed-check"
    "custom-load"
    "https://github.com/LLazarek/ruinit.git"
    "https://github.com/LLazarek/rscript.git"
    "pfds"))

(define racket-download-url
  "https://mirror.racket-lang.org/installers/7.7/racket-7.7-x86_64-linux-cs.sh")

;; ==================================================

(define-runtime-paths
  [repo-parent-path "../../"]
  [repo-path ".."])

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
  (define args/full-program-path
    (match args
      [(list* (and (? string?)
                   (not (regexp #rx"/"))
                   name)
              program-args)
       (cons (find-executable-path name)
             program-args)]
      [else args]))
  (cond [(dry-run?)
         (displayln @~a{
                        Would have run command in @(pretty-path (current-directory)):
                        $ @(string*-join args/full-program-path)
                        })
         #t]
        [else
         (displayln @~a{Executing $ @(string*-join args/full-program-path)})
         (apply system* args/full-program-path)]))

(define (install-racket racket-dir)
  (define-values {parent _}
    (basename racket-dir #:with-directory? #t))
  (parameterize ([current-directory parent])
    (define racket-installer-name
      (last (string-split racket-download-url "/")))
    (unless (file-exists? racket-installer-name)
      (displayln "Downloading Racket...")
      (shell* "wget"
	      racket-download-url)
      (shell* "chmod"
	      "u+x"
	      racket-installer-name)
      (displayln "Done."))

    (displayln "Installing Racket...")
    (define installer-outfile "./racket-installer-output.txt")
    (define installer-out (open-output-file installer-outfile
                                            #:exists 'replace))
    (define install-status
      (cond [(dry-run?)
             (shell* @~a{./@racket-installer-name})
             'done-ok]
            [else
             (match (process*/ports installer-out #f 'stdout
                                    @~a{./@racket-installer-name})
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
       (when (user-prompt! "Do you want to delete the installer?")
         (shell* "rm" racket-installer-name))])))

(define (install-pkg-dependencies raco-path)
  (displayln "Installing dependencies...")
  (begin0 (apply shell*

                 raco-path
                 "pkg"
                 "install"
                 "-D"
                 "-j" "2"
                 "--skip-installed"
                 PKG-DEPENDENCIES)
    (displayln "Done.")))

(define (download-TR raco-path TR-dir)
  (displayln "Downloading typed-racket...")
  (define-values {parent _}
    (basename TR-dir #:with-directory? #t))
  (define installed?
    (parameterize ([current-directory parent])
      (shell* raco-path
              "pkg"
              "update"
              "-j" "2"
              "--no-setup"
              "--catalog"
              "https://pkgs.racket-lang.org"
              "typed-racket")
      (shell* raco-path
              "pkg"
              "update"
              "-j" "2"
              "--clone"
              "typed-racket")
      #t))
  (cond [(directory-exists? TR-dir)
         (parameterize ([current-directory TR-dir])
           (shell* "git"
                   "checkout"
                   "v7.6"))
         (displayln "Done.")
         (modify-TR TR-dir)]
        [else
         #f]))

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
  (displayln "Installing gtp-benchmarks...")
  (parameterize ([current-directory parent])
    (shell* "git"
            "clone"
            "https://github.com/bennn/gtp-benchmarks.git"))
  (parameterize ([current-directory gtp-dir])
    (shell* "git"
            "checkout"
            "hash-top"))
  (displayln "Done."))

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

 (install-pkg-dependencies raco-path)
 (when (hash-ref flags 'deps-only)
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

(module test racket/base)
