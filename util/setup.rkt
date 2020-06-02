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
            "typed-racket"))
  (directory-exists? TR-dir))

(define (setup-existing-TR-dir! raco-path TR-dir)
  (displayln @~a{Installing existing TR at @TR-dir})
  (define TR-subpaths
    (for/list ([dir (in-list '("source-syntax"
                               "typed-racket-compatibility"
                               "typed-racket-doc"
                               "typed-racket-lib"
                               "typed-racket-more"
                               "typed-racket"))])
      (build-path TR-dir dir)))
  (apply shell*
         raco-path
         "pkg"
         "update"
         "-D"
         "--link"
         TR-subpaths))

(define TR-modified-module-rel-path
  (build-path "typed-racket-lib"
              "typed-racket"
              "utils"
              "require-contract.rkt"))
(define (modify-TR TR-dir)
  (displayln "Modifying typed-racket...")
  (unless (parameterize ([current-directory TR-dir])
            (shell* "git"
                    "checkout"
                    "v7.6"))
    (user-prompt! @~a{

                      Help! Unable to checkout branch v7.6 in @TR-dir
                      Please resolve the problem and checkout the branch before continuing.
                      }))
  (unless (dry-run?)
    (replace-in-file! (build-path TR-dir TR-modified-module-rel-path)
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

(define (check-TR-install racket-dir
                          TR-dir
                          #:display-failures? [display-failures? #f])
  (define show-str
    (system/string @~a{@|racket-dir|/bin/raco pkg show -ld typed-racket}))
  (define TR-dir-installed?
    (match (regexp-match* @regexp{"/[^"]+"} @; close "
                          show-str)
      [(list path) (string-contains? path (path->string racket-dir))]
      [else #f]))

  (define TR-modified-line-present?
    (shell* "grep"
            "interface for.*from"
            (build-path TR-dir TR-modified-module-rel-path)))

  (when (and (not TR-dir-installed?)
             display-failures?)
    (displayln
     @~a{

         ERROR: The local version of typed-racket is not installed.
         Run this setup script to install it.
         }))
  (when (and (not TR-modified-line-present?)
             display-failures?)
    (displayln
     @~a{

         ERROR: The installed version of typed-racket doesn't appear @;
         to have the required modifications.
         Run this setup script to make them.
         }))
  (and TR-dir-installed?
       TR-modified-line-present?))

(define (check-install-configuration racket-dir TR-dir gtp-dir)
  (define racket-version-str
    (system/string @~a{@|racket-dir|/bin/racket --version}))
  (define gtp-branches-str
    (parameterize ([current-directory gtp-dir])
      (system/string "git branch")))

  (define racket-version-ok?
    (regexp-match? @regexp{Racket v7\.7.*\[cs\]} racket-version-str))
  (define gtp-branch-ok?
    (regexp-match? @regexp{\* hash-top} gtp-branches-str))

  (unless racket-version-ok?
    (displayln
     @~a{

         ERROR: The installed racket has the wrong version.
         installed: @racket-version-str
         required:  Racket v7.7 [cs]
         }))
  (unless gtp-branch-ok?
    (displayln
     @~a{

         ERROR: The wrong branch of gtp-benchmarks is present.
         current branch:  @gtp-branches-str
         required branch: hash-top
         }))

  (and racket-version-ok?
       gtp-branch-ok?
       (check-TR-install racket-dir TR-dir #:display-failures? #t)))

(main
 #:arguments {[flags args]
              #:once-each
              [("-D" "--deps-only")
               'deps-only
               ("Only install pkg dependencies.")
               #:record]
              [("-v" "--verify-install")
               'verify-install
               ("Verify the current installation to ensure that"
                "all dependencies have the right versions or customizations.")
               #:conflicts '(deps-only)
               #:record]
              [("--dry-run")
               'dry-run
               ("Perform a dry run, printing commands instead of running them.")
               #:record]
              [("-r" "--root")
               'root-path
               ("The root path, where dependencies reside or should be installed."
                @~a{Default: @(pretty-path repo-parent-path)})
               #:collect ["path" take-latest repo-parent-path]]
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
 (define racket-dir (simple-form-path (build-path root "racket")))
 (define TR-dir
   (simple-form-path (or (hash-ref flags 'tr-path)
                         (build-path root "typed-racket"))))
 (define gtp-dir
   (simple-form-path (or (hash-ref flags 'gtp-path)
                         (build-path root "gtp-benchmarks"))))

 (define (verify-install!)
   (match (check-install-configuration racket-dir TR-dir gtp-dir)
     [#t
      (displayln "\nInstall is configured correctly.")
      (exit 0)]
     [#f
      (displayln "\nInstall is misconfigured.")
      (exit 1)]))
 (when (hash-ref flags 'verify-install)
   (verify-install!))

 (unless (or (directory-exists? racket-dir)
             (install-racket racket-dir))
   (eprintf "Something went wrong installing and setting up Racket.~n")
   (exit 1))

 (define raco-path (build-path racket-dir "bin" "raco"))

 (install-pkg-dependencies raco-path)
 (when (hash-ref flags 'deps-only)
   (exit 0))

 (cond [(not (directory-exists? TR-dir))
        (download-TR raco-path TR-dir)]
       [else (setup-existing-TR-dir! raco-path TR-dir)])
 (modify-TR TR-dir)

 (unless (directory-exists? gtp-dir)
   (install-gtp-repo gtp-dir))

 (verify-install!)

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
