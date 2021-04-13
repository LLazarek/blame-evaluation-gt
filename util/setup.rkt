#lang at-exp rscript

(require syntax/location
         (prefix-in db: "../db/db.rkt"))

(define-runtime-paths
  [repo-parent-path "../../"]
  [repo-path ".."]
  [dbs-dir "../dbs"])

;; ==================================================
;;  Modify these to configure setup
;; ==================================================

(define PKG-DEPENDENCIES
  '("require-typed-check"
    "custom-load"
    "https://github.com/LLazarek/ruinit.git"
    "https://github.com/LLazarek/rscript.git"
    "pfds"))

(define racket-version "7.8")
(define racket-download-url
  @~a{https://mirror.racket-lang.org/installers/@|racket-version|/racket-@|racket-version|-x86_64-linux-cs.sh})

(define expected-benchmark-names
  (map ~a '(suffixtree kcfa snake take5 acquire tetris synth gregor quadT quadU)))

(define expected-dbs
  ;;    dir                    db                        entry-per-bench?
  (hash "code-mutations" (hash "mutant-samples.rktdb"    #t
                               "pre-computed-mutant-results.rktdb" #t
                               "pre-selected-bt-roots.rktdb" #t
                               "transient-special-cases.rktdb" #f)))

(define expected-TR-branch "transient-blame")
(define expected-gtp-branch "master")
(define expected-blgt-branch "dev")

;; ==================================================

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
  ;; For setting up normal TR
  #;(parameterize ([current-directory parent])
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
  (parameterize ([current-directory parent])
    (shell* "git"
            "clone"
            "https://github.com/bennn/typed-racket.git"))
  (parameterize ([current-directory TR-dir])
    (shell* "git"
            "checkout"
            expected-TR-branch))
  (directory-exists? TR-dir))

(define (setup-existing-TR-dir! raco-path TR-dir)
  (displayln @~a{Installing TR at @TR-dir})
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
            "https://github.com/llazarek/gtp-benchmarks.git"))
  (parameterize ([current-directory gtp-dir])
    (shell* "git"
            "checkout"
            expected-gtp-branch))
  (displayln "Done."))

(define (check-TR-install racket-dir
                          TR-dir
                          #:display-failures? [display-failures? #f])
  ;;;; This doesn't seem to work
  ;; (define show-str
  ;;   (system/string @~a{@|racket-dir|/bin/raco pkg show -ld typed-racket}))
  ;; (define TR-dir-installed?
  ;;   (match (regexp-match* @regexp{"/[^"]+"} @; close "
  ;;                         show-str)
  ;;     [(list path) (string-contains? path (path->string racket-dir))]
  ;;     [else #f]))

  (displayln "Checking TR for transient implementation...")
  (define (error-has-interface-from? e)
    (equal? (blame-positive (exn:fail:contract:blame-object e))
            '(interface for first from (only-in racket first))))
  (define modified-TR-active?
    (with-handlers ([exn:fail:contract:blame? error-has-interface-from?])
      (dynamic-require `(submod (file ,(quote-source-file)) check-modified-TR) #f)
      #f))

  (define transient-kw-accepted?
    (with-handlers ([exn:fail:syntax? (const #f)])
      (eval #'(module test typed/racket (#%module-begin #:transient (+ 2 2))))
      #t))

  (define transient-blame-present?
    (with-handlers ([exn:fail:filesystem:missing-module? (const #f)])
      (and (dynamic-require 'typed-racket/utils/transient-contract-struct
                            'exn:fail:contract:blame:transient
                            (const #f))
           #t)))

  (when (and (not modified-TR-active?)
             display-failures?)
    (displayln
     @~a{

         ERROR: The modified version of typed-racket is not installed or active.
         Run this setup script to install it.
         }))
  (when (and (not transient-kw-accepted?)
             display-failures?)
    (displayln
     @~a{

         ERROR: Transient support is not available in the current typed-racket
         install. Are you trying to use a different version of typed racket?
         This setup script installs a custom version with support for Transient.
         Run me to install it.
         }))
  (when (and (not transient-blame-present?)
             display-failures?)
    (displayln
     @~a{

         ERROR: Transient support is available but without blame.
         Probably the install has become misconfigured somehow.
         Run me to fix it.
         }))
  (and modified-TR-active?
       transient-kw-accepted?
       transient-blame-present?))

(module check-modified-TR typed/racket
  (require/typed (only-in racket first)
    [first ((Listof Any) . -> . Boolean)])
  (first '(5)))

(define (check-db/keys path-to-db
                       [expected-keys #f]
                       #:display-failures? [display-failures? #t])
  (cond [(not (db:path-to-db? path-to-db))
         (when display-failures?
           (displayln
            @~a{

                WARNING: Missing database at @path-to-db
                }))
         #f]
        [(and expected-keys
              (not (set=? (db:keys (db:get path-to-db)) expected-keys)))
         (define missing (set-subtract (db:keys (db:get path-to-db))
                                       expected-keys))
         (when display-failures?
           (displayln
            @~a{

                WARNING: Database at @path-to-db doesn't have every expected benchmark.
                Specifically, these are missing: @missing
                }))
         #f]
        [else #t]))


(define (get-repo-current-branch repo-path)
  (parameterize ([current-directory repo-path])
    (match (system/string "git branch")
      [(regexp @regexp{(?m:^\* (.+)$)} (list _ name))
       name]
      [else
       (eprintf "WARNING: couldn't get current branch for repo ~a~n"
                repo-path)
       #f])))
(define (repo-branch-up-to-date-with-remote? repo-path branch [remote-name "origin"])
  (parameterize ([current-directory repo-path])
    (regexp-match? @~a{@|branch|.*up to date}
                   (system/string @~a{git remote show @remote-name}))))
(define (report-repo-status repo-dir active-branch expected-branch up-to-date?)
  (unless (equal? active-branch expected-branch)
    (displayln
     @~a{

         ERROR: The wrong branch of @repo-dir is active.
         current branch:  @active-branch
         required branch: @expected-branch
         }))
  (unless up-to-date?
    (displayln
     @~a{

         ERROR: @repo-dir is out of date (there are upstream commits)
         })))

(define (check-install-configuration racket-dir TR-dir gtp-dir)
  (displayln "Checking racket version...")
  (define racket-version-str
    (system/string @~a{@|racket-dir|/bin/racket --version}))
  (define racket-version-ok?
    (regexp-match? @~a{Racket v@(regexp-quote racket-version) \[cs\]} racket-version-str))

  (displayln "Checking blgt repo ...")
  (define blgt-active-branch (get-repo-current-branch repo-path))
  (define blgt-branch-ok? (equal? blgt-active-branch expected-blgt-branch))
  (define blgt-up-to-date? (repo-branch-up-to-date-with-remote? repo-path blgt-active-branch))

  (displayln "Checking gtp-benchmarks repo...")
  (define gtp-active-branch (get-repo-current-branch gtp-dir))
  (define gtp-branch-ok? (equal? gtp-active-branch expected-gtp-branch))
  (define gtp-up-to-date? (repo-branch-up-to-date-with-remote? gtp-dir gtp-active-branch))

  (displayln "Checking TR repo...")
  (define TR-active-branch (get-repo-current-branch TR-dir))
  (define TR-branch-ok? (equal? TR-active-branch expected-TR-branch))
  (define TR-up-to-date? (repo-branch-up-to-date-with-remote? TR-dir TR-active-branch))

  (displayln "Checking dbs...")
  (define dbs-ok? (check-expected-dbs))

  (unless racket-version-ok?
    (displayln
     @~a{

         ERROR: The installed racket has the wrong version.
         installed: @racket-version-str
         required:  Racket v@racket-version [cs]
         }))
  (report-repo-status repo-path blgt-active-branch expected-blgt-branch blgt-up-to-date?)
  (report-repo-status gtp-dir gtp-active-branch expected-gtp-branch gtp-up-to-date?)
  (report-repo-status TR-dir TR-active-branch expected-TR-branch TR-up-to-date?)

  (and racket-version-ok?
       blgt-branch-ok?
       blgt-up-to-date?
       gtp-branch-ok?
       gtp-up-to-date?
       TR-branch-ok?
       TR-up-to-date?
       dbs-ok?
       (check-TR-install racket-dir TR-dir #:display-failures? #t)))

(define (check-expected-dbs)
  (for*/and ([{dir dbs} (in-hash expected-dbs)]
             [{db-name should-have-all-expected-benchmarks?} (in-hash dbs)])
    (define db-path (build-path dbs-dir dir db-name))
    (check-db/keys db-path (and should-have-all-expected-benchmarks?
                                expected-benchmark-names))))

(main
 #:arguments {[flags args]
              #:once-each
              [("-v" "--verify-install")
               'verify-install
               ("Verify the current installation to ensure that"
                "all dependencies have the right versions or customizations.")
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
      (displayln "\nWARNING: Install is misconfigured.")
      (exit 1)]))
 (when (hash-ref flags 'verify-install)
   (verify-install!))

 (unless (or (directory-exists? racket-dir)
             (install-racket racket-dir))
   (eprintf "Something went wrong installing and setting up Racket.~n")
   (exit 1))

 (define raco-path (build-path racket-dir "bin" "raco"))

 (unless (directory-exists? TR-dir)
   (download-TR raco-path TR-dir))
 (modify-TR TR-dir)
 (unless (check-TR-install racket-dir TR-dir)
   (setup-existing-TR-dir! raco-path TR-dir))

 ;; Note: this must come AFTER installing TR, so that the pkgs which use TR use
 ;; the modified version of TR (which will behave the same as normal TR for the
 ;; pkgs, just some protocols in the implementation are different and if they
 ;; don't match we'll get runtime errors)
 (install-pkg-dependencies raco-path)

 (unless (directory-exists? gtp-dir)
   (install-gtp-repo gtp-dir))

 (when (user-prompt!
        @~a{

            Setup complete, though there may be warnings.

            See them by running this script again with `--verify-install` @;
            to verify the integrity of the install.

            Do you want to verify that everything is in working order
            by running the system tests?
            })
   (void (shell* raco-path
                 "test"
                 repo-path))))

(module test racket/base)
