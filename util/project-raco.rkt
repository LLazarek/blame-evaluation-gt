#lang at-exp rscript

;; project-raco has commands:
;; - make
;;   - compiles everything that is not a helper/utility script which can't be compiled on all systems (e.g. uses plot-util/quick)
;; - test
;;   - runs all tests, with the following extra features:
;;     - progress tracking and the ability to resume tests upon failure
;;     - running tests in parallel when possible
;;       + Specifically, we assume modules in different directories are safe to run in parallel
;; - progress
;;   - uses *-progress.log files to report on the progress of operations
;;   - for the main experiment, it can count how many mutants have completed

(require file/glob)

(define-runtime-paths
  [blame-evaluation-gt "../"])
(define glob-patterns-to-ignore
  '("mutation-analysis/plot-mutation-analyses.rkt"
    "util/places-suck-demonstration.rkt"
    "util/mutation-index-cache-performance-check.rkt"))
(define (ignored-path? p)
  (ormap (λ (pat) (glob-match? pat p))
         glob-patterns-to-ignore))
(define raco (build-path blame-evaluation-gt ".." "racket" "bin" "raco"))

(main
 #:arguments {[(hash-table ['compile compile?]
                           ['clean-compiled clean-compiled?])
               args]
              #:once-each
              [("-c" "--compile")
               'compile
               "Compile project code."
               #:record
               #:conflicts '(clean-compiled)]
              [("-C" "--clean-compiled")
               'clean-compiled
               "Clean all previously compiled project code."
               #:record]}
 (cond
   [compile?
    (parameterize ([current-directory blame-evaluation-gt])
      (apply system*
             raco
             "make"
             "-v"
             (for/list ([f (in-directory)]
                        #:when (and (path-has-extension? f ".rkt")
                                    (file-exists? f)
                                    (not (ignored-path? f))))
               f)))]
   [clean-compiled?
    (system* (find-executable-path "find")
             blame-evaluation-gt
             "-name" "compiled"
             "-type" "d"
             "-prune"
             "-exec" "rm" "-rf" "{}" ";")]))
