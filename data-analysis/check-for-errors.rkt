#lang at-exp rscript

(require "read-data.rkt")

;; -> any
;; Prints any errors encountered
(define (check-log-for-errors log-path)
  (define context-lines 5)
  (match (system/string @~a{ag -i -A @context-lines '\* (error)' '@log-path'})
    ["" empty]
    [errors-str
     (define errors (string-split errors-str "\n--\n"))
     errors]))

;; (listof string?) . -> . (hash/c string? (listof string?))
(define (categorize-errors errors)
  (define category
    (match-lambda [(regexp #rx"⚠ Some mutants logged error messages")
                   "Error output"]
                  [(regexp @regexp{
                                   Found that a configuration produces blamed, @;
                                   but configuration outcomes db says @;
                                   that it should produce runtime-error
                                   })
                   "Parity: blame when TR said runtime error"]
                  [(regexp @regexp{
                                   Found that a configuration produces (timeout|oom), @;
                                   but configuration outcomes db says @;
                                   that it should produce
                                   })
                   "Parity: timeout/oom"]
                  [other "?"]))
  (define ((add-to-list v) l) (cons v l))

  (for/fold ([categorization (hash)])
            ([error (in-list errors)])
    (hash-update categorization
                 (category error)
                 (add-to-list error)
                 empty)))

(main
 #:arguments ([flags mode-dirs]
              #:args mode-directory)
 #:check [(andmap path-to-existant-directory? mode-dirs)
          @~a{Unable to find directories: @(filter-not path-to-existant-directory? mode-dirs)}]

 (define found-errors-box (box #f))
 (for* ([mode-dir (in-list mode-dirs)]
        [bench-data-files (in-list (find-data-files mode-dir))])
   (define name (~a (basename mode-dir)
                    ": "
                    (benchmark-data-files-name bench-data-files)))
   (define errors (check-log-for-errors (benchmark-data-files-log bench-data-files)))
   (cond [(empty? errors)
          (displayln @~a{@name has no errors logged})]
         [else
          (displayln @~a{@name has @(length errors) errors logged})
          (define categories (categorize-errors errors))
          (pretty-display (for/hash ([{category errs} (in-hash categories)])
                            (define show
                              (if (string=? category "?")
                                  (list (length errs) (first errs))
                                  (length errs)))
                            (values category show)))
          (newline)
          (set-box! found-errors-box #t)]))
 (exit (if (unbox found-errors-box) 1 0)))
