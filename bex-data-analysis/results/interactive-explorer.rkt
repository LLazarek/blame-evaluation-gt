#lang at-exp rscript

(require "summarize.rkt"
         bex/configurables/configurables
         bex/util/path-utils
         racket/random)

(define (read-user-input-line! msg)
  (display (~a msg " "))
  (flush-output)
  (string-trim (read-line)))

(define (summary-hash->counts-hash hash)
  (for/hash ([{k bts} (in-hash hash)])
    (values k (length bts))))

(define (string->value s)
  (call-with-input-string s read))

(define (explore-bts! bts)
  (match bts
    [(? list? bt-list)
     (let loop ()
       (displayln @~a{

                      List of @(length bt-list) bts.
                      })
       (match (read-user-input-line! "Pick an index, or ask for random:")
         [(or "q" "quit" "exit") (void)]
         ["random"
          (pretty-write (random-ref bt-list))
          (loop)]
         [(app string->number (? number? n))
          (pretty-write (list-ref bt-list n))
          (loop)]
         [else
          (displayln "Try again.")
          (loop)]))]
    [(? (conjoin hash?
                 (λ (h) (andmap number? (hash-values h))))
        bt-count-hash)
     (displayln "Raw count hash:")
     (pretty-write bt-count-hash)]
    [(? hash? bt-hash)
     (let loop ()
       (displayln @~a{

                      Hash of bts:
                      @pretty-format[(summary-hash->counts-hash bt-hash)]
                      })
       (match (read-user-input-line! "Pick a key:")
         [(or "q" "quit" "exit") (void)]
         [(app string->value key)
          #:when (hash-has-key? bt-hash key)
          (match (hash-ref bt-hash key)
            [(and inner-thing (or (? list?) (? hash?)))
             (explore-bts! inner-thing)
             (loop)]
            [other-thing
             (displayln @~a{Don't know what to do with @~s[other-thing]})
             (loop)])]))]))

(main
 #:arguments ([(hash-table ['config   config-path]
                           ['summaries-db (app simple-form-path summaries-db-path)])
               data-dirs]
              #:once-each
              [("-c" "--config")
               'config
               ("Config for obtaining active mutator names.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              [("-S" "--mutant-summaries")
               'summaries-db
               ("Path to the db containing summaries of the mutants in the data.")
               #:collect {"path" take-latest #f}
               #:mandatory]
              #:args data-dirs)
 #:check [(andmap path-to-existant-directory? data-dirs)
          @~a{Can't find @(filter-not path-to-existant-directory? data-dirs)}]

 (install-configuration! config-path)

 (let loop ()
   (displayln @~a{
                  Exploring data in
                  @pretty-format[data-dirs]

                  })
   (match (read-user-input-line! "Select a mode to explore, or quit:")
     [(or "q" "quit" "exit") (void)]
     [(app (λ (name) (pick-file-by-name data-dirs name)) (? string? data-dir))
      (define summary (summarize data-dir summaries-db-path))
      (define-values {summary-table-str keymap}
        (format-summary summary #:with-keys #t))
      (let inner-loop ()
        (displayln "\n\n")
        (displayln summary-table-str)
        (match (read-user-input-line! "Select a key to explore, or quit:")
          [(or "Q" "Quit" "Exit") (void)]
          [(or "q" "quit" "exit") (loop)]
          [(app string->number key)
           #:when (hash-has-key? keymap key)
           (explore-bts! (hash-ref summary (hash-ref keymap key)))
           (inner-loop)]
          [other
           (displayln @~a{No key '@other', try again.})
           (inner-loop)]))]
     [other
      (displayln @~a{Don't know @other, try again.})
      (loop)])))
