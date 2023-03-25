#lang at-exp racket

(provide generate-adapter-ctc
         ->stx
         expand-field-count-for-child-fields
         lookup-td->adapter-generation-info
         type-diff->adapter/delegate
         merge-adapters

         (struct-out mutated-interface-type)
         make-base-type-adapter

         sexp-diff
         sexp->type-diff)

(require "../configurables/mutation/type-api-mutators.rkt"
         "../util/experiment-exns.rkt"
         "sexp-diff.rkt"
         "util.rkt"
         syntax/parse/define
         (for-syntax syntax/parse
                     racket/syntax
                     racket/list)
         racket/struct
         racket/syntax
         syntax/location)

(struct mutated-interface-type (original mutated mutation-type)
  #:transparent)

(define (mapping f)
  (λ (l) (map f l)))

;; type-diff
(struct td () #:transparent)

(define-simple-macro (td-struct name fields)
  (begin (struct name td fields #:transparent)
         (module+ tds (provide (struct-out name)))))

(td-struct td:-> (specified-arg-count
                  argument-index-map
                  maybe-rest-arg-td
                  result-index-map))
(td-struct td:->* (mandatory-arg-count optional-argument-index-map optional-kw-argument-map))
(td-struct td:and (left right))
(td-struct td:base (original new))
(td-struct td:parametric (vars sub-td))
(td-struct td:rec (name sub-td))
(td-struct td:vector (index-map))
(td-struct td:struct (maybe-parent name field-count index-map))
(td-struct td:listof (sub-td))
(td-struct td:vectorof (sub-td))
(td-struct td:boxof (sub-td))
(td-struct td:parameterof (sub-td))
(td-struct td:setof (sub-td))
(td-struct td:instanceof (sub-td))
(td-struct td:option (sub-td))
(td-struct td:pairof (left right))
(td-struct td:hash (key val))
(td-struct td:list* (len index-map tail-sub-td))
(td-struct td:class (init-field-td-map field-td-map method-td-map))
#;(td-struct td:without-inits (sub-td-class))

;; used to support diffs that we may not otherwise support, produced by arg swapping.
;; e.g. Swapping `(Listof A) (Setof A)` produces a diff
;; `((difference Listof Setof) A)`
;; Really all that matters for handling swaps tho is knowing that there is a diff between
;; two arg positions, so wrap it up in this td to notice that later.
;; If the mutation is really of a new type requiring more support, the error will still show up
;; later in `type-diff->adapter/delegate`.
(struct td:unknown-type td (sub-tds) #:transparent)

(define-logger adapter-generation)

(define (sexp->type-diff a-sexp-diff)
  (define (list->td-index-map a-list)
    (match a-list
      [(? td? single) `((0 . ,single))]
      [mixed-list
       (for/list ([maybe-td (in-list mixed-list)]
                  [index    (in-naturals)]
                  #:when (td? maybe-td))
         (cons index maybe-td))]))
  (log-adapter-generation-debug
   @~a{sexp->type-diff: @~s[a-sexp-diff]})
  (define td
    (let recur ([sexp-diff-part a-sexp-diff])
      (match (normalize->-types sexp-diff-part)
        [(list* '->
                (app parse->-parts
                     (list (app (mapping recur) arg-tds)
                           (app recur maybe-rest-arg-td)
                           (app (mapping recur) result-tds))))
         (define arg-map (list->td-index-map arg-tds))
         (define result-map (list->td-index-map result-tds))
         (and (or (not (empty? arg-map))
                  (not (empty? result-map))
                  maybe-rest-arg-td)
              (td:-> (length arg-tds)
                     arg-map
                     maybe-rest-arg-td
                     result-map))]
        [(list* '->*
                (app parse->*-parts
                     (list (app (mapping recur) mandatory-arg-tds)
                           (list (app (mapping recur) optional-arg-tds)
                                 kws)
                           (app recur maybe-rest-arg-td)
                           (app (mapping recur) result-tds))))
         (define mandatory-arg-map (list->td-index-map mandatory-arg-tds))
         (define optional-arg-map (list->td-index-map optional-arg-tds))
         (define optional-kw-map
           (for*/list ([{kw diff} (in-dict kws)]
                       [td (in-value (recur diff))]
                       #:when td)
             (cons kw td)))
         (define result-map (list->td-index-map result-tds))
         (define optional-part-adapter
           (and (not (and (empty? optional-arg-map)
                          (empty? optional-kw-map)))
                (td:->* (length mandatory-arg-tds)
                        optional-arg-map
                        optional-kw-map)))
         (define mandatory-part-adapter
           (and (or (not (empty? mandatory-arg-map))
                    (not (empty? result-map))
                    maybe-rest-arg-td)
                (td:-> (length mandatory-arg-tds)
                       mandatory-arg-map
                       maybe-rest-arg-td
                       result-map)))
         (if (and mandatory-part-adapter
                  optional-part-adapter)
             (td:and mandatory-part-adapter
                     optional-part-adapter)
             (or mandatory-part-adapter
                 optional-part-adapter))]
        [(list* 'case->
                (app (mapping recur) case-tds))
         (findf values case-tds)]
        [(list 'All vars (app recur sub-td))
         (and sub-td (td:parametric vars sub-td))]
        [(list 'Rec var (app recur sub-td))
         (and sub-td (td:rec var sub-td))]
        [(list* 'Vector (and (list _ ... (? td?) _ ... #f)
                             sub-tds))
         (td:vector (list->td-index-map sub-tds))]
        [(list 'Listof (app recur sub-td))
         (and sub-td (td:listof sub-td))]
        [(list 'List (app recur sub-tds) ...)
         (define index-map (list->td-index-map sub-tds))
         (and (not (empty? index-map))
              (td:list* (length sub-tds)
                        index-map
                        #f))]
        [(list 'List* (app recur sub-tds) ... (app recur tail-sub-td))
         (define index-map (list->td-index-map sub-tds))
         (and (or (not (empty? index-map))
                  tail-sub-td)
              (td:list* (length sub-tds)
                        index-map
                        tail-sub-td))]
        [(list 'Vectorof (app recur sub-td))
         (and sub-td (td:vectorof sub-td))]
        [(list 'Boxof (app recur sub-td))
         (and sub-td (td:boxof sub-td))]
        [(list 'Setof (app recur sub-td))
         (and sub-td (td:setof sub-td))]
        [(list 'Option (app recur sub-td))
         (and sub-td (td:option sub-td))]
        [(list 'Pairof (app recur car) (app recur cdr))
         (and (or car cdr)
              (td:pairof car cdr))]
        [(list 'HashTable (app recur key) (app recur val))
         (and (or key val)
              (td:hash key val))]
        [(list 'Parameterof (app recur sub-td))
         (and sub-td (td:parameterof sub-td))]
        [(list* (or 'struct 'struct:)
                (? symbol? name)
                parent ...
                (list [list fields ': (app recur sub-tds)] ...)
                (or '()
                    (list* (? keyword?) _)))
         (define field-map (list->td-index-map sub-tds))
         (define maybe-parent-td
           (match (map recur parent)
             [(list (? td? p)) p]
             [else #f]))
         (and (or maybe-parent-td
                  (not (empty? field-map)))
              (td:struct maybe-parent-td name (length fields) field-map))]
        [(list* 'Class
                (app parse-class-parts
                     (list (app recur implements-clause-sub-td)
                           (app recur implements/inits-clause-sub-td)
                           (list (list init-field-names
                                       (app recur init-field-sub-tds)
                                       '#:optional ...)
                                 ...)
                           (list (list field-names (app recur field-sub-tds))
                                 ...)
                           (list (list method-names (app recur method-sub-tds))
                                 ...))))
         (define init-field-dict
           (for/list ([name (in-list init-field-names)]
                      [sub-td (in-list init-field-sub-tds)]
                      #:when (td? sub-td))
             (cons name sub-td)))
         (define field-dict
           (for/list ([name (in-list field-names)]
                      [sub-td (in-list field-sub-tds)]
                      #:when (td? sub-td))
             (cons name sub-td)))
         (define method-dict
           (for/list ([name (in-list method-names)]
                      [sub-td (in-list method-sub-tds)]
                      #:when (td? sub-td))
             (cons name sub-td)))
         (define immediate-td
           (and (not (and (false? implements-clause-sub-td)
                          (false? implements/inits-clause-sub-td)
                          (empty? init-field-dict)
                          (empty? field-dict)
                          (empty? method-dict)))
                (td:class init-field-dict
                          field-dict
                          method-dict)))
         (cond [implements/inits-clause-sub-td
                (td:and immediate-td
                        implements/inits-clause-sub-td)]
               [implements-clause-sub-td
                (td:and immediate-td
                        (try-remove-inits implements-clause-sub-td))]
               [else
                immediate-td])]
        [(list 'Instance (app recur sub-td))
         (and sub-td (td:instanceof sub-td))]
        [(difference original new)
         (td:base original new)]
        [(or (? symbol?) #t #f (? number?) (? keyword?) (? string?)) #f]
        [(list* 'U
                (app (mapping recur)
                     (list _ ... (? td?) _ ...)))
         ;; Union branch mutations just turn into a seal of the whole thing
         (td:base '<U> '<mutated-U>)]
        [(? list? (app (mapping recur) (and sub-tds (list-no-order (? td?) _ ...))))
         (td:unknown-type (filter values sub-tds))]
        [(? list?) ;; no sub-tds since the above case didn't match
         #f]
        [something-else
         (error 'sexp->type-diff
                @~a{
                    Don't know how to convert sexp-diff to type-diff:
                    @~s[a-sexp-diff]
                    specifically, this part is unrecognized:
                    @~s[something-else]
                    })])))
  (unless td
    (error 'sexp->type-diff
           @~a{
               No diff found in argument.
               Expected: a sexp-diff
               Given: @~s[a-sexp-diff]
               }))
  td)

(define (parse-positional/kw-args optional-args-list)
  (match optional-args-list
    [(list* (? (negate keyword?) positional) ...
            (? keyword? kw)
            kws)
     (list positional
           (for/list ([kw (in-list (cons kw kws))]
                      [arg (in-list kws)]
                      [i (in-naturals)]
                      #:when (even? i))
             (cons kw arg)))]
    [only-positional-args
     (list only-positional-args
           empty)]))

(define-simple-macro (multi-match e
                                  [pat result #:otherwise default]
                                  ...)
  (list (match e [pat result] [else default]) ...))
(define-simple-macro (remove-pats [part ...] ...)
  (λ (l) (for/fold ([l l])
                   ([f (list (match-lambda [(list before ___ part ... after ___)
                                            (append before after)]
                                           [l l])
                             ...)])
           (f l))))
;; sexp? -> (list/c (or/c sexp? #f) ; implements
;;                  (or/c sexp? #f) ; implements/inits
;;                  (listof sexp)   ; init-fields
;;                  (listof sexp)   ; fields
;;                  (listof sexp)   ; methods
;;                  )
;; Assumption: all fields/init-fields are grouped together under a single
;; field/init-field block, and those blocks (if any) appear before all methods.
(define (parse-class-parts class-type-body)
  (multi-match
   class-type-body

   [(somewhere '#:implements implements-other-class-type)
    implements-other-class-type
    #:otherwise #f]
   [(somewhere '#:implements/inits implements-other-class-type)
    implements-other-class-type
    #:otherwise #f]
   [(somewhere (list 'init-field init-fields ...))
    init-fields
    #:otherwise empty]
   [(somewhere (list 'field init-fields ...))
    init-fields
    #:otherwise empty]
   [(app (remove-pats ['#:implements _]
                      ['#:implements/inits _]
                      [(list* 'init-field _)]
                      [(list* 'field _)])
         methods)
    methods
    #:otherwise (error 'parse-class-parts
                       @~a{
                           one or more assumptions about the shape of class types @;
                           have been violated in:
                           @~s[(cons 'Class class-type-body)]
                           })]))

;; (require syntax/parse)
;; (define (parse->*-parts ->*-contents)
;;   (syntax-parse ->*-contents
;;     [((mandatory-arg ...)
;;       {~optional (optional-arg ...) #:defaults ([(optional-arg 1) '()])}
;;       {~optional {~seq #:rest rest-t}}
;;       {~or ({~or {~datum values} {~datum Values}} results ...)
;;            result})
;;      (list (map syntax->datum (attribute mandatory-arg))
;;            (parse-positional/kw-args (map syntax->datum (attribute optional-arg)))
;;            (map syntax->datum (syntax->list #'[{~? rest-t}]))
;;            (map syntax->datum (syntax->list #'[{~? result {~@ results ...}}])))]))

(define parse-results
  (match-lambda [(list (or 'Values 'values) results ...)
                 results]
                [single-result
                 (list single-result)]))
(define (parse->*-parts ->*-contents)
  (define parse-rest+rng (match-lambda [(list '#:rest rest (app parse-results results))
                                        (list rest results)]
                                       [(list (app parse-results results))
                                        (list #f results)]))
  (match ->*-contents
    [(list* (list mandatory-args ...)
            (list optional-args ...) ...
            (and (or (list* '#:rest _)
                     (list _))
                 (app parse-rest+rng
                      (list rest
                            results))))
     (list mandatory-args
           (parse-positional/kw-args
            (match optional-args
              ['() '()]
              [(list inner) inner]))
           rest
           results)]))

(define (parse->-parts ->-contents)
  (match ->-contents
    [(list mandatory-args ...
           rest-arg (or '* '...)
           (app parse-results
                results))
     (list mandatory-args
           rest-arg
           results)]
    [(list mandatory-args ...
           (app parse-results
                results))
     (list mandatory-args
           #f
           results)]))

(define (try-remove-inits td)
  (match td
    [(td:class _ fields mtds)
     (td:class empty
               fields
               mtds)]
    [other other]))

(define (normalize->-types t)
  (match t
    [(list before ... (and -> (or '-> '->*)) after ...)
     (cons -> (append before after))]
    [other other]))

(module+ test
  (require ruinit)

  (test-begin
    #:name parse-positional/kw-args
    (test-equal? (parse-positional/kw-args '())
                 (list empty empty))
    (test-equal? (parse-positional/kw-args '(A B C))
                 (list '(A B C) empty))
    (test-equal? (parse-positional/kw-args '(A B C #:x X))
                 (list '(A B C)
                       '((#:x . X))))
    (test-equal? (parse-positional/kw-args '(A B C #:x X #:y Y))
                 (list '(A B C)
                       '((#:x . X)
                         (#:y . Y)))))
  (test-begin
    #:name parse->*-parts
    (test-equal? (parse->*-parts '((Nat Str) Nat))
                 '((Nat Str) (() ()) #f (Nat)))
    (test-equal? (parse->*-parts '((Nat Str) (Str #:f F) Nat))
                 '((Nat Str) ((Str) ((#:f . F))) #f (Nat)))
    (test-equal? (parse->*-parts '((Nat Str) (Str #:f F) #:rest F Nat))
                 '((Nat Str) ((Str) ((#:f . F))) F (Nat)))
    (test-equal? (parse->*-parts '((Nat Str) (Str #:f F) Nat))
                 '((Nat Str) ((Str) ((#:f . F))) #f (Nat)))
    (test-equal? (parse->*-parts '((Nat Str) (Str #:f F) (Values Nat Nat)))
                 '((Nat Str) ((Str) ((#:f . F))) #f (Nat Nat)))
    (test-equal? (parse->*-parts `((Nat Str) (Str #:f F) (Values ,(difference 'Nat 'Any) Nat)))
                 (list '(Nat Str) '((Str) ((#:f . F))) #f (list (difference 'Nat 'Any) 'Nat))))

  (test-begin
    #:name normalize->-types
    (test-equal? (normalize->-types '(A B -> C))
                 '(-> A B C))
    (test-equal? (normalize->-types '((A B -> C)))
                 '((A B -> C)))
    (test-equal? (normalize->-types '((A) (B) ->* C))
                 '(->* (A) (B) C)))

  (test-begin
    #:name sexp->type-diff
    (test-exn exn? (sexp->type-diff (sexp-diff 'A 'A)))
    (test-equal? (sexp->type-diff (sexp-diff 'A 'B))
                 (td:base 'A 'B))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A C)
                                             '(-> B C)))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A (listof C))
                                             '(-> B (listof C))))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A Z Z Z C)
                                             '(-> B Z Z Z C)))
                 (td:-> 4
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z A Z C)
                                             '(-> Z Z B Z C)))
                 (td:-> 4
                        `((2 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> X Z A Z C)
                                             '(-> Y Z B Z C)))
                 (td:-> 4
                        `((0 . ,(td:base 'X 'Y))
                          (2 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z A)
                                             '(-> Z Z Z B)))
                 (td:-> 3
                        '()
                        #f
                        `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values A))
                                             '(-> Z Z Z (values B))))
                 (td:-> 3
                        '()
                        #f
                        `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A))
                                             '(-> Z Z Z (values Z B))))
                 (td:-> 3
                        '()
                        #f
                        `((1 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A Z X))
                                             '(-> Z Z Z (values Z B Z Y))))
                 (td:-> 3
                        '()
                        #f
                        `((1 . ,(td:base 'A 'B))
                          (3 . ,(td:base 'X 'Y)))))
    (test-equal? (sexp->type-diff (sexp-diff '(Z Z Z -> (values Z A Z X))
                                             '(Z Z Z -> (values Z B Z Y))))
                 (td:-> 3
                        '()
                        #f
                        `((1 . ,(td:base 'A 'B))
                          (3 . ,(td:base 'X 'Y)))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z (-> A C)))
                                             '(-> Z Z Z (values Z (-> B C)))))
                 (td:-> 3
                        '()
                        #f
                        `((1 . ,(td:-> 1
                                       `((0 . ,(td:base 'A 'B)))
                                       #f
                                       '())))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> (U A B) (U C D) C)
                                             '(-> (U C D) (U A B) C)))
                 (td:-> 2
                        `((0 . ,(td:base '<U> '<mutated-U>))
                          (1 . ,(td:base '<U> '<mutated-U>)))
                        #f
                        '()))

    (test-equal? (sexp->type-diff (sexp-diff '(struct
                                               stream
                                                ((first : Natural) (rest : (-> stream)))
                                                #:prefab)
                                             '(struct
                                               stream
                                                ((first : Index) (rest : (-> stream)))
                                                #:prefab)))
                 (td:struct #f 'stream 2 `((0 . ,(td:base 'Natural 'Index)))))
    (test-equal? (sexp->type-diff (sexp-diff '(struct
                                               stream
                                                stream-parent
                                                ((first : Natural) (rest : (-> stream)))
                                                #:prefab)
                                             '(struct
                                               stream
                                                stream-parent522788
                                                ((first : Natural) (rest : (-> stream)))
                                                #:prefab)))
                 (td:struct (td:base 'stream-parent 'stream-parent522788) 'stream 2 '()))
    (test-equal? (sexp->type-diff (sexp-diff '(struct
                                               stream
                                                parent
                                                ((first : Natural) (rest : (-> stream)))
                                                #:prefab)
                                             '(struct
                                               stream
                                                parent
                                                ((first : Index) (rest : (-> stream)))
                                                #:prefab)))
                 (td:struct #f 'stream 2 `((0 . ,(td:base 'Natural 'Index)))))
    (test-equal? (sexp->type-diff (sexp-diff '(Listof A)
                                             '(Listof B)))
                 (td:listof (td:base 'A 'B)))
    (test-equal? (sexp->type-diff (sexp-diff '(List A B C D)
                                             '(List A B Z D)))
                 (td:list* 4
                           `((2 . ,(td:base 'C 'Z)))
                           #f))
    (test-equal? (sexp->type-diff (sexp-diff '(List* A B C DD)
                                             '(List* A B Z ZZ)))
                 (td:list* 3
                           `((2 . ,(td:base 'C 'Z)))
                           (td:base 'DD 'ZZ)))
    (test-equal? (sexp->type-diff (sexp-diff '(Setof A)
                                             '(Setof B)))
                 (td:setof (td:base 'A 'B)))
    (test-exn exn?
              (sexp->type-diff (sexp-diff '(Listof A)
                                          '(Listof A))))
    (test-equal? (sexp->type-diff (sexp-diff '(Parameterof A)
                                             '(Parameterof B)))
                 (td:parameterof (td:base 'A 'B)))
    (test-equal? (sexp->type-diff (sexp-diff '(Pairof A B)
                                             '(Pairof C B)))
                 (td:pairof (td:base 'A 'C) #f))
    (test-equal? (sexp->type-diff (sexp-diff '(HashTable A B)
                                             '(HashTable C B)))
                 (td:hash (td:base 'A 'C) #f))

    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (C) R)
                                             '(->* (B) (C) R)))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (C) #:rest RST R)
                                             '(->* (B) (C) #:rest RST R)))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (C) #:rest RST R)
                                             '(->* (A) (C) #:rest OPS R)))
                 (td:-> 1
                        '()
                        (td:base 'RST 'OPS)
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (C) R)
                                             '(->* (A) (B) R)))
                 (td:->* 1
                         `((0 . ,(td:base 'C 'B)))
                         '()))
    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (B #:c C) R)
                                             '(->* (A) (B #:c Z) R)))
                 (td:->* 1
                         '()
                         `((#:c . ,(td:base 'C 'Z)))))

    (test-equal? (sexp->type-diff (sexp-diff '(Class (sign-up (->* (A) (C) R)))
                                             '(Class (sign-up (->* (A) (B) R)))))
                 (td:class '()
                           '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field) (sign-up (->* (A) (C) R)))
                                             '(Class (init-field) (sign-up (->* (A) (B) R)))))
                 (td:class '()
                           '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (field) (sign-up (->* (A) (C) R)))
                                             '(Class (field) (sign-up (->* (A) (B) R)))))
                 (td:class '()
                           '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]) (sign-up (->* (A) (C) R)))
                                             '(Class (init-field [a Y]) (sign-up (->* (A) (B) R)))))
                 (td:class `((a . ,(td:base 'X 'Y)))
                           '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]))
                                             '(Class (init-field [a Y]))))
                 (td:class `((a . ,(td:base 'X 'Y)))
                           '()
                           '()))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (field [a X]))
                                             '(Class (field [a Y]))))
                 (td:class '()
                           `((a . ,(td:base 'X 'Y)))
                           '()))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]) (field [i Any]))
                                             '(Class (init-field [a Y]) (field [i Any]))))
                 (td:class `((a . ,(td:base 'X 'Y)))
                           '()
                           '()))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [i Any]) (field [a X]))
                                             '(Class (init-field [i Any]) (field [a Y]))))
                 (td:class '()
                           `((a . ,(td:base 'X 'Y)))
                           '()))
    (test-equal? (sexp->type-diff
                  (sexp-diff '(Class (init-field (next-tile (-> (Listof Tile) Tile)))
                                     (sign-up (-> String (Instance Player%) String))
                                     (show-players (-> (Listof String)))
                                     (run (->* (Natural) (#:show (-> Void)) RunResult)))
                             '(Class (init-field (next-tile (-> (Listof Tile) Tile)))
                                     (sign-up (-> String (Instance Player%) String))
                                     (show-players (-> (Listof String)))
                                     (run (->* (Integer) (#:show (-> Void)) RunResult)))))
                 (td:class '()
                           '()
                           `((run . ,(td:-> 1
                                            `((0 . ,(td:base 'Natural 'Integer)))
                                            #f
                                            '())))))
    (test-equal? (sexp->type-diff
                  (sexp-diff '(Class (init-field (next-tile (-> (Listof Tile) Tile)))
                                     (sign-up (-> String (Instance Player%) String))
                                     (show-players (-> (Listof String))))
                             '(Class (init-field (next-tile (-> (Listof Tile) Tile)))
                                     (sign-up (-> String (Instance Any) String))
                                     (show-players (-> (Listof String))))))
                 (td:class '()
                           '()
                           `((sign-up . ,(td:-> 2
                                                `((1 . ,(td:instanceof (td:base 'Player% 'Any))))
                                                #f
                                                '())))))

    (test-equal? (sexp->type-diff (sexp-diff '(case-> (-> A C)
                                                      (->* (A) (B) C))
                                             '(case-> (-> B C)
                                                      (->* (A) (B) C))))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(case-> (-> A C)
                                                      (->* (A) (B) C))
                                             '(case-> (-> A C)
                                                      (->* (B) (B) C))))
                 (td:-> 1
                        `((0 . ,(td:base 'A 'B)))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(case-> (-> A C)
                                                      (->* (A) (B) C))
                                             '(case-> (-> A C)
                                                      (->* (A) (Z) C))))
                 (td:->* 1
                         `((0 . ,(td:base 'B 'Z)))
                         '()))

    (test-equal? (sexp->type-diff (sexp-diff '(-> (A B C) R)
                                             '(-> (A Z C) R)))
                 (td:-> 1
                        `((0 . ,(td:unknown-type (list (td:base 'B 'Z)))))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> (A B C) R)
                                             '(-> (A Z C) R)))
                 (td:-> 1
                        `((0 . ,(td:unknown-type (list (td:base 'B 'Z)))))
                        #f
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(->* (A B) ((-> Z Z Z)) C)
                                             '(->* (A Z) ((-> Z Z Q)) Z)))
                 (td:and (td:-> 2
                                `((1 . ,(td:base 'B 'Z)))
                                #f
                                `((0 . ,(td:base 'C 'Z))))
                         (td:->* 2
                                 `((0 . ,(td:-> 2 '() #f `((0 . ,(td:base 'Z 'Q))))))
                                 '())))
    (test-equal? (sexp->type-diff (sexp-diff
                                   '(struct
                                      Array
                                      ((shape : (Vectorof Integer))
                                       (size : Integer)
                                       (strict? : (Boxof Boolean))
                                       (strict! : (-> Void))
                                       (unsafe-proc : (-> (Vectorof Integer) Float)))
                                      #:prefab)
                                   '(struct
                                      Array
                                      ((shape : (-> (Vectorof Integer) Float))
                                       (size : Integer)
                                       (strict? : (Boxof Boolean))
                                       (strict! : (-> Void))
                                       (unsafe-proc : (Vectorof Integer)))
                                      #:prefab)))
                 (td:struct #f
                            'Array
                            5
                            `((0 . ,(td:base '(Vectorof Integer)
                                             '(-> (Vectorof Integer) Float)))
                              (4 . ,(td:base '(-> (Vectorof Integer) Float)
                                             '(Vectorof Integer))))))
    (test-equal?
     (sexp->type-diff
      (sexp-diff '(Class
                   (init-field (n Name)
                               (order (-> (Listof Card) (Listof Card)) #:optional)))
                 '(Class
                   (init-field (n Any)
                               (order (-> (Listof Card) (Listof Card)) #:optional)))))
     (td:class `((n . ,(td:base 'Name 'Any)))
               '()
               '()))
    (test-equal?
     (sexp->type-diff
      (sexp-diff '(Class
                   #:implements FooBar
                   (init-field [a Number]
                               [b String]
                               [c Number])
                   (m (-> Number String String)))
                 '(Class
                   #:implements FooBar87894
                   (init-field [a Number]
                               [b Any]
                               [c Number])
                   (m (-> Number String String)))))
     (td:and (td:class `((b . ,(td:base 'String 'Any)))
                       '()
                       '())
             (td:base 'FooBar 'FooBar87894)))))

;; mutated-interface-type? -> contract?
(define (generate-adapter-ctc a-mutated-interface-type)
  (match-define (mutated-interface-type original
                                        mutated
                                        mutation-type)
    a-mutated-interface-type)
  (assert (not (equal? original mutated))
          #:name 'mutation-adapter:generate-adapter-ctc
          @~a{
              Original and mutated are the same?
              @~s[original]
              vs
              @~s[mutated]
              })
  (define type-diff (sexp->type-diff (sexp-diff original mutated)))
  (apply type-diff->adapter/delegate
         type-diff
         (lookup-td->adapter-generation-info mutation-type)))

(struct recur () #:transparent)
;; td?
;; (td? position? -> boolean?)
;; (td? position? -> contract?)
;; ->
;; contract?
;;
;; Converts `td` into a contract from the bottom up using `is-leaf?` and
;; `leaf->adapter`. `is-leaf?` decides where the "bottom" of the tree is, and
;; `leaf->adapter` converts the bottom into a base contract. All layers above
;; the base contract are converted into adapters that delegate down to the base.
(define (type-diff->adapter/delegate td is-leaf? leaf->adapter)
  (let loop ([inner-td td]
             [current-position 'pos])
    (define (loop-over-dict-values d pos)
      (for/list ([{k td} (in-dict d)])
        (cons k (loop td pos))))

    (match* {(is-leaf? inner-td current-position) inner-td}
      [{#t _} (leaf->adapter inner-td current-position)]
      [{#f (? td:base? base)}
       (error 'type-diff->adapter/delegate
              @~a{given leaf predicate @~e[is-leaf?] asked to recur into td:base @~e[base]})]
      [{#f (td:-> specified-arg-count arg-index-map maybe-rest-arg-td result-index-map)}
       (delegating-> specified-arg-count
                     (loop-over-dict-values arg-index-map (flip current-position))
                     (if maybe-rest-arg-td
                         (loop maybe-rest-arg-td (flip current-position))
                         (any/c-adapter))
                     (loop-over-dict-values result-index-map current-position))]
      [{#f (td:->* n optional-arg-index-map optional-kw-arg-map)}
       (delegating->* n
                      (loop-over-dict-values optional-arg-index-map (flip current-position))
                      (loop-over-dict-values optional-kw-arg-map (flip current-position)))]
      [{#f (td:and left right)}
       (delegating-and/c (loop left current-position)
                         (loop right current-position))]
      [{#f (td:parametric vars sub-td)}
       ;; We won't mutate a type variable (unless of course it's spelled the
       ;; same as a base type... let's hope not) so we can just treat it as a
       ;; non-parametric type for the adapter's purposes.
       ;; i.e. just ignore it the parametric part!
       (loop sub-td current-position)]
      [{#f (td:rec var sub-td)}
       ;; Turns out that the bencmarks have Rec types of this form only:
       ;; (Rec X (U t ... X t ...))
       ;; This, coupled with the fact that we turn all mutations of union
       ;; types into a single sealing adapter, means we can just ignore this!
       (loop sub-td current-position)]
      [{#f (td:struct maybe-parent name field-count index-map)}
       (define parent-adapter (and maybe-parent
                                   (loop maybe-parent current-position)))
       (delegating-struct (and parent-adapter
                               (expand-field-count-for-child-fields parent-adapter
                                                                    field-count))
                          name
                          field-count
                          (loop-over-dict-values index-map current-position))]
      [{#f (td:listof sub-td)}
       (delegating-listof (loop sub-td current-position))]
      [{#f (td:list* len index-map maybe-tail-sub-td)}
       (delegating-list* len
                         (loop-over-dict-values index-map current-position)
                         (if maybe-tail-sub-td
                             (loop maybe-tail-sub-td current-position)
                             (any/c-adapter)))]
      [{#f (td:vectorof sub-td)}
       (delegating-vectorof (loop sub-td current-position))]
      [{#f (td:boxof sub-td)}
       (delegating-boxof (loop sub-td current-position))]
      [{#f (td:setof sub-td)}
       (delegating-setof (loop sub-td current-position))]
      [{#f (td:pairof left right)}
       (delegating-pairof (if left (loop left current-position) (any/c-adapter))
                          (if right (loop right current-position) (any/c-adapter)))]
      [{#f (td:hash left right)}
       (delegating-hash/c (if left (loop left current-position) (any/c-adapter))
                          (if right (loop right current-position) (any/c-adapter)))]
      [{#f (td:parameterof sub-td)}
       (delegating-parameter/c (loop sub-td current-position))]
      [{#f (td:option sub-td)}
       (delegating-option (loop sub-td current-position))]
      [{#f (td:class init-field-td-map field-td-map method-td-map)}
       (delegating-class/c (loop-over-dict-values init-field-td-map current-position)
                           (loop-over-dict-values field-td-map current-position)
                           (loop-over-dict-values method-td-map current-position))]
      #;[{#f (td:without-inits sub-td)}
       (delegating-class/c-without-inits (loop sub-td current-position))]
      [{#f (td:instanceof sub-td)}
       (delegating-instanceof (loop sub-td current-position))]
      [{_ _}
       (error 'type-diff->adapter/delegate
              @~a{
                  Unexpected leaf-predicate request to recur from @is-leaf?
                  on @~s[inner-td]
                  which is part of @~s[td]
                  })])))

(module+ test
  (test-begin
    #:name type-diff->adapter/delegate
    (test-equal? (let/ec return
                   (type-diff->adapter/delegate
                    (sexp->type-diff
                     (sexp-diff '(-> Number String)
                                '(-> Any String)))
                    is-td-leaf?:base-type-substitution
                    (match-lambda** [{_ pos} (return pos)])))
                 'neg)))

(define (merge-adapters adapters)
  (foldl* delegating-and/c adapters))

(define (foldl* f l)
  (match l
    [(cons h t)
     (foldl f h t)]
    [empty empty]))

(define-simple-macro (simple-leaf-pattern pat)
  (match-lambda** [{pat _} #t]
                  [{_ _} #f]))

(define is-td-leaf?:base-type-substitution
  (simple-leaf-pattern (or (? td:base?)
                           (td:option (? td:base?)))))
(define leaf->adapter:base-type-substitution
  (match-lambda** [{(or (td:base original new)
                        (td:option (td:base original new)))
                    _}
                   (make-base-type-adapter original new)]))

(define largest-possible-vector-size (expt 2 64))
(define round->exact (compose1 inexact->exact round))
(define realize-delta 0.00001)
(define (make-base-type-adapter original-type new-type)
  ;; Commented out: See note in type-api-mutators.rkt at `base-type-gen/restr`.
  ;;
  ;; Must handle all of the ids that that mutator mutates.
  ;; (define transfomer
  ;;   (match (difference original-type new-type)
  ;;     [(difference _ 'String)                               ~a]
  ;;     [(difference _ 'Number)                               (λ (n) (+ n 0+1i))]
  ;;     [(difference _ 'One)                                  (const 1)]

  ;;     [(difference 'Real 'Integer)                          truncate]
  ;;     [(difference 'Integer 'Natural)                       abs]
  ;;     [(difference 'Nonnegative-Integer 'Positive-Integer)  add1]
  ;;     [(difference 'Exact-Rational 'Integer)                round]
  ;;     [(difference 'Float 'Integer)                         (compose1 inexact->exact truncate)]

  ;;     [(difference 'String 'Symbol)                         string->symbol]
  ;;     [(difference 'Boolean 'Natural)                       (λ (b) (if b 1 0))]

  ;;     ;; lltodo: consider doing something better for unions
  ;;     [(difference '<U> '<mutated-U>)                       (const (sealed '?))]))
  ;; (transform/c transfomer
  ;;              original-type
  ;;              new-type)
  (sealing-adapter))

(define is-td-leaf?:function-arg/result-swap
  ;; It would be nice to check some relationship betw the two sub-tds (the
  ;; wildcards in this pattern) to make sure that this really does look like a
  ;; swap, but that's not possible unfortunately, because the td algo often does
  ;; things like this:
  ;; (-> (Listof A) (Listof B) R)
  ;; ~>
  ;; (-> (Listof B) (Listof A) R)
  ;; becomes the td:
  ;; (td:-> ((0 . (td:listof (td:base A B)))
  ;;         (1 . (td:listof (td:base B A)))))
  ;; or, for some mutations, even worse:
  ;; (td:-> ((0 . (td:unknown-type (list (td:base A B) ...)))
  ;;         (1 . (td:unknown-type (list (td:base B A) ...)))))
  ;;
  ;; So it's not clear how to check this in general.
  (simple-leaf-pattern (or (td:-> _ `(,_ ,_) #f '())
                           (td:-> _ '()      #f `(,_ ,_))
                           (td:->* _ `(,_ ,_) '())
                           (td:->* _ '() `(,_ ,_))
                           (td:->* _ `(,_) `(,_)))))
(define leaf->adapter:function-arg/result-swap
  (match-lambda** [{(or (binding (td:-> _
                                        `((,i1 . ,td1)
                                          (,i2 . ,td2))
                                        #f
                                        '())
                                 #:with [arg? #t])
                        (binding (td:-> _
                                        '()
                                        #f
                                        `((,i1 . ,td1)
                                          (,i2 . ,td2)))
                                 #:with [arg? #f]))
                    _}
                   (swap-> arg? i1 i2)]
                  [{(or (binding (td:->* n
                                        `((,i1 . ,td1)
                                          (,i2 . ,td2))
                                        '()))
                        (binding (td:->* n
                                        '()
                                        `((,i1 . ,td1)
                                          (,i2 . ,td2))))
                        (binding (td:->* n
                                        `((,i1 . ,td1))
                                        `((,i2 . ,td2)))))
                    _}
                   (swap->* n i1 i2)]))

(define is-td-leaf?:struct-field-swap
  ;; See note above about function arg/result swap sub-tds
  (simple-leaf-pattern (td:struct _ _ _ `(,_ ,_))))

(define leaf->adapter:struct-field-swap
  (match-lambda** [{(td:struct _
                               name
                               field-count
                               `((,i1 . ,td1)
                                 (,i2 . ,td2)))
                    _}
                   (swap-struct-field name field-count i1 i2)]))

(define is-td-leaf?:class-field-swap
  ;; See note above about function arg/result swap sub-tds
  (simple-leaf-pattern (or (td:class `(,_ ,_)
                                     '()
                                     _)
                           (td:class '()
                                     `(,_ ,_)
                                     _))))
(define leaf->adapter:class-field-swap
  (match-lambda**
   [{(or (td:class `((,i1 . ,td1)
                     (,i2 . ,td2))
                   '()
                   _)
         (td:class '()
                   `((,i1 . ,td1)
                     (,i2 . ,td2))
                   _))
     _}
    (swap-class-field i1 i2)]))


#;(define (vector-swap-adapter type-diff)
  (match type-diff
    [(td:vector `((,i1 . ,(td:base t1-orig t1-new))
                  (,i2 . ,(td:base t2-orig t2-new))))
     (assert (and (equal? t1-orig t2-new)
                  (equal? t2-orig t1-new))
             #:name 'vector-swap-adapter
             @~a{Mutation type is vector arg swap but diff doesn't look like a swap:
                          @t1-orig -> @t1-new
                          @t2-orig -> @t2-new})
     (swap-vector i1 i2)]
    [(td:base original new)
     (error 'vector-swap-adapter
            "Should be impossible: got a base type diff?")]))

(define lookup-td->adapter-generation-info
  (match-lambda
    [(or (== type:base-type-substitution)
         (== type:complex-type->Any))
     (list is-td-leaf?:base-type-substitution
           leaf->adapter:base-type-substitution)]
    [(or (== type:function-arg-swap)
         (== type:function-result-swap))
     (list is-td-leaf?:function-arg/result-swap
           leaf->adapter:function-arg/result-swap)]
    [(== type:struct-field-swap)
     (list is-td-leaf?:struct-field-swap
           leaf->adapter:struct-field-swap)]
    [(== type:class-field-swap)
     (list is-td-leaf?:class-field-swap
           leaf->adapter:class-field-swap)]
    [other-type
     (error
      'mutation-adapter:generate-adapter-ctc
      @~a{
          Received lookup request for unknown mutation type: @other-type
          })]))

(define-logger adaptation)

(require racket/generic)
(define-generics adapted
  (->stx adapted))

(struct adapter/c ())

(define-simple-macro (define-adapter name (field-name ...)
                       #:name name-e
                       #:->stx make-stx-fn
                       {~or* {~seq #:full-projection full-proj-e}
                             ({~literal λ} (value-name:id) proj-body ...)})
  #:with [field-accessor ...] (for/list ([sub-name (in-list (attribute field-name))])
                                (format-id sub-name "~a-~a" #'name sub-name))
  (begin
    (struct name adapter/c (field-name ...)
     #:property prop:contract
     (build-contract-property
      #:name (λ (this)
               (define field-name (field-accessor this)) ...
               name-e)
      #:late-neg-projection
      (let ([p {~? full-proj-e
                   (λ (this)
                     (define field-name (field-accessor this)) ...
                     (λ (blame)
                       (λ (value-name neg-party)
                         proj-body ...)))}])
        (catch+transform-adapter-errors
         (make-adapter-projection-unique
          (wrap-projection-transformation p
                                          inform-transient-about-adaptation!)))))
     #:methods gen:adapted
     [(define/generic generic->stx ->stx)
      (define (->stx this)
        (define field-name (field-accessor this)) ...
        (make-stx-fn generic->stx))])
    (provide name)))

(define (make-adapter-projection-unique p)
  (define adapted (mutable-seteq))
  (wrap-projection-transformation p
                                  (λ (v adapted-v)
                                    (cond [(set-member? adapted v)
                                           v]
                                          [else
                                           (set-add! adapted adapted-v)
                                           (log-adaptation-info
                                            @~a{
                                                adapted @~e[v] (@(eq-hash-code v)) @;
                                                to @~e[adapted-v] (@(eq-hash-code adapted-v))
                                                })
                                           adapted-v]))))

(define (catch+transform-adapter-errors proj)
  (define (exn->internal-error e)
    (raise-internal-experiment-error
     'mutation-adapter
     @~a{adapter raised a contract violation: @exn-message[e]}))
  (λ (this)
    (define inner1 (with-handlers ([exn:fail? exn->internal-error])
                     (proj this)))
    (λ (blame)
      (define inner2 (with-handlers ([exn:fail? exn->internal-error])
                       (inner1 blame)))
      (λ (v neg-party)
        (with-handlers ([exn:fail? exn->internal-error])
          (inner2 v neg-party))))))

(define (inform-transient-about-adaptation! v adapted-v)
  (when (transient-register-adapted-value?)
    (define transient-assert
      (dynamic-require 'typed-racket/utils/shallow-contract 'shallow-shape-check))
    (transient-assert adapted-v values '??? (quote-source-file) (cons v 'noop)))
  adapted-v)

;; late-neg-projection?
;; (v1 v2 -> (or/c v1 v2))
;; ->
;; late-neg-projection?
(define (wrap-projection-transformation proj f)
  (λ (this)
    (define inner1 (proj this))
    (λ (blame)
      (define inner2 (inner1 blame))
      (λ (v neg-party)
        (define adapted-v (inner2 v neg-party))
        (f v adapted-v)))))

(define-simple-macro (define-simple-delegating-adapter name [sub-ctc-name ...]
                       ({~literal λ} (value-name:id) proj-body ...))
  (define-adapter name [sub-ctc-name ...]
    #:name (list 'name (contract-name sub-ctc-name) ...)
    #:->stx (λ (->stx) #`(name #,(->stx sub-ctc-name) ...))
    (λ (value-name) proj-body ...)))

(define-adapter transform/c (transformer from-type to-type)
  #:name 'transform/c
  #:->stx (λ _ #`(make-base-type-adapter '#,from-type '#,to-type))
  (λ (v) (transformer v)))

(define transient-register-adapted-value? (make-parameter #t))
(struct sealed (v) #:prefab)
(define-adapter sealing-adapter ()
  #:name 'sealing-adapter
  #:->stx (λ _ #`(sealing-adapter))
  (λ (v) (sealed v)))
(define-adapter no-adapter ()
  #:name 'no-adapter
  #:->stx (λ _ #`any/c)
  (λ (v) v))

(define-simple-delegating-adapter any/c-adapter ()
  (λ (v) v))

(struct dependent-result-adapter (maker) ;; (-> list? (-> list? list?))
  #:transparent)

;; (struct-instance? -> (values (or/c #f (case-> (-> list? list?)
;;                                               (-> list? list? list? (values list? list?))))
;;                              (or/c #f (-> list? list?) dependent-result-adapter?)))
;; ->
;; (struct-instance? -> late-neg-projection?)
;;
;; `make-arg/result-adapters` returns up to two functions:
;; the first can transform the arguments, and the second can transform the results.
;; In transforming arguments, if the procedure accepts 3 args then it gets kw-args too.
;; Otherwise, it only gets positional args and kw-args are left untouched.
;;
;; The result transformer can be parameterized by the argument values, in which
;; case it should be wrapped with dependent-result-adapter.
;;
;; Either function being #f means that that step is not necessary
;; (which might make things more efficient).
(define (simple->adapter-projection make-arg/result-adapters)
  (λ (this)
    (define-values {arg-adapter result-adapter} (make-arg/result-adapters this))
    (define impersonator-wrapper
      (make-keyword-procedure
       (λ (kws kw-args . args)
         (define-values {new-args new-kw-args}
           (cond [(and (procedure? arg-adapter)
                       (procedure-arity-includes? arg-adapter 3))
                  (arg-adapter args kws kw-args)]
                 [(procedure? arg-adapter)
                  (values (arg-adapter args)
                          kw-args)]
                 [else (values args kw-args)]))
         (define impersonator-arg-results
           (if (empty? kw-args)
               new-args
               (append (list new-kw-args) new-args)))
         (cond [result-adapter
                (define adapter-fn
                  (match result-adapter
                    [(dependent-result-adapter maker)
                     (maker args)]
                    [plain plain]))
                (apply values
                       (λ results
                         (apply values (adapter-fn results)))
                       impersonator-arg-results)]
               [else (apply values impersonator-arg-results)]))))
    (λ (blame)
      (λ (v neg-party)
        (impersonate-procedure v
                               impersonator-wrapper)))))

(define (index-ctc-pairs->names index-ctc-pairs)
  (for/list ([{index ctc} (in-dict index-ctc-pairs)])
    (list index (contract-name ctc))))
(define (index-ctc-pairs->stx index-ctc-pairs ->stx [quote-index? #f])
  #`(list #,@(for/list ([{index ctc} (in-dict index-ctc-pairs)])
               #`(cons #,(if quote-index?
                             #`'#,index
                             index)
                       #,(->stx ctc)))))

(define-adapter delegating-> (specified-arg-count
                              arg-index-ctc-pairs
                              rest-arg-ctc
                              result-index-ctc-pairs)
  #:name (list 'delegating->
               specified-arg-count
               (index-ctc-pairs->names arg-index-ctc-pairs)
               (contract-name rest-arg-ctc)
               (index-ctc-pairs->names result-index-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-> #,specified-arg-count
                            #,(index-ctc-pairs->stx arg-index-ctc-pairs ->stx)
                            #,(->stx rest-arg-ctc)
                            #,(index-ctc-pairs->stx result-index-ctc-pairs ->stx)))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define specified-arg-count (delegating->-specified-arg-count this))
     (define arg-index-ctc-pairs (delegating->-arg-index-ctc-pairs this))
     (define rest-arg-ctc (delegating->-rest-arg-ctc this))
     (define result-index-ctc-pairs (delegating->-result-index-ctc-pairs this))
     (values (and (or (not (empty? arg-index-ctc-pairs))
                      (not (any/c-adapter? rest-arg-ctc)))
                  ;; lltodo (deferred): support kw-args if necessary
                  (λ (args)
                    (define-values {specified-args rest-args}
                      (split-at args specified-arg-count))
                    (append (apply-contracts-in-list specified-args arg-index-ctc-pairs)
                            (apply-contract (listof rest-arg-ctc) rest-args))))
             (and (not (empty? result-index-ctc-pairs))
                  (λ (results) (apply-contracts-in-list results result-index-ctc-pairs)))))))

(define-adapter swap-> (argument? i1 i2)
  #:name (λ (this)
            (list (if (swap->-argument? this)
                      'swap->arg
                      'swap->result)
                  (swap->-i1 this)
                  (swap->-i2 this)))
  #:->stx (λ (->stx) #`(swap-> #,argument? #,i1 #,i2))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define argument? (swap->-argument? this))
     (define i1 (swap->-i1 this))
     (define i2 (swap->-i2 this))
     ;; lltodo (deferred): support kw-args if necessary
     (define swapper (λ (args/results) (swap-in-list args/results i1 i2)))
     (if argument?
         (values swapper #f)
         (values #f swapper)))))

(define-adapter swap->* (mandatory-arg-count i1 i2)
  #:name (λ (this)
            (list 'swap->*
                  (swap->*-mandatory-arg-count this)
                  (swap->*-i1 this)
                  (swap->*-i2 this)))
  #:->stx (λ (->stx) #`(swap->* #,mandatory-arg-count '#,i1 '#,i2))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define mandatory-arg-count (swap->*-mandatory-arg-count this))
     (define i1 (swap->*-i1 this))
     (define i2 (swap->*-i2 this))
     (values (λ (args kws kw-args)
               (define-values {mandatory optional} (split-at args mandatory-arg-count))
               (define args-to-swap-supplied?
                 (for/and ([i (list i1 i2)])
                   (if (keyword? i)
                       (member i kws)
                       (> (length optional) i))))
               (cond [(not args-to-swap-supplied?)
                      (values args kw-args)]
                     [else
                      (define args+kw-args (append optional kw-args))
                      (define positions+kws (append (build-list (length optional) values)
                                                    kws))
                      (define swapped-args+kw-args
                        (swap-in-list args+kw-args
                                      (index-of positions+kws i1)
                                      (index-of positions+kws i2)))
                      (define-values {new-optional new-kws}
                        (split-at swapped-args+kw-args (length optional)))
                      (values (append mandatory new-optional)
                              new-kws)]))
             #f))))

;; This ->* support relies heavily on `delegating-and/c` to avoid duplicating
;; the work of delegating->, since they can be used together if the union of
;; both functionalities are needed.
(define-adapter delegating->* (mandatory-arg-count optional-arg-index-ctc-pairs optional-arg-kw-ctc-pairs)
  #:name (list 'delegating->*
               mandatory-arg-count
               (index-ctc-pairs->names optional-arg-index-ctc-pairs)
               (index-ctc-pairs->names optional-arg-kw-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating->* #,mandatory-arg-count
                             #,(index-ctc-pairs->stx optional-arg-index-ctc-pairs ->stx)
                             #,(index-ctc-pairs->stx optional-arg-kw-ctc-pairs ->stx #t)))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define mandatory-arg-count (delegating->*-mandatory-arg-count this))
     (define optional-arg-index-ctc-pairs (delegating->*-optional-arg-index-ctc-pairs this))
     (define optional-arg-kw-ctc-pairs (delegating->*-optional-arg-kw-ctc-pairs this))
     (values (and (not (and (empty? optional-arg-index-ctc-pairs)
                            (empty? optional-arg-kw-ctc-pairs)))
                  (λ (args kws kw-args)
                    (define-values {mandatory optional} (split-at args mandatory-arg-count))
                    (values (append mandatory
                                    (apply-contracts-in-list optional
                                                             optional-arg-index-ctc-pairs))
                            (apply-kw-contracts kws kw-args optional-arg-kw-ctc-pairs))))
             #f))))

(define (apply-contract ctc v)
  (contract ctc v #f #f))

(define (apply-kw-contracts kws kw-args kw-ctc-pairs)
  (for/list ([kw (in-list kws)]
             [kw-arg (in-list kw-args)])
    (apply-contract (dict-ref kw-ctc-pairs kw (thunk any/c)) kw-arg)))

(require (only-in rackunit require/expose))
(require/expose racket/private/class-c-old (build-class/c
                                            build-internal-class/c))
(define-adapter delegating-class/c (init-field-name-ctc-pairs
                                    field-name-ctc-pairs
                                    method-name-ctc-pairs)
  #:name (list 'delegating-class/c
               (index-ctc-pairs->names init-field-name-ctc-pairs)
               (index-ctc-pairs->names field-name-ctc-pairs)
               (index-ctc-pairs->names method-name-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-class/c #,(index-ctc-pairs->stx init-field-name-ctc-pairs ->stx #t)
                                  #,(index-ctc-pairs->stx field-name-ctc-pairs ->stx #t)
                                  #,(index-ctc-pairs->stx method-name-ctc-pairs ->stx #t)))
  (λ (c)
    (define (shift-index-map-indices map [Δ 1])
      (for/list ([{i c} (in-dict map)])
        (cons (+ i Δ) c)))
    (define (fixup->-arg-adapter-indices-for-this-argument adapter)
      (match adapter
        [(delegating-> specified-arg-count arg-map rest-arg-ctc result-map)
         (delegating-> (add1 specified-arg-count)
                       (shift-index-map-indices arg-map)
                       rest-arg-ctc
                       result-map)]
        [(delegating->* n arg-map kw-map)
         (delegating->* (add1 n) arg-map kw-map)]
        [other other]))

    ;; This magic derived from looking at
    ;; (syntax->datum (expand-once (expand-once #'(class/c (init-field [a number?]) (field [f string?]) [get-a (-> number?)]))))
    (define methods (map car method-name-ctc-pairs))
    (define method-ctcs (map (compose1 fixup->-arg-adapter-indices-for-this-argument cdr)
                             method-name-ctc-pairs))
    (define init-fields (map car init-field-name-ctc-pairs))
    (define init-field-ctcs (map cdr init-field-name-ctc-pairs))
    (define fields (map car field-name-ctc-pairs))
    (define field-ctcs (map cdr field-name-ctc-pairs))
    (apply-contract (build-class/c methods
                             method-ctcs
                             (append init-fields fields)
                             (append init-field-ctcs field-ctcs)
                             init-fields
                             init-field-ctcs
                             (list)
                             (list)
                             (build-internal-class/c (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list))
                             #f
                             #f)
                    c)))

(define-simple-delegating-adapter delegating-instanceof [sub-ctc]
  (λ (o) (apply-contract (instanceof/c sub-ctc) o)))

(define/contract (transform-struct-fields v           ;; an instance of the prefab struct...
                                          name        ;; ... with this name, and...
                                          field-count ;; ... which has this many fields of its own
                                                      ;;     (i.e. not from parents)
                                          transform   ;; a function to transform those fields
                                          )
  (->i ([v struct?]
        [name symbol?]
        [field-count natural?]
        [transform {field-count}
                   ((and/c list? (property/c length (=/c field-count)))
                    . -> .
                    (and/c list? (property/c length (=/c field-count))))])
       [result struct?])

  ;; Problem getting struct type info here, so we assume that all structs must be prefab.
  ;; Even with the assumption of prefab, and if we added mutability, this actually can't be done
  ;; with `impersonate-struct`.
  ;; The redirector proc can't know the index that the original accessor proc got.
  ;; and the accessor can't be wrapped in a function that communicates that info to the redirector,
  ;; due to the ctc of `impersonate-struct`.

  ;; fields are in order of inheritance chain (root, child, child's child, ...)
  ;; so the fields of the most-specific struct this is an instance of are last
  (define all-fields-including-parents-and-children (struct->list v))
  (define key (prefab-struct-key v))
  (define parents-field-count
    (match key
      [(? symbol?) 0]
      [(or (cons (== name)
                 parent-fields+counts)
           (list* _ ..1
                  (== name)
                  (== field-count)
                  parent-fields+counts))
       (apply + (filter integer? parent-fields+counts))]))
  (assert (>= (length all-fields-including-parents-and-children) (+ field-count
                                                                    parents-field-count))
          @~a{
              internal adapter error:
              expected at least @(+ field-count parents-field-count) fields for struct @~s[v], @;
              but struct->list only reports @(length all-fields-including-parents-and-children)
              })
  (define-values {parents-fields this+childrens-fields}
    (split-at all-fields-including-parents-and-children
              parents-field-count))
  (define-values {this-structs-own-fields childrens-fields}
    (split-at this+childrens-fields
              field-count))
  (define struct-type (prefab-key->struct-type key (length all-fields-including-parents-and-children)))
  ;; (define-values {name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?}
    ;; (struct-type-info struct-type))
  ;; so all we can do is make a new copy of the struct.
  ;; This is *wrong!* if anything uses eq?, but let's assume none of the benchmarks check struct eq?.
  ;; At least the simple ones don't.
  (apply (struct-type-make-constructor struct-type)
         (append parents-fields
                 (transform this-structs-own-fields)
                 childrens-fields)))

(define-adapter delegating-struct (maybe-parent-ctc name field-count index-ctc-pairs)
  #:name (list 'delegating-struct
               (and maybe-parent-ctc (contract-name maybe-parent-ctc))
               name
               field-count
               (index-ctc-pairs->names index-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-struct #,(and maybe-parent-ctc (->stx maybe-parent-ctc))
                                 '#,name
                                 #,field-count
                                 #,(index-ctc-pairs->stx index-ctc-pairs ->stx)))
  (λ (v)
    (transform-struct-fields (apply-contract (or maybe-parent-ctc any/c) v)
                             name
                             field-count
                             (λ (fields)
                               (apply-contracts-in-list fields
                                                        index-ctc-pairs)))))

;; lltodo: this may no longer be necessary if we use the prefab key info
(define (expand-field-count-for-child-fields a-ds offset)
  a-ds
  #;(match a-ds
    [(delegating-struct maybe-parent-adapter name field-count index-ctc-pairs)
     (define expanded-parent-adapter
       (and maybe-parent-adapter
            (expand-field-count-for-child-fields maybe-parent-adapter
                                                 offset)))
     (delegating-struct expanded-parent-adapter
                        name
                        (+ field-count offset)
                        index-ctc-pairs)]
    [(swap-struct-field name field-count i1 i2)
     (swap-struct-field name (+ field-count offset) i1 i2)]))

(define-simple-delegating-adapter delegating-and/c [left-ctc right-ctc]
  (λ (v) (apply-contract left-ctc (apply-contract right-ctc v))))
(define-simple-delegating-adapter delegating-listof [sub-ctc]
  (λ (v) (apply-contract (listof sub-ctc) v)))
(define-adapter delegating-list* (len index-ctc-pairs tail-ctc)
  #:name (list 'delegating-list*
               len
               (index-ctc-pairs->names index-ctc-pairs)
               (contract-name tail-ctc))
  #:->stx (λ (->stx)
            #`(delegating-list*
               #,len
               #,(index-ctc-pairs->stx index-ctc-pairs ->stx)
               #,(->stx tail-ctc)))
  (λ (v)
    (define-values {parts tail} (split-at v len))
    (append (apply-contracts-in-list parts index-ctc-pairs)
            (apply-contract tail-ctc tail))))

(define-simple-delegating-adapter delegating-parameter/c [sub-ctc]
  (λ (p)
    ;; This seems like the right thing, but apparently TR contracts can
    ;; impersonate parameters, and it's undocumented but
    ;; `make-derived-parameter` can't be used on an impersonator.
    #;(make-derived-parameter p
                            (λ (new-v) (apply-contract sub-ctc new-v))
                            (λ (inner-v) (apply-contract sub-ctc inner-v)))
    ;; The result still satisfies `parameter?` and can be used in `parameterize`
    (impersonate-procedure p
                           (λ args
                             (define new-args
                               (match args
                                 ['() args]
                                 [(list new-v) (list (apply-contract sub-ctc new-v))]))
                             (apply values
                                    (match-lambda [(? void? v) v]
                                                  [inner-v (apply-contract sub-ctc inner-v)])
                                    new-args)))))

(define-simple-delegating-adapter delegating-vectorof [sub-ctc]
  (λ (v) (apply-contract (vectorof sub-ctc) v)))
(define-simple-delegating-adapter delegating-boxof [sub-ctc]
  (λ (v)
    (assert (box? v) @~a{delegating box got a value that isn't a box: @~e[v]})
    (apply-contract (box/c sub-ctc) v)))
(define-simple-delegating-adapter delegating-setof [sub-ctc]
  (λ (v)
    (for/set ([el (in-set v)])
      (apply-contract sub-ctc el))))
(define-simple-delegating-adapter delegating-pairof [car-ctc cdr-ctc]
  (λ (v) (apply-contract (cons/c car-ctc cdr-ctc) v)))
(define-simple-delegating-adapter delegating-hash/c [key-ctc val-ctc]
  (λ (v)
    ;; hash/c doesn't like impersonator ctcs on keys, so let's just copy the hash
    (for/hash ([{key val} (in-hash v)])
      (values (apply-contract key-ctc key)
              (apply-contract val-ctc val)))))
(define-simple-delegating-adapter delegating-option [sub-ctc]
  (λ (v)
    (if (false? v)
        v
        (apply-contract sub-ctc v))))

(define-adapter swap-struct-field (name field-count i1 i2)
  #:name (list 'swap-struct-field name field-count i1 i2)
  #:->stx (λ _ #`(swap-struct-field '#,name #,field-count #,i1 #,i2))
  (λ (v)
    (transform-struct-fields v name field-count (λ (fields) (swap-in-list fields i1 i2)))))

#;(struct swap-vector adapter/c (i1 i2)
  #:property prop:contract
  (build-contract-property
   #:name (λ (this) (list 'vector-swap (swap-vector-i1 this) (swap-vector-i2 this)))
   #:late-neg-projection
   (λ (this)
     (define i1 (swap-vector-i1 this))
     (define i2 (swap-vector-i2 this))
     (define impersonator-procedure
       (λ _ (error)))
     (λ (blame)
       (λ (v neg-party)
         (impersonate-vector v
                             impersonator-procedure))))))

(define-adapter class-getter-for (field-id)
  #:name (list 'class-getter-for field-id)
  #:->stx (λ _ #`(class-getter-for '#,field-id))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define field-id (class-getter-for-field-id this))
     (values #f
             (dependent-result-adapter
              (match-lambda
                [(list object-instance)
                 (λ (original-results)
                   (list (dynamic-get-field field-id object-instance)))]))))))
(define-adapter class-setter-for (field-id instead-of-field-id)
  #:name (list 'class-setter-for field-id instead-of-field-id)
  #:->stx (λ _ #`(class-setter-for '#,field-id '#,instead-of-field-id))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define field-id (class-setter-for-field-id this))
     (define instead-of-field-id (class-setter-for-instead-of-field-id this))
     (values #f
             ;; This is gross. Instead of redirecting the set-field to the new
             ;; field, and letting the underlying setter do the work, we have to
             ;; let the underlying setter do the wrong thing, and then fix it up
             ;; afterwards and do the right thing instead.
             ;; What we'd really like is to replace the method that's getting
             ;; called here, but that doesn't seem possible.
             (dependent-result-adapter
              (match-lambda
                [(list object-instance v)
                 (define original-instead-of-field-value
                   (dynamic-get-field instead-of-field-id object-instance))
                 (λ (original-results)
                   ;; restore the value of the instead-of-field
                   (dynamic-set-field! instead-of-field-id
                                       object-instance
                                       original-instead-of-field-value)
                   ;; and set the one we actually want to be set
                   (dynamic-set-field! field-id object-instance v)
                   (list (void)))]))))))
(define-adapter swap-class-field (field-id1 field-id2)
  #:name (list 'swap-class-field field-id1 field-id2)
  ;; This indirection through the syntax is unfortunately necessary, because we
  ;; the field accesses are hapening through named methods - for which we only
  ;; have symbols at this level, and racket's class system doesn't offer a way
  ;; to do things like override a dynamically-computed method in a superclass.
  ;;
  ;; It would be nice to instead have all method accesses go through a generic
  ;; dynamic getter/setter (akin to `dynamic-get-field`), so that we could just
  ;; subclass dynamically and override those methods (like the commented code
  ;; below), but those generic access methods aren't typeable, so we're stuck
  ;; with this.
  #:->stx (λ _
            (with-syntax ([{get-field1
                            set-field1
                            get-field2
                            set-field2}
                           (list (format-id #'here "get-field:~a" field-id1)
                                 (format-id #'here "set-field:~a" field-id1)
                                 (format-id #'here "get-field:~a" field-id2)
                                 (format-id #'here "set-field:~a" field-id2))])
              #`(class/c
                 [get-field2 (class-getter-for '#,field-id1)]
                 [set-field2 (class-setter-for '#,field-id1 '#,field-id2)]
                 [get-field1 (class-getter-for '#,field-id2)]
                 [set-field1 (class-setter-for '#,field-id2 '#,field-id1)]))
            #;#`(swap-class-field '#,field-id1 '#,field-id2))
  (λ (v)
    ;; Wrong! These dynamic methods can't be typed!
    #;(class v
      (define/override (get-field name)
        (dynamic-get-field (match name
                             [(== field-id1) field-id2]
                             [(== field-id2) field-id1]
                             [other other])
                           this))
      (define/override (set-field! name v)
        (dynamic-set-field! (match name
                              [(== field-id1) field-id2]
                              [(== field-id2) field-id1]
                              [other other])
                            this
                            v)))
    (error 'swap-class-field
           "should never be called!")))

(define-adapter adapter-reference (name)
  #:name (list 'adapter-reference name)
  #:->stx (λ _
            (datum->syntax #f name))
  (λ (v)
    (error 'adapter-reference
           "should never be called!")))

;; Returns `vs`, but with the elements at each index in `index-ctc-pairs`
;; contracted with the corresponding contract.
(define (apply-contracts-in-list vs index-ctc-pairs)
  (for/fold ([result empty]
             [remaining-ctcs (sort index-ctc-pairs < #:key car)]
             #:result (reverse result))
            ([v (in-list vs)]
             [i (in-naturals)])
    (match remaining-ctcs
      ['() (values (cons v result)
                   empty)]
      [(cons (cons ctc-i ctc) rest)
       #:when (= ctc-i i)
       (values (cons (apply-contract ctc v) result)
               rest)]
      [else
       (values (cons v result)
               remaining-ctcs)])))

(define (swap-in-list vs index1 index2)
  (define v1 (list-ref vs index1))
  (define v2 (list-ref vs index2))
  (list-set (list-set vs index1 v2)
            index2
            v1))

(module+ test
  (test-begin
    #:name apply-contracts-in-list
    (test-equal? (apply-contracts-in-list '() '())
                 '())
    (test-equal? (apply-contracts-in-list '() `((0 . ,any/c)))
                 '())
    (test-equal? (apply-contracts-in-list '(1) `((0 . ,any/c)))
                 '(1))
    (test-match (apply-contracts-in-list '(1) `((0 . ,(sealing-adapter))))
                (list (? sealed?))))

  (define-test-syntax (test-adapter-contract
                       [name:id test-value:expr
                                #:with-contract ctc:expr]
                       e ...
                       check:expr)
    #'(let ([name (apply-contract ctc test-value)])
        e ...
        check))

  (test-begin
    #:name value-adapter
    (test-equal? (contract-name
                  (generate-adapter-ctc (mutated-interface-type 'Real
                                                                'Integer
                                                                type:base-type-substitution)))
                 #;'transform/c
                 'sealing-adapter)
    (test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Real
                                                                      'Integer
                                                                      type:base-type-substitution))]
     (sealed? v))
    ;; lltodo: re-enable these tests if we return to transformation instead of sealing
    ;; Also the tests below
    #;(test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Real
                                                                      'Integer
                                                                      type:base-type-substitution))]
     (test-equal? v 5))
    #;(test-adapter-contract
     [v 5/2
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Exact-Rational
                                                                      'Index
                                                                      type:base-type-substitution))]
     (test-equal? v (round 5/2)))
    #;(test-adapter-contract
     [v 5
        #:with-contract (generate-adapter-ctc (mutated-interface-type 'Integer
                                                                      'Real
                                                                      type:base-type-substitution))]
     (and/test (not/test (test-equal? v 5))
               (test-within v 5 realize-delta)))
    )

  (test-begin
   #:name delegating->
   (test-adapter-contract
     [f (λ (x) x)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Real Integer)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (sealed? (f 5.2)))
    #;(test-adapter-contract
     [f (λ (x) x)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Real Integer)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-equal? (f 5.2) 5))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer Integer)
                                           '(-> Real Integer)
                                           type:base-type-substitution)))
                 '(delegating-> 1
                                [[0 #;transform/c
                                    sealing-adapter]]
                                (any/c-adapter)
                                []))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer Real)
                                           '(-> Integer Integer)
                                           type:base-type-substitution)))
                 '(delegating-> 1
                                []
                                (any/c-adapter)
                                [[0 #;transform/c
                                       sealing-adapter]]))
    (test-adapter-contract
     [f (λ _ 5.2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer Real)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (sealed? (f 2)))
    #;(test-adapter-contract
     [f (λ _ 5.2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer Real)
                                                 '(-> Integer Integer)
                                                 type:base-type-substitution))]
     (test-equal? (f 2) 5))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer (values Integer Real))
                                           '(-> Integer (values Integer Integer))
                                           type:base-type-substitution)))
                 '(delegating-> 1
                                []
                                (any/c-adapter)
                                [[1 #;transform/c
                                       sealing-adapter]]))
    (test-adapter-contract
     [f (λ _ (values 2 5.2))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer (values Integer Real))
                                                 '(-> Integer (values Integer Integer))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f 0)])
       (sealed? v2)))
    #;(test-adapter-contract
     [f (λ _ (values 2 5.2))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer (values Integer Real))
                                                 '(-> Integer (values Integer Integer))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f 0)])
       (test-equal? v2 5)))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> String
                                                (-> Integer Real)
                                                (values Integer Real))
                                           '(-> String
                                                (-> Integer Integer)
                                                (values Integer Real))
                                           type:base-type-substitution)))
                 '(delegating-> 2
                                [[1 (delegating-> 1
                                                  []
                                                  (any/c-adapter)
                                                  [[0 #;transform/c
                                                      sealing-adapter]])]]
                                (any/c-adapter)
                                []))
    (test-adapter-contract
     [f (λ (s g) (values 2 (g 0)))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> String
                                                      (-> Integer Real)
                                                      (values Integer Real))
                                                 '(-> String
                                                      (-> Integer Integer)
                                                      (values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (sealed? v2)))
    (test-adapter-contract
     [f (λ (s g) (values 2 (g 0)))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> String
                                                      (-> Integer Real)
                                                      (Values Integer Real))
                                                 '(-> String
                                                      (-> Integer Integer)
                                                      (Values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (sealed? v2)))
    (test-adapter-contract
     [f (λ (s g) (values 2 g))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> String
                                                      (-> Integer Real)
                                                      (values Integer Real))
                                                 '(-> String
                                                      Any
                                                      (values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (sealed? v2)))
    #;(test-adapter-contract
     [f (λ (s g) (values 2 (g 0)))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> String
                                                      (-> Integer Real)
                                                      (values Integer Real))
                                                 '(-> String
                                                      (-> Integer Integer)
                                                      (values Integer Real))
                                                 type:base-type-substitution))]
     (let-values ([{v1 v2} (f "" (λ _ 5.2))])
       (test-equal? v2 5))))

  (test-begin
    #:name swap->
    (test-adapter-contract
     [f (λ (a b) (values a b))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer
                                                      String
                                                      (values Integer String))
                                                 '(-> String
                                                      Integer
                                                      (values Integer String))
                                                 type:function-arg-swap))]
     (let-values ([{v1 v2} (f 1 "two")])
       (and/test (test-equal? v1 "two")
                 (test-equal? v2 1))))
    (test-adapter-contract
     [f (λ (a b) (values a b))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer
                                                      String
                                                      (values Integer String))
                                                 '(-> Integer
                                                      String
                                                      (values String Integer))
                                                 type:function-result-swap))]
     (let-values ([{v1 v2} (f 1 "two")])
       (and/test (test-equal? v1 "two")
                 (test-equal? v2 1))))
    (test-adapter-contract
     [f (λ (a b) (list a))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> Integer
                                                      String
                                                      (Listof Integer))
                                                 '(-> String
                                                      Integer
                                                      (Listof Integer))
                                                 type:function-arg-swap))]
     (let ([v (f 1 "two")])
       (test-equal? v (list "two"))))
    (test-adapter-contract
     [f (λ (a b) (list a))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> (U Integer String)
                                                      (U A B)
                                                      (Listof Integer))
                                                 '(-> (U A B)
                                                      (U Integer String)
                                                      (Listof Integer))
                                                 type:function-arg-swap))]
     (let ([v (f 1 "two")])
       (test-equal? v (list "two"))))
    (test-adapter-contract
     [f (λ (a b) (list a))
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> (Listof String)
                                                      (Listof B)
                                                      (Listof Integer))
                                                 '(-> (Listof B)
                                                      (Listof String)
                                                      (Listof Integer))
                                                 type:function-arg-swap))]
     (let ([v (f 1 "two")])
       (test-equal? v (list "two"))))
    (test-adapter-contract
     [f (λ (a b) a)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(-> (Listof String)
                                                      (Setof String)
                                                      (Listof Integer))
                                                 '(-> (Setof String)
                                                      (Listof String)
                                                      (Listof Integer))
                                                 type:function-arg-swap))]
     (let ([v (f 1 "two")])
       (test-equal? v "two")))
    (test-adapter-contract
     [f (λ (a [m "m"] [b 42]) m)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(->* {Natural}
                                                       {Month
                                                        Natural}
                                                       Natural)
                                                 '(->* {Natural}
                                                       {Natural
                                                        Month}
                                                       Natural)
                                                 type:function-arg-swap))]
     (and/test (let ([v (f 1 "two" 3)])
                 (test-equal? v 3))
               (let ([v (f 1)])
                 (test-equal? v "m"))))
    (test-adapter-contract
     [f (λ (a #:m [m "m"] #:b [b 42] #:c [c 'c]) m)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(->* {Natural}
                                                       {#:m Month
                                                        #:b Natural
                                                        #:c Symbol}
                                                       Natural)
                                                 '(->* {Natural}
                                                       {#:m Natural
                                                        #:b Month
                                                        #:c Symbol}
                                                       Natural)
                                                 type:function-arg-swap))]
     (and/test (let ([v (f 1 #:b 3 #:c 'hello #:m "two")])
                 (test-equal? v 3))
               (let ([v (f 1 #:b 3 #:c 'hello)])
                 (test-equal? v "m"))
               (let ([v (f 1 #:m "two" #:c 'hello)])
                 (test-equal? v "two"))))
    (test-adapter-contract
     [f (λ (a [m "m"] #:b [b 42] #:c [c 'c]) m)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(->* {Natural}
                                                       {Month
                                                        #:b Natural
                                                        #:c Symbol}
                                                       Natural)
                                                 '(->* {Natural}
                                                       {Natural
                                                        #:b Month
                                                        #:c Symbol}
                                                       Natural)
                                                 type:function-arg-swap))]
     (and/test (let ([v (f 1 "two" #:b 3 #:c 'hello)])
                 (test-equal? v 3))
               (let ([v (f 1 #:b 3 #:c 'hello)])
                 (test-equal? v "m"))
               (let ([v (f 1 "two" #:c 'hello)])
                 (test-equal? v "two")))))

  (test-begin
   #:name delegating-struct
   (ignore (struct temp (x y z) #:prefab)
           (struct temp-child temp (a b c) #:prefab))
   (test-adapter-contract
     [t (temp 5.5 "hello" 2.3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(struct temp ([x : Real] [y : String] [z : Real]))
                                                 '(struct temp ([x : Integer] [y : String] [z : Real]))
                                                 type:base-type-substitution))]
     (and/test (temp? t)
               (sealed? (temp-x t))
               (test-equal? (temp-y t) "hello")
               (test-equal? (temp-z t) 2.3)))
   #;(test-adapter-contract
     [t (temp 5.5 "hello" 2.3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '[#:struct temp ([x : Real] [y : String] [z : Real])]
                                                 '[#:struct temp ([x : Integer] [y : String] [z : Real])]
                                                 type:base-type-substitution))]
     (and/test (temp? t)
               (test-equal? (temp-x t) (round->exact 5.5))
               (test-equal? (temp-y t) "hello")
               (test-equal? (temp-z t) 2.3)))

   ;; Child struct adaptation
   (test-adapter-contract
    [t (temp-child 5.5 "hello" 2.3 'a 'b 'c)
       #:with-contract (generate-adapter-ctc
                        (mutated-interface-type '(struct temp-child temp ([a : Real] [b : String] [c : Real]))
                                                '(struct temp-child temp ([a : Integer] [b : String] [c : Real]))
                                                type:base-type-substitution))]
    (and/test/message [(temp-child? t) "not a temp-child"]
                      [(sealed? (temp-child-a t)) "field a not sealed"]
                      [(test-equal? (temp-x t) 5.5) "other field sealed: x"]
                      [(test-equal? (temp-y t) "hello") "other field sealed: y"]
                      [(test-equal? (temp-z t) 2.3) "other field sealed: z"]
                      [(test-equal? (temp-child-b t) 'b) "other field sealed: b"]
                      [(test-equal? (temp-child-c t) 'c) "other field sealed: c"]))
   ;; Child struct adaptation for a parent's field
   (test-adapter-contract
    [t (temp-child 5.5 "hello" 2.3 'a 'b 'c)
       #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(struct temp-child
                                                    (struct temp ([x : Real] [y : String] [z : Real]))
                                                    ([a : Integer] [b : Integer] [c : Integer] ))
                                                 '(struct temp-child
                                                    (struct temp ([x : Real] [y : Integer] [z : Real]))
                                                    ([a : Integer] [b : Integer] [c : Integer] ))
                                                 type:base-type-substitution))]
    (and/test/message [(temp-child? t) "not a temp-child"]
                      [(test-equal? (temp-x t) 5.5) "other field sealed: x"]
                      [(sealed? (temp-y t)) "field y not sealed"]
                      [(test-equal? (temp-z t) 2.3) "other field sealed: z"]
                      [(test-equal? (temp-child-a t) 'a) "other field sealed: a"]
                      [(test-equal? (temp-child-b t) 'b) "other field sealed: b"]
                      [(test-equal? (temp-child-c t) 'c) "other field sealed: c"]))
   (test-adapter-contract
    [t (temp-child 5.5 "hello" 2.3 'a 'b 'c)
       #:with-contract (generate-adapter-ctc
                        (mutated-interface-type '(struct temp-child
                                                   (struct temp ([x : Real] [y : String] [z : Real]))
                                                   ([a : Integer] [b : Integer] [c : Integer] ))
                                                '(struct temp-child
                                                   (struct temp ([x : Real] [y : Real] [z : String]))
                                                   ([a : Integer] [b : Integer] [c : Integer] ))
                                                type:struct-field-swap))]
    (and/test/message [(temp-child? t) "not a temp-child"]
                      [(test-equal? (temp-x t) 5.5) "other field wrong: x"]
                      [(test-equal? (temp-y t) 2.3) "field y not swapped"]
                      [(test-equal? (temp-z t) "hello") "field z not swapped"]
                      [(test-equal? (temp-child-a t) 'a) "other field wrong: a"]
                      [(test-equal? (temp-child-b t) 'b) "other field wrong: b"]
                      [(test-equal? (temp-child-c t) 'c) "other field wrong: c"]))
   (ignore (struct exp (a) #:prefab)
           (struct app exp (fun arg)
             #:prefab)
           (struct annotated-app app (ann)
             #:prefab))
   (test-adapter-contract
    [aa (annotated-app 1 (exp 2) 3 4)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(struct annotated-app
                                                    (struct app
                                                      (struct exp ([a : Integer]) #:prefab)
                                                      ([fun : (struct exp ([a : Integer]) #:prefab)]
                                                       [arg : Integer])
                                                      #:prefab)
                                                    ([ann : Integer])
                                                    #:prefab)
                                                 '(struct annotated-app
                                                    (struct app
                                                      (struct exp ([a : Any]) #:prefab)
                                                      ([fun : (struct exp ([a : Any]) #:prefab)]
                                                       [arg : Integer])
                                                      #:prefab)
                                                    ([ann : Integer])
                                                    #:prefab)
                                                 type:base-type-substitution))]
    (and/test/message [(annotated-app? aa) "not an annotated-app"]
                      [(test-equal? (annotated-app-ann aa) 4) "field wrong: ann"]
                      [(exp? (app-fun aa)) "field wrong: fun, not an exp"]
                      [(sealed? (exp-a (app-fun aa))) "field wrong: exp-a of fun field, not sealed"]
                      [(sealed? (exp-a aa)) "field wrong: exp-a not sealed"]))

   (ignore (struct Array (shape
                          size
                          strict?
                          strict!
                          unsafe-proc)
             #:prefab)
           (struct Settable-Array Array (set-proc) #:prefab)
           (struct Mutable-Array Settable-Array (data) #:prefab))
   (test-adapter-contract
    [m-a (Mutable-Array (vector 0) 0 (box #f) void void void (vector 0))
         #:with-contract (generate-adapter-ctc
                          (mutated-interface-type
                           '(struct Mutable-Array
                              (struct Settable-Array
                                (struct Array
                                  ((shape : (Vectorof Integer))
                                   (size : Integer)
                                   (strict? : (Boxof Boolean))
                                   (strict! : (-> Void))
                                   (unsafe-proc : (-> (Vectorof Integer) Float)))
                                  #:prefab)
                                ((set-proc : (-> (Vectorof Integer) Float Void)))
                                #:prefab)
                              ((data : (Vectorof Float)))
                              #:prefab)
                           '(struct Mutable-Array
                              (struct Settable-Array
                                (struct Array
                                  ((shape : (Vectorof Integer))
                                   (size : Integer)
                                   (strict? : (Boxof Any))
                                   (strict! : (-> Void))
                                   (unsafe-proc : (-> (Vectorof Integer) Float)))
                                  #:prefab)
                                ((set-proc : (-> (Vectorof Integer) Float Void)))
                                #:prefab)
                              ((data : (Vectorof Float)))
                              #:prefab)
                           type:base-type-substitution))]
    (and/test/message [(Mutable-Array? m-a) "not a mutable array"]
                      [(box? (Array-strict? m-a)) "field wrong: Array-strict? not a box"]
                      [(sealed? (unbox (Array-strict? m-a))) "field wrong: Array-strict? contents not sealed"]))
   (test-adapter-contract
    [m-a (Mutable-Array (vector 0) 0 (box #f) void void void (vector 0))
         #:with-contract (generate-adapter-ctc
                          (mutated-interface-type
                           '(struct Array
                                  ((shape : (Vectorof Integer))
                                   (size : Integer)
                                   (strict? : (Boxof Boolean))
                                   (strict! : (-> Void))
                                   (unsafe-proc : (-> (Vectorof Integer) Float)))
                                  #:prefab)
                           '(struct Array
                                  ((shape : (Vectorof Integer))
                                   (size : Integer)
                                   (strict? : (Boxof Any))
                                   (strict! : (-> Void))
                                   (unsafe-proc : (-> (Vectorof Integer) Float)))
                                  #:prefab)
                           type:base-type-substitution))]
    (and/test/message [(Mutable-Array? m-a) "not a mutable array"]
                      [(box? (Array-strict? m-a)) "field wrong: Array-strict? not a box"]
                      [(sealed? (unbox (Array-strict? m-a))) "field wrong: Array-strict? contents not sealed"]
                      [(procedure? (Settable-Array-set-proc m-a)) "field wrong: Settable-Array-set-proc"]
                      [(vector? (Mutable-Array-data m-a)) "field wrong: Mutable-Array-data"])))
  (test-begin
   #:name swap-struct-fields
   (ignore (struct temp (x y z) #:prefab))
   (test-adapter-contract
     [t (temp 5.5 "hello" 2.3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(struct temp ([x : Integer] [y : String] [z : Real]))
                                                 '(struct temp ([y : String] [x : Integer] [z : Real]))
                                                 type:struct-field-swap))]
     (and/test (temp? t)
               (test-equal? (temp-y t) 5.5)
               (test-equal? (temp-x t) "hello")
               (test-equal? (temp-z t) 2.3))))

  (test-begin
    #:name delegating-listof
    (test-adapter-contract
     [l (list 1 2 3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(Listof Number)
                                                 '(Listof String)
                                                 type:base-type-substitution))]
     (and/test (list? l)
               (andmap sealed? l))))
  (test-begin
    #:name delegating-vectorof
    (test-adapter-contract
     [v (vector 1 2 3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(Vectorof Number)
                                                 '(Vectorof String)
                                                 type:base-type-substitution))]
     (and/test (vector? v)
               (andmap sealed? (vector->list v)))))
  (test-begin
    #:name delegating-boxof
    (test-adapter-contract
     [v (box 1)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(Boxof Number)
                                                 '(Boxof String)
                                                 type:base-type-substitution))]
     (and/test (box? v)
               (sealed? (unbox v)))))
  (test-begin
    #:name delegating-setof
    (test-adapter-contract
     [l (set 1 2 3)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(Setof Number)
                                                 '(Setof String)
                                                 type:base-type-substitution))]
     (and/test (set? l)
               (andmap sealed? (set->list l)))))
  (test-begin
    #:name delegating-pairof
    (test-adapter-contract
     [v (cons 1 2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(Pairof A B)
                                                 '(Pairof C B)
                                                 type:base-type-substitution))]
     (and/test (pair? v)
               (sealed? (car v))
               (equal? (cdr v) 2))))
  (test-begin
    #:name delegating-hash
    (test-adapter-contract
     [v (hash 1 2)
        #:with-contract (generate-adapter-ctc
                         (mutated-interface-type '(HashTable A B)
                                                 '(HashTable C B)
                                                 type:base-type-substitution))]
     (and/test (hash? v)
               (sealed? (first (hash-keys v)))
               (equal? (first (hash-values v)) 2))))
  (test-begin
    #:name delegating-parameter/c
    (let ([outer-p (make-parameter 5)])
      (test-adapter-contract
       [p outer-p
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(Parameterof Number)
                                                   '(Parameterof String)
                                                   type:base-type-substitution))]
       (and/test (parameter? p)
                 (sealed? (p))
                 (begin (p "hello")
                        (sealed? (outer-p)))))))

  (test-begin
    #:name delegating->*
    (let ([outer-f (λ (x y [z "hi"])
                     z)])
      (test-adapter-contract
       [f outer-f
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(->* (Number String) (String) String)
                                                   '(->* (Number String) (Number) String)
                                                   type:base-type-substitution))]
       (and/test (procedure? f)
                 (string? (f 1 "a"))
                 (sealed? (f 1 "a" 42)))))
    (let ([outer-f (λ (x y #:a [a 1] #:z [z "hi"] #:b [b 2])
                     z)])
      (test-adapter-contract
       [f outer-f
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(->* (Number String) (#:a Any #:z String #:b Any) String)
                                                   '(->* (Number String) (#:a Any #:z Number #:b Any) String)
                                                   type:base-type-substitution))]
       (and/test (procedure? f)
                 (string? (f 1 "a" #:b 5))
                 (sealed? (f 1 "a" #:b 5 #:z 42)))))
    (let ([outer-f (λ (x y . rst)
                     rst)])
      (test-adapter-contract
       [f outer-f
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(->* (Number String) () #:rest Integer String)
                                                   '(->* (Number String) () #:rest Any String)
                                                   type:base-type-substitution))]
       (and/test (procedure? f)
                 (list? (f 1 "a" 22))
                 (sealed? (first (f 1 "a" 22))))))
    (let ([outer-f (λ (x y [z "hi"])
                     (list y z))])
      (test-adapter-contract
       [f outer-f
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(->* (Number String) (String) String)
                                                   '(->* (Number Any) (Any) String)
                                                   type:base-type-substitution))]
       (and/test (procedure? f)
                 (list? (f 1 "a"))
                 (sealed? (first (f 1 "a" "b")))
                 (sealed? (second (f 1 "a" "b")))))))

  (test-begin
    #:name delegating-class/c
    (let ([outer-c (class object% (super-new) (init-field a) (define/public (m x y) y))])
      (test-adapter-contract
       [c outer-c
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(Class (init-field [a Number])
                                                           (m (-> Number String String)))
                                                   '(Class (init-field [a Number])
                                                           (m (-> Number Number String)))
                                                   type:base-type-substitution))]
       (and/test/message
        [(class? c) "not a class"]
        [(sealed? (send (new c [a 5]) m 5 55)) "used optional arg"])))
    (let ([outer-c (class object% (super-new) (init-field a) (define/public (m x y [z "hello"]) z))])
      (test-adapter-contract
       [c outer-c
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(Class (init-field [a Number])
                                                           (m (->* (Number String) (String) String)))
                                                   '(Class (init-field [a Number])
                                                           (m (->* (Number String) (Number) String)))
                                                   type:base-type-substitution))]
       (and/test/message
        [(class? c) "not a class"]
        [(string? (send (new c [a 5]) m 5 "hi")) "unused optional arg"]
        [(sealed? (send (new c [a 5]) m 5 "hi" 55)) "used optional arg"])))
    (let ([outer-c (class object% (super-new) (init-field a) (define/public (m x y) y))])
      (test-adapter-contract
       [c outer-c
          #:with-contract (generate-adapter-ctc
                           (mutated-interface-type '(Class (init-field [a Number])
                                                           (m (-> Number Number String)))
                                                   '(Class (init-field [a String])
                                                           (m (-> Number Number String)))
                                                   type:base-type-substitution))]
       (and/test/message
        [(class? c) "not a class"]
        [(sealed? (get-field a (new c [a 5]))) "getting field out"])))
    (test-equal? (syntax->datum
                  (->stx
                   (generate-adapter-ctc
                    (mutated-interface-type '(Class (init-field [a Number])
                                                    (m (-> Number String String)))
                                            '(Class (init-field [a Number])
                                                    (m (-> Number Number String)))
                                            type:base-type-substitution))))
                 '(delegating-class/c (list)
                                      (list)
                                      (list (cons 'm
                                                  (delegating->
                                                   2
                                                   (list (cons 1 (sealing-adapter)))
                                                   (any/c-adapter)
                                                   (list)))))))

  (define-test (test:eval-class-with-ctc/get/set-object-fields original-type
                                                               mutated-type)
    (define ns (make-base-namespace))
    (eval #'(require "mutation-adapter.rkt" rackunit racket racket/class)
          ns)
    (eval #'(require/expose "mutation-adapter.rkt" [apply-contract])
          ns)
    (eval #`(define c (apply-contract #,(->stx (generate-adapter-ctc
                                                (mutated-interface-type
                                                 original-type
                                                 mutated-type
                                                 type:class-field-swap)))
                                      (class object%
                                        (super-new)
                                        (init-field a b c)
                                        (define/public (m n s) s)
                                        (define/public (get-field:a) a)
                                        (define/public (get-field:b) b)
                                        (define/public (get-field:c) c)
                                        (define/public (set-field:a v) (set! a v))
                                        (define/public (set-field:b v) (set! b v))
                                        (define/public (set-field:c v) (set! c v)))))
          ns)
    (eval #`(define o (new c [a 5] [b "hello"] [c 42]))
          ns)
    (define before-set
      (eval #'(list (send o get-field:a)
                    (send o get-field:b)
                    (send o get-field:c))
            ns))
    (eval #`(send o set-field:b "hi") ;; this gets redirected to a set of `a`...
          ns)
    (define after-set
      (eval #'(list (send o get-field:a)
                    (send o get-field:b)
                    (send o get-field:c))
            ns))
    (and/test/message [(test-equal? before-set '("hello" 5 42))
                       "before set-field"]
                      ;; ... which appears as the value of `b` externally.
                      [(test-equal? after-set '("hello" "hi" 42))
                       "after set-field"]))
  (test-begin
    #:name swap-class-field
    (test:eval-class-with-ctc/get/set-object-fields
     '(Class (init-field [a Number]
                         [b String]
                         [c Number])
             (m (-> Number String String)))
     '(Class (init-field [a String]
                         [b Number]
                         [c Number])
             (m (-> Number String String))))
    (test:eval-class-with-ctc/get/set-object-fields
     '(Class
       #:implements FooBar
       (init-field [a Number]
                   [b String]
                   [c Number])
       (m (-> Number String String)))
     '(Class
       #:implements FooBar
       (init-field [a String]
                   [b Number]
                   [c Number])
       (m (-> Number String String))))
    (test:eval-class-with-ctc/get/set-object-fields
     '(Class
       #:implements (Class (init-field [a Number]
                                       [b String]))
       (init-field [a Number]
                   [b String]
                   [c Number])
       (m (-> Number String String)))
     '(Class
       #:implements (Class (init-field [a String]
                                       [b Number]))
       (init-field [a String]
                   [b Number]
                   [c Number])
       (m (-> Number String String))))
    (test:eval-class-with-ctc/get/set-object-fields
     '(Class (field [a Number]
                    [b String]
                    [c Number])
             (m (-> Number String String)))
     '(Class (field [a String]
                    [b Number]
                    [c Number])
             (m (-> Number String String))))
    (test:eval-class-with-ctc/get/set-object-fields
     '(Class (init-field [i1 Real]
                         [i2 String])
             (field [a Number]
                    [b String]
                    [c Number])
             (m (-> Number String String)))
     '(Class (init-field [i1 Real]
                         [i2 String])
             (field [a String]
                    [b Number]
                    [c Number])
             (m (-> Number String String)))))
  (test-begin
    #:name swap-ho-class-field
    (ignore (define ns (make-base-namespace))
            (define (eval* e) (eval e ns))
            (eval* #'(require "mutation-adapter.rkt" rackunit racket racket/class))
            (eval* #'(require/expose "mutation-adapter.rkt" [apply-contract]))
            (eval* #`(define f (apply-contract #,(->stx (generate-adapter-ctc
                                                        (mutated-interface-type
                                                         '(-> (Class (init-field [a Number]
                                                                                 [b String]
                                                                                 [c Number])
                                                                     (m (-> Number String String)))
                                                              Number)
                                                         '(-> (Class (init-field [a String]
                                                                                 [b Number]
                                                                                 [c Number])
                                                                     (m (-> Number String String)))
                                                              Number)
                                                         type:class-field-swap)))
                                              (λ (c)
                                                (define o (new c [a 5] [b "hello"] [c 42]))
                                                (send o get-field:a))))))

    (test-equal? (eval* #'(f (class object%
                               (super-new)
                               (init-field a b c)
                               (define/public (m n s) s)
                               (define/public (get-field:a) a)
                               (define/public (get-field:b) b)
                               (define/public (get-field:c) c)
                               (define/public (set-field:a v) (set! a v))
                               (define/public (set-field:b v) (set! b v))
                               (define/public (set-field:c v) (set! c v)))))
                 "hello")))

