#lang at-exp racket

(provide generate-adapter-ctc
         ->stx
         generate-negative-delegating-adapter-ctc
         expand-field-count-for-child-fields

         (struct-out mutated-interface-type)

         make-base-type-adapter
         swap->
         swap-struct-field
         sealing-adapter
         no-adapter

         delegating->
         delegating->*
         delegating-struct
         delegating-listof
         delegating-vectorof
         delegating-boxof
         delegating-pairof
         delegating-class/c
         delegating-parameter/c

         sexp-diff)

(require "../mutate/type-api-mutators.rkt"
         "sexp-diff.rkt"
         "util.rkt"
         syntax/parse/define
         (for-syntax syntax/parse
                     racket/syntax)
         racket/struct
         syntax/location)

(struct mutated-interface-type (original mutated mutation-type)
  #:transparent)

(define (mapping f)
  (λ (l) (map f l)))

(define-match-expander binding
  (syntax-parser
    [(_ pat {~seq #:with [name:id val:expr]} ...)
     #'(and pat
            (app (const val) name)
            ...)]))

;; type-diff
(struct td () #:transparent)
(struct td:-> td (argument-index-map result-index-map) #:transparent)
(struct td:->* td (mandatory-arg-count optional-argument-index-map optional-kw-argument-map)
  #:transparent)
(struct td:base td (original new) #:transparent)
(struct td:parametric td (vars sub-td) #:transparent)
(struct td:vector td (index-map) #:transparent)
(struct td:struct td (field-count index-map) #:transparent)
(struct td:listof td (sub-td) #:transparent)
(struct td:vectorof td (sub-td) #:transparent)
(struct td:boxof td (sub-td) #:transparent)
(struct td:parameterof td (sub-td) #:transparent)
(struct td:setof td (sub-td) #:transparent)
(struct td:option td (sub-td) #:transparent)
(struct td:pairof td (left right) #:transparent)
;; lltodo: support field types too
(struct td:class td (init-field-td-map method-td-map) #:transparent)

;; used to support diffs that we may not otherwise support, produced by arg swapping.
;; e.g. Swapping `(Listof A) (Setof A)` produces a diff
;; `((difference Listof Setof) A)`
;; Really all that matters for handling swaps tho is knowing that there is a diff between
;; two arg positions, so wrap it up in this td to notice that later.
;; If the mutation is really of a new type requiring more support, the error will still show up
;; later in `type-diff->contract`.
(struct td:unknown-type td (sub-tds) #:transparent)

(define (sexp->type-diff a-sexp-diff)
  (define (list->td-index-map a-list)
    (match a-list
      [(? td? single) `((0 . ,single))]
      [mixed-list
       (for/list ([maybe-td (in-list mixed-list)]
                  [index    (in-naturals)]
                  #:when (td? maybe-td))
         (cons index maybe-td))]))
  (define td
    (let recur ([sexp-diff-part a-sexp-diff])
      (match (normalize->-types sexp-diff-part)
        [(list* '-> (app (mapping recur)
                         (list arg-tds ...
                               (or (list* 'values result-tds)
                                   result-tds))))
         (define arg-map (list->td-index-map arg-tds))
         (define result-map (list->td-index-map (if (list? result-tds)
                                                    result-tds
                                                    (list result-tds))))
         (and (or (not (empty? arg-map)) (not (empty? result-map)))
              (td:-> arg-map result-map))]
        [(list '->*
               (app (mapping recur)
                    (list mandatory-arg-tds ...))
               (app parse-positional/kw-args
                    (list (app (mapping recur)
                               (list optional-arg-tds ...))
                          kws))
               (or (and (? list?)
                        (app (mapping recur) (list* 'values result-tds)))
                   (app recur result-tds)))
         (define mandatory-arg-map (list->td-index-map mandatory-arg-tds))
         (define optional-arg-map (list->td-index-map optional-arg-tds))
         (define optional-kw-map
           (for*/list ([{kw diff} (in-dict kws)]
                       [td (in-value (recur diff))]
                       #:when td)
             (cons kw td)))
         (define result-map (list->td-index-map (if (list? result-tds)
                                                    result-tds
                                                    (list result-tds))))
         (cond [(not (and (empty? optional-arg-map)
                          (empty? optional-kw-map)))
                ;; Relying on the assumption of a single mutation here.
                ;; Abort if it doesn't hold.
                (assert (and (empty? mandatory-arg-map)
                             (empty? result-map))
                        #:name 'sexp->type-diff
                        @~a{
                            ->* mutation support currently assumes a single mutation, @;
                            but that doesn't seem to be true: @;
                            found type diffs in both optional args and mandatory/results
                            })
                (td:->* (length mandatory-arg-tds)
                        optional-arg-map
                        optional-kw-map)]
               [(or (not (empty? mandatory-arg-map))
                    (not (empty? result-map)))
                (td:-> mandatory-arg-map result-map)]
               [else #f])]
        [(list* 'case->
                (app (mapping recur) case-tds))
         (findf values case-tds)]
        [(list 'All vars (app recur sub-td))
         (and sub-td (td:parametric vars sub-td))]
        [(list* 'Vector (and (list _ ... (? td?) _ ... #f)
                             sub-tds))
         (td:vector (list->td-index-map sub-tds))]
        [(list 'Listof (app recur sub-td))
         (and sub-td (td:listof sub-td))]
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
        [(list 'Parameterof (app recur sub-td))
         (and sub-td (td:parameterof sub-td))]
        [(list* (or 'struct 'struct:)
                (? symbol? name)
                (or          (list* (? symbol? parent) (list [list fields ': (app recur sub-tds)] ...) _)
                    (binding (list*                    (list [list fields ': (app recur sub-tds)] ...) _)
                             #:with [parent #f])))
         (td:struct (length fields) (list->td-index-map sub-tds))]
        [(list* 'Class
                (app parse-class-parts
                     (list (list (list init-field-names (app recur init-field-sub-tds))
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
         (assert (empty? field-dict)
                 "Have to handle class field mutations too...")
         (define method-dict
           (for/list ([name (in-list method-names)]
                      [sub-td (in-list method-sub-tds)]
                      #:when (td? sub-td))
             (cons name sub-td)))
         (td:class init-field-dict
                   method-dict)]
        [(difference original new)
         (td:base original new)]
        [(list* 'values (app (mapping recur)
                             result-tds))
         (and (ormap td? result-tds)
              (cons 'values result-tds))]
        [(or (? symbol?) #t #f) #f]
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

(define (parse-class-parts class-type-body)
  (match class-type-body
    [(or (list (list 'init-field init-fields ...)
               (list 'field fields ...)
               methods ...)
         (list (list 'field fields ...)
               (list 'init-field init-fields ...)
               methods ...)
         (binding (list (list 'init-field init-fields ...)
                        methods ...)
                  #:with [fields empty])
         (binding (list (list 'field fields ...)
                        methods ...)
                  #:with [init-fields empty])
         (binding (list methods ...)
                  #:with [init-fields empty]
                  #:with [fields empty]))
     (list init-fields
           fields
           methods)]))

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
                 (td:-> `((0 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A (listof C))
                                             '(-> B (listof C))))
                 (td:-> `((0 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> A Z Z Z C)
                                             '(-> B Z Z Z C)))
                 (td:-> `((0 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z A Z C)
                                             '(-> Z Z B Z C)))
                 (td:-> `((2 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> X Z A Z C)
                                             '(-> Y Z B Z C)))
                 (td:-> `((0 . ,(td:base 'X 'Y))
                          (2 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z A)
                                             '(-> Z Z Z B)))
                 (td:-> '()
                        `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values A))
                                             '(-> Z Z Z (values B))))
                 (td:-> '()
                        `((0 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A))
                                             '(-> Z Z Z (values Z B))))
                 (td:-> '()
                        `((1 . ,(td:base 'A 'B)))))
    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z A Z X))
                                             '(-> Z Z Z (values Z B Z Y))))
                 (td:-> '()
                        `((1 . ,(td:base 'A 'B))
                          (3 . ,(td:base 'X 'Y)))))
    (test-equal? (sexp->type-diff (sexp-diff '(Z Z Z -> (values Z A Z X))
                                             '(Z Z Z -> (values Z B Z Y))))
                 (td:-> '()
                        `((1 . ,(td:base 'A 'B))
                          (3 . ,(td:base 'X 'Y)))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> Z Z Z (values Z (-> A C)))
                                             '(-> Z Z Z (values Z (-> B C)))))
                 (td:-> '()
                        `((1 . ,(td:-> `((0 . ,(td:base 'A 'B)))
                                       '())))))

    (test-equal? (sexp->type-diff (sexp-diff '(-> (U A B) (U C D) C)
                                             '(-> (U C D) (U A B) C)))
                 (td:-> `((0 . ,(td:base '<U> '<mutated-U>))
                          (1 . ,(td:base '<U> '<mutated-U>)))
                        '()))

    (test-equal? (sexp->type-diff (sexp-diff '(struct
                                               stream
                                                ((first : Natural) (rest : (-> stream)))
                                                #:prefab)
                                             '(struct
                                               stream
                                                ((first : Index) (rest : (-> stream)))
                                                #:prefab)))
                 (td:struct 2 `((0 . ,(td:base 'Natural 'Index)))))
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
                 (td:struct 2 `((0 . ,(td:base 'Natural 'Index)))))
    (test-equal? (sexp->type-diff (sexp-diff '(Listof A)
                                             '(Listof B)))
                 (td:listof (td:base 'A 'B)))
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

    (test-equal? (sexp->type-diff (sexp-diff '(->* (A) (C) R)
                                             '(->* (B) (C) R)))
                 (td:-> `((0 . ,(td:base 'A 'B)))
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
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field) (sign-up (->* (A) (C) R)))
                                             '(Class (init-field) (sign-up (->* (A) (B) R)))))
                 (td:class '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (field) (sign-up (->* (A) (C) R)))
                                             '(Class (field) (sign-up (->* (A) (B) R)))))
                 (td:class '()
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]) (sign-up (->* (A) (C) R)))
                                             '(Class (init-field [a Y]) (sign-up (->* (A) (B) R)))))
                 (td:class `((a . ,(td:base 'X 'Y)))
                           `((sign-up . ,(td:->* 1
                                                 `((0 . ,(td:base 'C 'B)))
                                                 '())))))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]))
                                             '(Class (init-field [a Y]))))
                 (td:class `((a . ,(td:base 'X 'Y)))
                           '()))
    (test-equal? (sexp->type-diff (sexp-diff '(Class (init-field [a X]) (field [i Any]))
                                             '(Class (init-field [a Y]) (field [i Any]))))
                 (td:class `((a . ,(td:base 'X 'Y)))
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
                           `((run . ,(td:-> `((0 . ,(td:base 'Natural 'Integer)))
                                            '())))))

    (test-equal? (sexp->type-diff (sexp-diff '(case-> (-> A C)
                                                      (->* (A) (B) C))
                                             '(case-> (-> B C)
                                                      (->* (A) (B) C))))
                 (td:-> `((0 . ,(td:base 'A 'B)))
                        '()))
    (test-equal? (sexp->type-diff (sexp-diff '(case-> (-> A C)
                                                      (->* (A) (B) C))
                                             '(case-> (-> A C)
                                                      (->* (B) (B) C))))
                 (td:-> `((0 . ,(td:base 'A 'B)))
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
                 (td:-> `((0 . ,(td:unknown-type (list (td:base 'B 'Z)))))
                        '()))))

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
  (define full-combinator
    (match mutation-type
      [(== type:base-type-substitution)
       (base-type-substitution-adapter type-diff)]
      [(== type:function-arg-swap)
       (function-arg/result-swap-adapter type-diff)]
      [(== type:function-result-swap)
       (function-arg/result-swap-adapter type-diff)]
      [(== type:struct-field-swap)
       (struct-field-swap-adapter type-diff)]
      ;; [(== type:vector-arg-swap)
      ;;  (vector-swap-adapter type-diff)]
      ;; [type:function-arg-drop
      ;;  (function-arg-drop-adapter annotated-diff)]
      ;; [type:function-result-drop
      ;;  (function-result-drop-adapter annotated-diff)]
      ;; [type:union-branch-drop
      ;;  (union-branch-drop-adapter annotated-diff)]
      [other-type
       (error
        'mutation-adapter:generate-adapter-ctc
        @~a{
            Received unknown or unimplemented mutation type: @other-type
            For mutation:
            @~s[original]
            -->
            @~s[mutated]
            })]))
  full-combinator)

(struct recur () #:transparent)
;; td? (td? -> (or/c contract? recur?)) -> contract?
;; Converts `td` into a contract from the bottom up using `instantiator`,
;; which decides where the "bottom" of the tree is by producing the base contract,
;; or asks to continue unpacking the tree by producing `(recur)`.
;; All layers above the base contract will simply delegate down to the base.
(define (type-diff->contract td instantiator)
  (let loop ([inner-td td])
    (define (loop-over-dict-values d)
      (for/list ([{k td} (in-dict d)])
        (cons k (loop td))))

    (match* {(instantiator inner-td) inner-td}
      [{(and (not (recur)) result) _} result]
      [{(recur) (? td:base? base)}
       (error 'type-diff->contract
              @~a{given instantiator @~e[instantiator] asked to recur into td:base @~e[base]})]
      [{(recur) (td:-> arg-index-map result-index-map)}
       (delegating-> (loop-over-dict-values arg-index-map)
                     (loop-over-dict-values result-index-map))]
      [{(recur) (td:parametric vars sub-td)}
       ;; We won't mutate a type variable (unless of course it's spelled the
       ;; same as a base type... let's hope not) so we can just treat it as a
       ;; non-parametric type for the adapter's purposes.
       ;; i.e. just ignore it the parametric part!
       (loop sub-td)]
      [{(recur) (td:struct field-count index-map)}
       (delegating-struct field-count (loop-over-dict-values index-map))]
      [{(recur) (td:listof sub-td)}
       (delegating-listof (loop sub-td))]
      [{(recur) (td:vectorof sub-td)}
       (delegating-vectorof (loop sub-td))]
      [{(recur) (td:boxof sub-td)}
       (delegating-boxof (loop sub-td))]
      [{(recur) (td:setof sub-td)}
       (delegating-setof (loop sub-td))]
      [{(recur) (td:pairof left right)}
       (delegating-pairof (if left (loop left) (any/c-adapter))
                          (if right (loop right) (any/c-adapter)))]
      [{(recur) (td:parameterof sub-td)}
       (delegating-parameter/c (loop sub-td))]
      [{(recur) (td:->* n optional-arg-index-map optional-kw-arg-map)}
       (delegating->* n
                      (loop-over-dict-values optional-arg-index-map)
                      (loop-over-dict-values optional-kw-arg-map))]
      [{(recur) (td:class init-field-td-map method-td-map)}
       (delegating-class/c (loop-over-dict-values init-field-td-map)
                           (loop-over-dict-values method-td-map))]
      [{_ _}
       (error 'type-diff->contract
              @~a{
                  Unexpected instantiator request to recur from @instantiator
                  on @~s[inner-td]
                  which is part of @~s[td]
                  })])))

(define (type-sexp-not-containing-struct-subexps? s)
  (let loop ([s s]
             [top #t])
    (match s
      [(list* (or 'struct 'struct:) _ more)
       (and top
            (loop more #f))]
      [(? list? l)
       (andmap (λ (e) (loop e #f)) l)]
      [datum #t])))

(define/contract (replace-name-in-negative-types type name replacement)
  (type-sexp-not-containing-struct-subexps? ;; `struct` handling below assumes
                                            ;; that `struct` decls must be at
                                            ;; the top level of a type
   symbol?
   any/c
   . -> .
   any/c)

  (let loop ([subtype type]
             [negative? #f])
    (match subtype
      [(list '-> arg-ts ... res-t)
       (append (list '->)
               (map (λ (t) (loop t (not negative?))) arg-ts)
               (list (loop res-t negative?)))]
      [(list '->* mandatory-arg-ts optional-arg-ts res-t)
       (append (list '->*)
               (list (map (λ (t) (loop t (not negative?))) mandatory-arg-ts)
                     (map (λ (t) (loop t (not negative?))) optional-arg-ts))
               (list (loop res-t negative?)))]
      [(list* (and struct (or 'struct 'struct:)) name field-types)
       ;; Literal #t for `negative?` here bc this can only appear at the top level!
       `(,struct ,name . ,(map (λ (t) (loop t #t)) field-types))]
      [(? list? sub-exps) (map (λ (t) (loop t negative?)) sub-exps)]
      [(== name)
       #:when negative?
       replacement]
      [other other])))

(module+ test
  (test-begin
    #:name type-sexp-not-containing-struct-subexps?
    (type-sexp-not-containing-struct-subexps? '())
    (type-sexp-not-containing-struct-subexps? 'A)
    (type-sexp-not-containing-struct-subexps? '(-> A B C))
    (type-sexp-not-containing-struct-subexps? '(struct s ([x : Nat] [y : Int])))
    (not (type-sexp-not-containing-struct-subexps? '(-> (struct s ([x : Nat] [y : Int]))))))
  (test-begin
    #:name replace-name-in-negative-types
    (test-equal? (replace-name-in-negative-types '(A B C (D E (B) G) (H I J))
                                                 'B
                                                 'X)
                 '(A B C (D E (B) G) (H I J)))
    (test-equal? (replace-name-in-negative-types '(-> A B C (D E (B) G) (H I J))
                                                 'B
                                                 'X)
                 '(-> A X C (D E (X) G) (H I J)))
    (test-equal? (replace-name-in-negative-types '(A B C (-> D E (-> B) G) (H I J))
                                                 'B
                                                 'X)
                 '(A B C (-> D E (-> X) G) (H I J)))
    (test-equal? (replace-name-in-negative-types '(-> A B C (-> D E (-> B R) G) (H I J))
                                                 'B
                                                 'X)
                 '(-> A X C (-> D E (-> X R) G) (H I J)))
    (test-equal? (replace-name-in-negative-types '(-> A B C (-> D E (-> B B) G) (H I J))
                                                 'B
                                                 'X)
                 '(-> A X C (-> D E (-> X B) G) (H I J)))
    (test-equal? (replace-name-in-negative-types '(struct Date ((ymd : YMD) (jdn : Integer)) #:prefab)
                                                 'YMD
                                                 '???)
                 '(struct Date ((ymd : ???) (jdn : Integer)) #:prefab))))

;; sexp? symbol? contract? -> contract?
;; Generate a contract that delegates on `name` in *negative positions only* to `ctc`.
(define (generate-negative-delegating-adapter-ctc type name ctc)
  (define td (sexp->type-diff (sexp-diff type (replace-name-in-negative-types type name (gensym name)))))
  (type-diff->contract td
                       (match-lambda [(? td:base?) ctc]
                                     [else (recur)])))

(define (base-type-substitution-adapter type-diff)
  (type-diff->contract type-diff
                       (match-lambda [(or (td:base original new)
                                          (td:option (td:base original new)))
                                      (make-base-type-adapter original new)]
                                     [else (recur)])))

(define largest-possible-vector-size (expt 2 64))
(define round->exact (compose1 inexact->exact round))
(define realize-delta 0.00001)
(define (make-base-type-adapter original-type new-type)
  ;; see notes: sealing makes more consistent sense for base types
  #;(let ([transform/c (λ (f) (transform/c f original-type new-type))])
    (match (difference original-type new-type)
      [(difference 'Number 'Real) (transform/c real-part)]
      [(difference 'Real 'Number) (transform/c (λ (x) (+ x 0+1i)))]
      [(difference 'Real 'Integer) (transform/c round->exact)]
      [(difference 'Integer 'Real) (transform/c (λ (x) (+ x realize-delta)))]
      [(or (difference 'Integer 'Natural)
           (difference 'Integer 'Index)) (transform/c abs)]
      [(difference 'Exact-Rational 'Index)
       (transform/c (compose1 abs round->exact))]
      [(or (difference 'Natural 'Integer)
           (difference 'Index 'Integer)) (transform/c -)]
      [(or (difference 'Index 'Natural)
           (difference 'Index 'Exact-Rational)) (transform/c (const (add1 largest-possible-vector-size)))]
      [(difference 'Natural 'Index) (transform/c (λ (x)
                                                   (if (< x largest-possible-vector-size)
                                                       x
                                                       0)))]

      [(difference 'Integer 'String) (transform/c ~a)]))
  (sealing-adapter))

(define (function-arg/result-swap-adapter type-diff)
  (type-diff->contract
   type-diff
   ;; It would be nice to check some relationship betw `td1` and `td2`, to make
   ;; sure that this really does look like a swap, but that's not possible
   ;; unfortunately, because the td algo often does things like this:
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
   (match-lambda [(or (binding (td:-> `((,i1 . ,td1)
                                        (,i2 . ,td2))
                                      '())
                               #:with [arg? #t])
                      (binding (td:-> '()
                                      `((,i1 . ,td1)
                                        (,i2 . ,td2)))
                               #:with [arg? #f]))
                  (swap-> arg? i1 i2)]
                 [(td:base original new)
                  (error 'function-arg-swap-adapter
                         @~a{
                             Should be impossible: got a base type diff? From recurring into: @;
                             @~s[type-diff]
                             })]
                 [else (recur)])))

(define (struct-field-swap-adapter type-diff)
  (match type-diff
    [(td:struct field-count
                `((,i1 . ,td1)
                  (,i2 . ,td2))) ;; See note above about function arg/result swap sub-tds
     (swap-struct-field field-count i1 i2)]
    [other
     (error 'struct-field-swap-adapter
            @~a{Should be impossible: got a td that isn't td:struct?: @~e[other]})]))


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
  (struct name adapter/c (field-name ...)
    #:property prop:contract
    (build-contract-property
     #:name (λ (this)
              (define field-name (field-accessor this)) ...
              name-e)
     #:late-neg-projection
     {~? full-proj-e
         (λ (this)
           (define field-name (field-accessor this)) ...
           (λ (blame)
             (λ (value-name neg-party)
               proj-body ...)))})
    #:methods gen:adapted
    [(define/generic generic->stx ->stx)
     (define (->stx this)
       (define field-name (field-accessor this)) ...
       (make-stx-fn generic->stx))]))

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
  (λ (v)
    (define sealed-v (sealed v))
    ;; an alternative option for cooperating with transient blame tracking;
    ;; deferred for now in favor of using a prefab `sealed` struct
    (when (transient-register-adapted-value?)
      (define transient-assert
        (dynamic-require 'typed-racket/utils/shallow-contract 'shallow-shape-check))
      (transient-assert sealed-v values '??? (quote-source-file) (cons v 'noop)))
    sealed-v))
(define-adapter no-adapter ()
  #:name 'no-adapter
  #:->stx (λ _ #`any/c)
  (λ (v) v))

(define-simple-delegating-adapter any/c-adapter ()
  (λ (v) v))

;; (struct-instance? -> (values (or/c #f (case-> (-> list? list?)
;;                                               (-> list? list? list? (values list? list?))))
;;                              (or/c #f (-> list? list?))))
;; ->
;; (struct-instance? -> late-neg-projection?)
;;
;; `make-arg/result-adapters` returns up to two functions:
;; the first can transform the arguments, and the second can transform the results.
;; In transforming arguments, if the procedure accepts 3 args then it gets kw-args too.
;; Otherwise, it only gets positional args and kw-args are left untouched.
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
         (if (procedure? result-adapter)
             (apply values
                    (λ results
                      (apply values (result-adapter results)))
                    impersonator-arg-results)
             (apply values impersonator-arg-results)))))
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

(define-adapter delegating-> (arg-index-ctc-pairs result-index-ctc-pairs)
  #:name (list 'delegating->
               (index-ctc-pairs->names arg-index-ctc-pairs)
               (index-ctc-pairs->names result-index-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-> #,(index-ctc-pairs->stx arg-index-ctc-pairs ->stx)
                            #,(index-ctc-pairs->stx result-index-ctc-pairs ->stx)))
  #:full-projection
  (simple->adapter-projection
   (λ (this)
     (define arg-index-ctc-pairs (delegating->-arg-index-ctc-pairs this))
     (define result-index-ctc-pairs (delegating->-result-index-ctc-pairs this))
     (values (and (not (empty? arg-index-ctc-pairs))
                  ;; lltodo (deferred): support kw-args if necessary
                  (λ (args) (apply-contracts-in-list args arg-index-ctc-pairs)))
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

;; This ->* support relies heavily on the 'only one mutation' assumption to
;; avoid duplicating the work of delegating->
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
(define-adapter delegating-class/c (init-field-index-ctc-pairs method-index-ctc-pairs)
  #:name (list 'delegating-class/c
               (index-ctc-pairs->names init-field-index-ctc-pairs)
               (index-ctc-pairs->names method-index-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-class/c #,(index-ctc-pairs->stx init-field-index-ctc-pairs ->stx #t)
                                  #,(index-ctc-pairs->stx method-index-ctc-pairs ->stx #t)))
  (λ (c)
    (define (shift-index-map-indices map [Δ 1])
      (for/list ([{i c} (in-dict map)])
        (cons (+ i Δ) c)))
    (define (fixup->-arg-adapter-indices-for-this-argument adapter)
      (match adapter
        [(delegating-> arg-map result-map)
         (delegating-> (shift-index-map-indices arg-map)
                       result-map)]
        [(delegating->* n arg-map kw-map)
         (delegating->* (add1 n) arg-map kw-map)]
        [other other]))

    ;; This magic derived from looking at
    ;; (syntax->datum (expand-once (expand-once #'(class/c (init-field [a number?]) [get-a (-> number?)]))))
    (define methods (map car method-index-ctc-pairs))
    (define method-ctcs (map (compose1 fixup->-arg-adapter-indices-for-this-argument cdr)
                             method-index-ctc-pairs))
    (define init-fields (map car init-field-index-ctc-pairs))
    (define init-field-ctcs (map cdr init-field-index-ctc-pairs))
    (apply-contract (build-class/c methods
                             method-ctcs
                             init-fields
                             init-field-ctcs
                             init-fields
                             init-field-ctcs
                             (list)
                             (list)
                             (build-internal-class/c (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list) (list))
                             #f
                             #f)
                    c)))

(define/contract (transform-struct-fields v           ;; an instance of a struct...
                                          field-count ;; ... which has this many fields of its own
                                                      ;;     (i.e. not from parents)
                                          transform   ;; a function to transform those fields
                                          )
  (->i ([v struct?]
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
  (define all-fields-including-parents (struct->list v))
  (assert (>= (length all-fields-including-parents) field-count)
          @~a{
              internal adapter error:
              expected at least @field-count fields for struct @~s[v], @;
              but struct->list only reports @(length all-fields-including-parents)
              })
  (define-values {parents-fields this-structs-own-fields}
    (split-at all-fields-including-parents
              (- (length all-fields-including-parents) field-count)))
  (define key (prefab-struct-key v))
  (define struct-type (prefab-key->struct-type key (length all-fields-including-parents)))
  ;; (define-values {name init-field-cnt auto-field-cnt accessor-proc mutator-proc immutable-k-list super-type skipped?}
    ;; (struct-type-info struct-type))
  ;; so all we can do is make a new copy of the struct.
  ;; This is *wrong!* if anything uses eq?, but let's assume none of the benchmarks check struct eq?.
  ;; At least the simple ones don't.
  (apply (struct-type-make-constructor struct-type)
         (append parents-fields
                 (transform this-structs-own-fields))))

(define-adapter delegating-struct (field-count index-ctc-pairs)
  #:name (list 'delegating-struct
               field-count
               (index-ctc-pairs->names index-ctc-pairs))
  #:->stx (λ (->stx)
            #`(delegating-struct #,field-count #,(index-ctc-pairs->stx index-ctc-pairs ->stx)))
  (λ (v)
    (transform-struct-fields v
                             field-count
                             (λ (fields)
                               (apply-contracts-in-list fields
                                                        index-ctc-pairs)))))

(define (expand-field-count-for-child-fields a-ds offset)
  (match a-ds
    [(delegating-struct field-count index-ctc-pairs)
     (delegating-struct (+ field-count offset) index-ctc-pairs)]
    [(swap-struct-field field-count i1 i2)
     (swap-struct-field (+ field-count offset) i1 i2)]))

(define-simple-delegating-adapter delegating-listof [sub-ctc]
  (λ (v) (apply-contract (listof sub-ctc) v)))

(define-simple-delegating-adapter delegating-parameter/c [sub-ctc]
  (λ (p)
    (make-derived-parameter p
                            (λ (new-v) (apply-contract sub-ctc new-v))
                            (λ (inner-v) (apply-contract sub-ctc inner-v)))))

(define-simple-delegating-adapter delegating-vectorof [sub-ctc]
  (λ (v) (apply-contract (vectorof sub-ctc) v)))
(define-simple-delegating-adapter delegating-boxof [sub-ctc]
  (λ (v) (apply-contract (box/c sub-ctc) v)))
(define-simple-delegating-adapter delegating-setof [sub-ctc]
  (λ (v)
    (for/set ([el (in-set v)])
      (apply-contract sub-ctc v))))
(define-simple-delegating-adapter delegating-pairof [car-ctc cdr-ctc]
  (λ (v) (apply-contract (cons/c car-ctc cdr-ctc) v)))

(define-adapter swap-struct-field (field-count i1 i2)
  #:name (list 'delegating-struct i1 i2)
  #:->stx (λ _ #`(swap-struct-field #,field-count #,i1 #,i2))
  (λ (v)
    (transform-struct-fields v field-count (λ (fields) (swap-in-list fields i1 i2)))))

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
                 '(delegating-> [[0 #;transform/c
                                    sealing-adapter]] []))
    (test-equal? (contract-name
                  (generate-adapter-ctc
                   (mutated-interface-type '(-> Integer Real)
                                           '(-> Integer Integer)
                                           type:base-type-substitution)))
                 '(delegating-> [] [[0 #;transform/c
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
                 '(delegating-> [] [[1 #;transform/c
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
                 '(delegating-> [[1 (delegating-> [] [[0 #;transform/c
                                                         sealing-adapter]])]]
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
       (test-equal? v "two"))))

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
       #:with-contract (expand-field-count-for-child-fields
                        (generate-adapter-ctc
                         (mutated-interface-type '(struct temp ([x : Real] [y : String] [z : Real]))
                                                 '(struct temp ([x : Real] [y : Integer] [z : Real]))
                                                 type:base-type-substitution))
                        3)]
    (and/test/message [(temp-child? t) "not a temp-child"]
                      [(test-equal? (temp-x t) 5.5) "other field sealed: x"]
                      [(sealed? (temp-y t)) "field y not sealed"]
                      [(test-equal? (temp-z t) 2.3) "other field sealed: z"]
                      [(test-equal? (temp-child-a t) 'a) "other field sealed: a"]
                      [(test-equal? (temp-child-b t) 'b) "other field sealed: b"]
                      [(test-equal? (temp-child-c t) 'c) "other field sealed: c"]))
   (test-adapter-contract
    [t (temp-child 5.5 "hello" 2.3 'a 'b 'c)
       #:with-contract (expand-field-count-for-child-fields
                        (generate-adapter-ctc
                         (mutated-interface-type '(struct temp ([x : Real] [y : String] [z : Real]))
                                                 '(struct temp ([x : Real] [y : Real] [z : String]))
                                                 type:struct-field-swap))
                        3)]
    (and/test/message [(temp-child? t) "not a temp-child"]
                      [(test-equal? (temp-x t) 5.5) "other field wrong: x"]
                      [(test-equal? (temp-y t) 2.3) "field y not swapped"]
                      [(test-equal? (temp-z t) "hello") "field z not swapped"]
                      [(test-equal? (temp-child-a t) 'a) "other field wrong: a"]
                      [(test-equal? (temp-child-b t) 'b) "other field wrong: b"]
                      [(test-equal? (temp-child-c t) 'c) "other field wrong: c"])))
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
                 (sealed? (f 1 "a" #:b 5 #:z 42))))))

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
                                      (list (cons 'm
                                                  (delegating->
                                                   (list (cons 1 (sealing-adapter)))
                                                   (list))))))))

