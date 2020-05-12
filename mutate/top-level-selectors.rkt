#lang at-exp racket/base

(require racket/contract/base)
(provide (contract-out
          [top-level-selector/c    contract?]
          [select-all              top-level-selector/c]
          [select-define/contract  top-level-selector/c]
          [select-any-define       top-level-selector/c]

          [leftmost-identifier-in  (syntax? . -> . symbol?)]))

(require racket/bool
         racket/contract/region
         racket/format
         racket/function
         racket/list
         racket/match
         syntax/parse)

(define-syntax-class contracted-definition
  #:description "define/contract form"
  (pattern ((~and (~datum define/contract)
                  def/c)
            id/sig ctc body ...)))
(define-syntax-class definition
  #:description "define form"
  (pattern (def-form id/sig body ...)
           #:when (regexp-match? #rx"define"
                                 (symbol->string (syntax->datum #'def-form)))))

(define (leftmost-identifier-in stx)
  (match (flatten (list (syntax->datum stx)))
    [(list* (? symbol? s) _) s]
    [else '<no-name-found>]))

(define top-level-selector/c
  (->i ([stx syntax?])
       (values [parts-to-mutate (or/c #f (listof syntax?))]
               [mutated-id (or/c #f symbol?)]
               [reconstruct-stx (or/c #f ((listof syntax?) . -> . syntax?))])
       #:post/desc {parts-to-mutate mutated-id reconstruct-stx}
       (or (andmap false? (list parts-to-mutate
                                mutated-id
                                reconstruct-stx
                                ;; force to bool
                                #f))
           (andmap identity (list parts-to-mutate
                                  mutated-id
                                  reconstruct-stx
                                  ;; force to bool
                                  #t))
           "Either all results must be #f or all must be non-#f.")))

(define (select-all stx)
  (define name (leftmost-identifier-in stx))
  (match (syntax->list stx)
    [(? list? stx/list)
     (values stx/list
             name
             (λ (stxs/mutated)
               (datum->syntax stx stxs/mutated)))]
    [else
     (values (list stx)
             name
             (match-lambda
               [(list stx/datum/mutated)
                stx/datum/mutated]
               [a-bigger-list
                (error 'select-all
                       @~a{
                           Mutation produced multiple stxs from one stx?
                           Original: @stx
                           Mutated: @a-bigger-list
                           })]))]))
(define (select-define/contract stx)
  (syntax-parse stx
    [def:contracted-definition
      (define body-stxs (syntax->list (syntax/loc stx
                                        (def.body ...))))
      (define (reconstruct-definition body-stxs/mutated)
        (quasisyntax/loc stx
          (def.def/c def.id/sig def.ctc
            #,@body-stxs/mutated)))
      (values body-stxs
              (leftmost-identifier-in #'def.id/sig)
              reconstruct-definition)]
    [_ (values #f #f #f)]))
(define (select-any-define stx)
  (syntax-parse stx
    [def:definition
      (define body-stxs (syntax->list (syntax/loc stx
                                        (def.body ...))))
      (define (reconstruct-definition body-stxs/mutated)
        (quasisyntax/loc stx
          (def.def-form def.id/sig
            #,@body-stxs/mutated)))
      (values body-stxs
              (leftmost-identifier-in #'def.id/sig)
              reconstruct-definition)]
    [_ (values #f #f #f)]))
