#lang racket

(provide take1-or-empty

         select-top-of-errortrace/context-fallback
         select-all-errortrace/context-fallback
         select-top-of-context
         select-all-context

         select-top-of-errortrace/context-fallback/filter-typed
         select-top-of-context/filter-typed

         select-all-blamed
         select-last-blamed
         select-first-blamed)

(define/match (take1-or-empty l #:right? [right? #f])
  [{(list* x _)    #f} (list x)]
  [{(list _ ... x) #t} (list x)]
  [{_ _}               empty])


(define (select-top-of-errortrace/context-fallback config blamed errortrace context)
  (match* {(take1-or-empty errortrace) (take1-or-empty context)}
    [{(? cons? top) _            } top]
    [{'()           (? cons? top)} top]
    [{'()           '()          } empty]))

(define (select-all-errortrace/context-fallback config blamed errortrace context)
  (if (empty? errortrace)
      context
      errortrace))

(define (select-top-of-context config blamed errortrace context)
  (take1-or-empty context))

(define (select-all-context config blamed errortrace context)
  context)


(define (select-all-blamed config blamed errortrace context)
  blamed)

(define (select-last-blamed config blamed errortrace context)
  (take1-or-empty blamed #:right? #t))

(define (select-first-blamed config blamed errortrace context)
  (take1-or-empty blamed #:right? #f))


(define (filter-typed stack config)
  (filter (λ (mod) (equal? (hash-ref config mod 'none) 'none))
          stack))
(define (select-top-of-errortrace/context-fallback/filter-typed config blamed errortrace context)
  (match* {(take1-or-empty (filter-typed errortrace config))
           (take1-or-empty (filter-typed context    config))}
    [{(? cons? top) _            } top]
    [{'()           (? cons? top)} top]
    [{'()           '()          } empty]))

(define (select-top-of-context/filter-typed config blamed errortrace context)
  (take1-or-empty (filter-typed context config)))

