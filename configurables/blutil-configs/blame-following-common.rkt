#lang racket

(provide take1-or-empty

         select-top-of-errortrace/context-fallback
         select-all-errortrace/context-fallback
         select-top-of-context
         select-all-context

         select-top-of-context/filter-max

         select-all-blamed
         select-last-blamed-pair
         select-first-blamed-pair)

(define/match (take1-or-empty l #:right? [right? #f])
  [{(list* x _)    #f} (list x)]
  [{(list _ ... x) #t} (list x)]
  [{_ _}               empty])

(define (take-up-to-N l n #:right? [right? #f])
  (define taker (if right? take-right take))
  (taker l (min (length l) n)))


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

;; For transient blame, which always points to pairs of modules (the boundary).
;; Since there is no guarantee about which side is the untyped one, both sides
;; are returned for every boundary; the whole boundary list is flattened into an
;; even length list. We just take both sides when we pick a boundary, so these
;; functions pick the first and last pairs.
(define (select-last-blamed-pair config blamed errortrace context)
  (take-up-to-N blamed 2 #:right? #t))

(define (select-first-blamed-pair config blamed errortrace context)
  (take-up-to-N blamed 2 #:right? #f))


(define (filter-max stack config)
  (filter-not (λ (mod) (equal? (hash-ref config mod 'none) 'max))
              stack))

(define (select-top-of-context/filter-max config blamed errortrace context)
  (take1-or-empty (filter-max context config)))

