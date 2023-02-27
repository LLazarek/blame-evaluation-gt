#lang racket

(provide take1-or-empty

         select-top-of-errortrace/context-fallback
         select-all-errortrace/context-fallback
         select-top-of-context
         select-all-context

         select-top-of-errortrace/context-fallback/filter-typed
         select-top-of-context/filter-typed

         select-all-blamed
         select-last-blamed-pair
         select-first-blamed-pair

         select-all-blamed/filter-library
         select-top-of-context/filter-typed+library
         select-last-blamed-pair/filter-library
         select-first-blamed-pair/filter-library)

(require "../../util/experiment-exns.rkt")

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

;; lltodo: maybe this is really the shape that Natural blame should have, to be
;; consistent with Transient...
#;(define (select-blame-positive config blamed errortrace context)
  (match blamed
    [(list pos neg)
     pos]
    [else
     (raise-internal-experiment-error
      'blame-following-common:select-blame-positive
      "TR blame has unexpected shape (not (list pos neg)): ~a"
      blamed)]))

;; For transient blame, which always points to pairs of modules (the boundary).
;; Since there is no guarantee about which side is the untyped one, both sides
;; are returned for every boundary; the whole boundary list is flattened into an
;; even length list. We just take both sides when we pick a boundary, so these
;; functions pick the first and last pairs.
(define (select-last-blamed-pair config blamed errortrace context)
  (take-up-to-N blamed 2 #:right? #t))

(define (select-first-blamed-pair config blamed errortrace context)
  (take-up-to-N blamed 2 #:right? #f))


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


(define (config->benchmark-id config)
  (equal-hash-code (list->set (hash-keys config))))
(define client-mods-by-benchmark-id
  (hash #;"acquire" 548409849495921129
        '(main.rkt player.rkt strategy.rkt)

        #;"gregor" 331490195211507796
        '(main.rkt)

        #;"kcfa" 548407543311834055
        '(main.rkt ui.rkt)

        #;"quadT" #;"quadU" 131120763051543598
        '(main.rkt quad-main.rkt)

        #;"snake" 553760278578814055
        '(main.rkt handlers.rkt)

        #;"synth" 8104232757965284
        '(main.rkt sequencer.rkt mixer.rkt synth.rkt drum.rkt)

        #;"take5" 551790292889251081
        '(main.rkt player.rkt)

        #;"tetris" 551127599752439499
        '(main.rkt world.rkt)

        #;"suffixtree" 548402567047909558
        '(main.rkt lcs.rkt)

        #;"sieve" 548402560262049974
        '(main.rkt)))
(define (config->library-side-predicate config)
  (define client-mods (hash-ref client-mods-by-benchmark-id
                                (config->benchmark-id config)
                                (λ _
                                  (raise-internal-experiment-error
                                   'blame-following-common:client-side-filtering
                                   "Found unkown config: ~a"
                                   config))))
  (define library-mods (remove* client-mods
                                (hash-keys config)))
  (λ (mod) (member mod library-mods)))
(define (filter-out-library-side candidate-mods config)
  (filter-not (config->library-side-predicate config)
              candidate-mods))
(define (filter-out-library-side/boundary-pairs blamed-pairs config)
  (define boundary-pairs (match blamed-pairs
                           [(list* one more)
                            (for/list ([l (in-list blamed-pairs)]
                                       [r (in-list more)]
                                       [i (in-naturals)]
                                       #:when (even? i))
                              (list l r))]
                           [else empty]))
  (define library-side? (config->library-side-predicate config))
  (append*
   (for/list ([pair (in-list boundary-pairs)])
     (match pair
       [(list (? library-side?)
              (? library-side?))
        '()]
       [(list (? (negate library-side?))
              (? (negate library-side?)))
        pair]
       [(list-no-order (? library-side?)
                       (? (negate library-side?) interface-or-something-else-special))
        ;; Boundaries involving the interface stick around, and just point to the interface
        (list interface-or-something-else-special
              interface-or-something-else-special)]))))

(define (select-all-blamed/filter-library config blamed errortrace context)
  (filter-out-library-side blamed config))
(define (select-top-of-context/filter-typed+library config blamed errortrace context)
  (select-top-of-context/filter-typed config
                                      blamed
                                      errortrace
                                      (filter-out-library-side context config)))
(define (select-last-blamed-pair/filter-library config blamed errortrace context)
  (take-up-to-N (filter-out-library-side/boundary-pairs blamed) 2 #:right? #t))
(define (select-first-blamed-pair/filter-library config blamed errortrace context)
  (take-up-to-N (filter-out-library-side/boundary-pairs blamed) 2 #:right? #f))
