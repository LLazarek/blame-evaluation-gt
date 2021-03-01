#lang typed/racket

#;
(module server-typed typed/racket

  (provide neg-abs)

  (: neg-abs (Real -> Nonnegative-Real))
  (define (neg-abs x)
    (if (< x 0)
        (- x)  ;; << mistake 
        (- x))))

(module server racket

  (provide neg-abs)

  (define (neg-abs x)
    (if (< x 0)
        (- x)  ;; << mutation fro '- to '+
        (- x))))

(module layer-typed typed/racket

  (provide na-client)

  (define-type NPR Nonpositive-Real)

  (require/typed
   (submod ".." server)
   [neg-abs (-> Real NPR)])

  (: na-client (-> Real NPR))
  (define (na-client x)
    (* 4 (neg-abs x))))

(module layer racket

  (provide na-client)

  (require (submod ".." server))

  (define (na-client x)
    (* 4 (neg-abs x))))

(module main typed/racket

  (define-type NPR Nonpositive-Real)

  (require/typed
   (submod ".." layer-typed)
   [na-client (-> Real NPR)])

  (define x (na-client -10))
  (displayln x))

(require 'main)


