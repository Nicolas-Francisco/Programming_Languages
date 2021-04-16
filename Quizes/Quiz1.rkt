#lang play
(deftype Card
  [club n]
  [diamond n]
  [heart n]
  [spade n])

; suit :: Card -> (Card -> bool)
(define (suit card)
   (λ (card2)
      (match card2
         [(club _) (club? card)]
         [(diamond _) (diamond? card)]
         [(heart _) (heart? card)]
         [(spade _) (spade? card)])))

(define (color1? lst)
  (def fc (car lst))
  (def ml (map (suit fc) lst))
  (foldl (λ(x y) (and x y)) #t ml))

(define (color2? lst)
  (filter (suit (car lst)) lst))

(define (color3? lst)
  (foldl (λ (x y) ((suit x) y)) (#t) (lst)))

(define (color4? lst)
  (def fc (car lst))
  (equal?
   (length (filter (suit fc) lst))
   (length lst)))

(define ex1 (list (club 1) (club 12) (club 3) (club 4)))

(define ex2 (list (club 1) (club 12) (club 3) (spade 4)))