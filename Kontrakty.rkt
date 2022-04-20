#lang racket
;zad 3
;Napisz procedurę suffixes, zwracającą wszystkie sufiksy listy podanej jako argument.
;Napisz dla tej procedury odpowiedni kontrakt parametryczny.
(define (suffixes x)
  (cond [(empty? x)
         empty]
        [#t
         (cons (cdr x) (suffixes (cdr x) ) )]
        )
  )

;Zadanie 4. (2 pkt)
;Poniższa procedura ma za zadanie obliczyć listę wszystkich podlist listy podanej jako
;argument:
( define/contract ( sublists xs)
   (-> list list)
   (if ( null? xs)
       ( list null )
       (if (empty? (cdr xs)) xs (cons xs
         ( sublists (cdr xs))))))
;Niestety, procedura ta zawiera błąd:
;> ( sublists '(1 2))
;'((1 2) 2)
;> ( sublists '(1 2 3))
;'((1 2 3) 2 3 (1 . 3) . 3)
;Napisz kontrakt parametryczny dla tej procedury, który odrzuci błędne wyniki. Popraw
;procedurę, aby działała zgodnie z założeniem oraz spełniała swój kontrakt.

;Zadanie 5.
;Wskaż w poniższych kontraktach wystąpienia pozytywne i negatywne. Zaimplementuj
;procedury spełniające te kontrakty.
(define/contract (pom1 a)
  (-> number? string?)
  "42"
  )
(define/contract (pom2 a)
  (-> string? boolean?)
  #t
  )
(define/contract (5a a b)
( parametric->/c [a b] (-> a b a))
  a
  )
(define/contract (5b a b c)
( parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
  (a (b c) )
  )
(define/contract (5c a b )
( parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
  (a b))
(define/contract (5d a b)
( parametric->/c [a] (-> (-> (-> a a) a) a))
  b
  )