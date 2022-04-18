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