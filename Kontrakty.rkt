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
( define/contract ( sublistsa xs)
   (parametric->/c [a](-> (listof a) (listof a) ) )
   (if ( null? xs)
       ( list null )
       (if (empty? (cdr xs)) xs (cons xs
         ( sublists (cdr xs))))))

( define ( sublists xs )
   (parametric->/c [a](-> (listof a) (listof a) ) )
   ( if ( null? xs )
        ( list null )
        ( append-map
          ( lambda ( ys ) ( list ( cons ( car xs ) ys ) ys ) )
          ( sublists ( cdr xs ) ) ) ) )
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
( parametric->/c [a b] (-> a b a)) ;oba negatywne
  a
  )
(define/contract (5b a b c)
( parametric->/c [a b c] (-> (-> a b c) (-> a b) a c)) ;a - negatywne, b - negatywne, c - negatywne
  (a (b c) )
  )
(define/contract (5c f g )
( parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c))); a - 3 razy negatywne,b 3 razy negatywne, c negatywne
  (lambda (x) (f (g x)))
  )
(define/contract (5d f)
( parametric->/c [a] (-> (-> (-> a a) a) a));a - 6
  (lambda (x) (x (x x)))
  )




;kod z wykładu
;========================================================
(require rackunit)

;(define (fact n)
;  (if (natural? n)
;      (if (= n 0)
;          1
;          (* n (fact (- n 1))))
;      (error 'fact "Niepoprawne dane wejściowe")))

;(define (fact n)
;  (check-pred natural? n)
;  (if (= n 0)
;      1
;      (* n (fact (- n 1)))))

(define positive-natural/c
  (and/c natural? positive?))

(define/contract (fact n)
  (-> natural? positive-natural/c)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define/contract (filter p? xs)
  (parametric->/c [a] (-> (-> a boolean?) (listof a) (listof a)))
  (match xs
    ['()   null]
    [(cons x xs)
       (if (p? x)
           (cons x (filter p? xs))
           (filter p? xs))]))

(define/contract (map f xs)
  (parametric->/c [a b] (-> (-> a b) (listof a) (listof b)))
  (match xs
    ['() null]
    [(cons x xs) (cons (f x) (map f xs))]))

(define/contract (bad-id x)
  (parametric->/c [a] (-> a any/c))
  x)

; ==================================================================

(struct leaf () #:transparent)
(struct node (l elem r) #:transparent)

(define (tree? t)
  (or (leaf? t)
      (and (node? t) (tree? (node-l t)) (tree? (node-r t)))))

(define (bad-treeof c)
  (or/c (struct/c leaf)
        (struct/c node (bad-treeof c) c (bad-treeof c))))

(define (treeof c)
  (flat-rec-contract tree
                     (struct/c leaf)
                     (struct/c node tree c tree)))

(define/contract (insert x t)
  (-> number? (treeof number?) (treeof number?))
  (match t
    [(leaf)       (node (leaf) x (leaf))]
    [(node l y r)
       (if (< x y)
           (node (insert x l) y r)
           (node l y (insert x r)))]))