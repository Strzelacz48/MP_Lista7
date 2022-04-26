#lang plait
(define (my-map f xs)
  (type-case (Listof 'a) xs
    [(cons x xs) (cons (f x) (my-map f xs))]
    [empty       empty]))

(define-type (Tree 'a)
  (leaf)
  (node [l : (Tree 'a)] (elem : 'a) [r : (Tree 'a)]))

(define (insert x t)
  (type-case (Tree 'a) t
    [(leaf)  (node (leaf) x (leaf))]
    [(node l y r)
       (if (< x y)
           (node (insert x l) y r)
           (node l y (insert x r)))]))
;Zadanie 1. (2 pkt)
;Wstawianie do zwykłych binarnych drzew przeszukiwań może prowadzić do powsta-
;wania bardzo długich ścieżek w drzewie, a co za tym idzie do powolnego działania
;operacji na takim drzewie. Dlatego zaproponowano wiele struktur danych bazujących
;na drzewach przeszukiwań, ale dla których operacje wstawiania i usuwania dbają o to
;by drzewo było (w miarę) zbalansowane. Jedną z takich struktur są 2-3 drzewa, które
;można opisać za pomocą następujących warunków:
;• drzewo składa się z liści oraz wierzchołków dwóch rodzajów: takich co mają jeden
;element i dwoje dzieci oraz takich co mają dwa elementy i troje dzieci;
;• wszystkie ścieżki od korzenia do liścia mają tę samą długość, zwaną wysokością
;drzewa;
;• dla każdego wierzchołka przechowywany (każdy) element jest większy od wszyst-
;kich elementów lewego poddrzewa;
;• dla każdego wierzchołka przechowywany (każdy( element jest mniejszy od wszyst-
;kich elementów prawego poddrzewa;
;• dla wierzchołków mających troje dzieci lewy element (a) jest większy od prawego
;(b), a wszystkie elementy środkowego poddrzewa są pomiędzy a i b.
;Zdefiniuj typ opisujący 2-3 drzewa. Które własności 2-3 drzew potrafisz wymusić samą
;definicją typu? Napisz predykat sprawdzający pozostałe warunki.

  (define-type (2-3tree 'a)
  (leaf)
  (2-node (left : (2-3tree 'a)) (elem : 'a) (right : (2-3tree 'a)))
  (3-node (left : (2-3tree 'a)) (a : 'a)
          (middle : (2-3tree 'a))
          (b : 'a) (right : (2-3tree 'a))))

(define (t-true? t min max acc h)
  (type-case (2-3tree 'a) t
    [(leaf) (equal? acc h)]
    [(2-node l x r)
     (and (< min x)
          (< x max)
          (t-true? l min x (+ acc 1) h)
          (t-true? r x max (+ acc 1) h))]
    [(3-node l a m b r)
     (and (> a b)
          (< min a)
          (< min b)
          (< a max)
          (< b max)
          (t-true? m b a (+ acc 1) h)
          (t-true? l min b (+ acc 1) h)
          (t-true? r a max (+ acc 1) h))]))

(define (h-tree t acc)
  (type-case (2-3tree 'a) t
    [(leaf) acc]
    [(2-node l x r) (h-tree l (+ acc 1))]
    [(3-node l a m b r) (h-tree l (+ acc 1))]))

(define (2-3tree-true? t)
  (t-true? t -inf.0 +inf.0 0 (h-tree t 0)))