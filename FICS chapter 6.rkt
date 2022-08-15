;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |FICS chapter 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 26
; trans-loc: (Listof (Listof (Char -> Bool) (Char -> Char))) (Listof Char) -> (Listof Char)

; Exercise 27
; foldr: (X -> Y) (Listof X) X -> (Listof Y)

; Exercise 28
; foldr-filter: (X -> boolean) (Listof X) -> (Listof X)
; Purpose: recreate the functionality of filter using foldr
; Examples:
(check-expect (foldr-filter even? '(1 2 3 4)) '(2 4))
(check-expect (foldr-filter (lambda (x) (= (modulo x 3) 0)) '(2 3 5 6)) '(3 6))

(define (foldr-filter f lst)
  (foldr (lambda (x y) (if (f x) (cons x y) y)) empty lst))

; Exercise 29
; (foldr cons empty mylist) -> copy mylist
; (foldr cons mylist1 mylist2) -> append mylist1 to the end of mylist2

; Exercise 30

; unique-left: (Listof Any) -> (Listof Any)
; Purpose: Return a copy of lst with duplicates removed. The earliest occurrences
; of duplicates are retained.
; Examples:
(check-expect (unique-left (list 1 4 2 1 5 4)) (list 1 4 2 5))
(check-expect (unique-left (list 1 1 2 1 1)) (list 1 2))

(define (unique-left lst)
  (foldr (lambda (x y) (cons x (foldr-filter (lambda (z) (not (= z x))) y))) empty lst))

; in: Any (Listof Any) -> Bool
; Purpose: Returns true if x is in lst, else false.
; Examples:
(check-expect (in (list 1 2 3 4) 2) #t)
(check-expect (in (list 1 2 3 4) 5) #f)

(define (in lst x)
  (foldr (lambda (y z) (if (= x y) #t z)) #f lst))
  
; unique-right: (Listof Any) -> (Listof Any)
; Purpose: Return a copy of lst with duplicates removed. The last occurrences
; of duplicates are retained.
; Examples:
(check-expect (unique-right (list 1 4 2 1 5 4)) (list 2 1 5 4))
(check-expect (unique-right (list 1 1 2 1 1)) (list 2 1))

(define (unique-right lst)
  (foldr (lambda (x y) (if (in (unique-right y) x) y (cons x y))) empty lst))

; Exercise 31
; cross: (Listof X) (Listof Y) -> (Listof (Listof X Y))
; Purpose: Combine lst1 and lst2 to create an ordered list of lists of their combinations
; Examples:
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))

(define (cross lst1 lst2)
  (foldr (lambda (x1 y1) (append
           (foldr (lambda (x2 y2) (cons (list x1 x2) y2)) empty lst2)
           y1)) empty lst1))

; Exercise 32
; unfoldr: (X -> Boolean) (X -> Any) (X -> X) Any -> (Listof Any)
; Purpose: Generate a list starting with f(s), consisting of successive applications of f(g(s)),
; until p(g(s)) is true.
; Examples:
(check-expect (unfoldr (lambda (x) (= x 0)) (lambda (x) x) (lambda (x) (- x 1)) 3) (list 3 2 1))

(define (unfoldr p f g s)
  (cond
    [(p s) empty]
    [else (cons (f s) (unfoldr p f g (g s)))]))

; map-unfoldr: (Any -> Any) (Listof Any) -> (Listof Any)
; Purpose: Return a new list consisting of elements of lst that have had f applied to them
; Examples:
(check-expect (map-unfoldr sqr '(1 2 3)) '(1 4 9))
(check-expect (map-unfoldr (lambda (x) (if (even? x) (sqr x) x)) '(1 2 3 4)) '(1 4 3 16))

(define (map-unfoldr f lst)
  (unfoldr empty? (lambda (x) (f (first x))) rest lst))

; build-unfoldr: Number (Number -> Any) -> (Listof Any)
; Purpose: Constructs a list by applying f to the numbers between 0 and (- n 1)
; Examples:
(check-expect (build-unfoldr 22 add1) (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))
(check-expect (build-list 5 even?) '(#t #f #t #f #t))

(define (build-unfoldr n f)
  (unfoldr (lambda (x) (= x n)) f add1 0))

; mult-table: Number Number -> (Listof (Lisitof Numbers))
; Purpose: Produces a list of n lists, each of length n, containing the multiplied indices of
; the position in the two lists
; Examples
(check-expect (mult-table 2 2) '((0 0) (0 1)))
(check-expect (mult-table 3 3) '((0 0 0) (0 1 2) (0 2 4)))

(define (mult-table n m)
  (unfoldr (lambda (x) (= x n)) (lambda (x) (build-unfoldr m (lambda (y) (* x y)))) add1 0))

; Exercise 33
; I've already seen this in CS61a

; Exercise 34
; Add a cond case [(symbol=? msg 'type) 'my-point]

















