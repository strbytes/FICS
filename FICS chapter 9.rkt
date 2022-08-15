#lang htdp/isl+

; gcd: Number Number -> Number
; Purpose: Find the greatest common denominator of two numbers using Euclid's Algorithm

; (define (gcd n m)
;   (cond
;     [(zero? n) m]
;     [(zero? m) n]
;     [else (gcd m (remainder n m))]))


; flat: (Listof Lists) -> (Listof Any)
; Purpose: Recursively bring values out of nested lists so they are in a flat list
; Examples:
(check-expect (flat '((0 1 0))) '(0 1 0))
(check-expect (flat '((0 1 0 0 1) (0 1 0 0 1))) '(0 1 0 0 1 0 1 0 0 1))

(define (flat lst)
    (cond
        [(empty? lst) empty]
        [(list? (first lst)) (append (flat (first lst)) (flat (rest lst)))]
        [else (cons (first lst) (flat (rest lst)))]))

(check-expect (flat '(((0 1 0 0 1) (0 1 0 0 1)) ((0 1 0)))) '(0 1 0 0 1 0 1 0 0 1 0 1 0))

; make-repeat: Number Any -> (Listof Any)
; Purpose: Create a list containing num copies of val
; Examples:
(check-expect (make-repeat 4 1) '(1 1 1 1))
(check-expect (make-repeat 5 '(0)) '((0) (0) (0) (0) (0)))

(define (make-repeat num val)
    (cond
        [(zero? num) empty]
        [else (cons val (make-repeat (sub1 num) val))]))

; combine: (Listof Lists) (Listof Lists) -> (Listof Lists) (Listof Lists)
; Purpose: Combine two lists of partial bjorklund solutions
; Examples:
(check-expect (combine '((0) (0) (0)) '((1))) '(((0 1)) ((0) (0))))
(check-expect (combine '((0 1 0) (0 1 0) (0 1 0)) '((0 1) (0 1))) 
    '(((0 1 0 0 1) (0 1 0 0 1)) ((0 1 0))))

(define (combine p1 p2)
    (cond
        [(> (length p2) (length p1)) (combine p2 p1)]
        [(empty? (rest p2)) (list (list (append (first p1) (first p2))) (rest p1))]
        [else (let ([soln (combine (rest p1) (rest p2))])
                (list (cons (append (first  p1) (first  p2)) (first soln))
                         (first (rest soln))))]))

(check-expect (combine (make-repeat 8 '(0)) (make-repeat 5 '(1)))
    '(((0 1) (0 1) (0 1) (0 1) (0 1)) ((0) (0) (0))))
(check-expect (combine '((1)) '((0))) '(((1 0)) ()))

; bjork-helper: (Listof Numbers) (Listof Numbers) -> (Listof Numbers)
; Purpose: Perform the recursive application of the bjorklund algorithm
; Examples:
(check-expect (bjork-helper (make-repeat 8 '(0)) (make-repeat 4 '(1)))
    '(((0 1 0) (0 1 0) (0 1 0) (0 1 0)) ()))
(check-expect (bjork-helper (make-repeat 8 '(0)) (make-repeat 5 '(1)))
    '(((0 1 0 0 1) (0 1 0 0 1)) ((0 1 0))))

(define (bjork-helper s1 s2)
    (cond
        [(empty? s2) (list s1 s2)]
        [(empty? (rest s2)) (list s1 s2)]
        [else (let ([soln (combine s1 s2)])
            (bjork-helper (first soln) (first (rest soln))))]))

; bjorklund: Number Number -> (Listof Numbers)
; Purpose: Distribute an integer number of pulses over an integer number of slots
;   in as distributed a manner as possible
; Examples:
(check-expect (bjorklund 12 4) '(0 1 0 0 1 0 0 1 0 0 1 0))
(check-expect (bjorklund 13 5) '(0 1 0 0 1 0 1 0 0 1 0 1 0))

(define (bjorklund slots pulses) 
    (flat (bjork-helper (make-repeat (- slots pulses) '(0)) (make-repeat pulses '(1)))))

(check-expect (bjorklund 21 13) '(1 0 1 1 0 1 0 1 1 0 1 1 0 1 0 1 1 0 1 1 0))
(check-expect (bjorklund 50 25) (flat (make-repeat 25 '(0 1))))

;;; Didn't do this quite right -- it works but it's supposed to keep track of a struct
; similar to make-repeat, with only the number of repeats and the sequence to be repeated
; -- this is what's supposed to be computed upon in bjork-helper to produce the result

; Exercise 44

; insertion-sort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using insertion sort
; Examples:
(check-expect (insertion-sort '(2 1 3)) '(1 2 3))
(check-expect (insertion-sort '(3 3 6 2 4 1)) '(1 2 3 3 4 6))

(define (insertion-sort lst)
    (cond
        [(empty? lst) empty]
        [else (insert-helper (first lst) (insertion-sort (rest lst)))]))

; insert-helper: Number (Listof Numbers) -> (Listof Numbers)
; Purpose: Insert a number into an ordered list of numbers in the correct place
; Examples:
(check-expect (insert-helper 3 '(2 4 5)) '(2 3 4 5))
(check-expect (insert-helper 3 '(1 2 3 4 5)) '(1 2 3 3 4 5))

(define (insert-helper val lst)
    (cond
        [(empty? lst) (list val)]
        [(> val (first lst)) (cons (first lst) (insert-helper val (rest lst)))]
        [(<= val (first lst)) (cons val lst)]))

; Exercise 45

; selection-sort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using selection sort
; Examples:
(check-expect (selection-sort '(2 1 3)) '(1 2 3))
(check-expect (selection-sort '(3 3 6 2 4 1)) '(1 2 3 3 4 6))

(define (selection-sort lst)
    (cond
        [(empty? lst) empty]
        [else (let ([smallest (apply min lst)])
            (cons smallest (selection-sort (remove smallest lst))))]))

; Exercise 46

;;;;;;;;;;;;;;;;;;;; Copied from chapter 8
; Binary tree struct 
(define-struct Empty ())
(define-struct Node (val left right))

; add-bst: BST val -> BST
; Purpose: Add a value to a BST in the correct location (removes duplicates)

(define (add-bst bst val)
  (cond
    [(Empty? bst) (make-Node val (make-Empty) (make-Empty))]
    [(= (Node-val bst) val) bst]
    [(> (Node-val bst) val) 
      (make-Node (Node-val bst) (add-bst (Node-left bst) val) (Node-right bst))]
    [(< (Node-val bst) val) 
      (make-Node (Node-val bst) (Node-left bst) (add-bst (Node-right bst) val))]))
;;;;;;;;;;;;;;;;;;;;

; tree-sort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using tree sort
; Examples:
(check-expect (tree-sort '(2 1 3)) '(1 2 3))
(check-expect (tree-sort '(3 3 6 2 4 1)) '(1 2 3 3 4 6))

(define (tree-sort lst) (flat-tree (tree-helper lst)))

; tree-helper: (Listof Numbers) -> (BSTof Numbers)
; Purpose: Convert a list of numbers into a binary search tree
; Examples:
(check-expect (tree-helper '(3 1 2)) 
    '(make-Node 2 
        (make-Node 1 (make-Empty) (make-Empty)) 
        (make-Node 3 (make-Empty) (make-Empty))))

(define (tree-helper lst) ; this is pure structural recursion,
    (cond                 ; this was supposed to use an accumulator
        [(empty? lst) (make-Empty)]
        [else (add-bst (tree-helper (cdr lst)) (car lst))]))

; flat-tree: BST -> (Listof Any)
; Purpose: Convert a binary search tree into an ordered list
; Examples:
(check-expect (flat-tree
    '(make-Node 2 
        (make-Node 1 (make-Empty) (make-Empty)) 
        (make-Node 3 (make-Empty) (make-Empty))))
    '(1 2 3))

(define (flat-tree bst)
    (cond
        [(Empty? bst) empty]
        [else (append 
            (flat-tree (Node-left bst)) 
            (list (Node-val bst)) 
            (flat-tree (Node-right bst)))]))

; Exercise 47

; tree-sort-2: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using tree sort with an improved flattening function
; Examples:
(check-expect (tree-sort-2 '(2 1 3)) '(1 2 3))
(check-expect (tree-sort-2 '(3 3 6 2 4 1)) '(1 2 3 4 6))

(define (tree-sort-2 lst) 
    (flat-tree-2 empty (tree-helper-2  lst (make-Empty))))

; flat-tree-2: BST -> (Listof Any)
; Purpose: Convert a binary search tree into an ordered list using an accumulator
; Examples:
(check-expect (flat-tree-2
    '(make-Node 2 
        (make-Node 1 (make-Empty) (make-Empty)) 
        (make-Node 3 (make-Empty) (make-Empty))))
    '(1 2 3))

(define (flat-tree-2 lst bst)
    (cond
        [(Empty? bst) empty]
        [else (append 
            (flat-tree-2 lst (Node-left bst))
            (list (Node-val bst))
            lst
            (flat-tree-2 lst (Node-right bst)))]))

; tree-helper-2: (Listof Numbers) BST -> BST
; Purpose: Was supposed to write tree-helper using an accumulator in the first place
;   but used pure structural recursion instead. Including the proper implementation here
;   since I'm already rewriting the other helper here.

(define (tree-helper-2 lst t)
    (cond
        [(empty? lst) t]
        [else (add-bst (tree-helper-2 (cdr lst) t) (car lst))]))

; Exercise 48

; quick-treesort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using the quicksort-style tree sort algorithm
; Examples:
(check-expect (quick-treesort '(2 1 3)) '(1 2 3))
(check-expect (quick-treesort '(3 3 6 2 4 1)) '(1 2 3 4 6))

(define (quick-treesort lst) (flat-tree-2 empty (quick-tree lst)))

; quick-tree: (Listof Numbers) -> BST
; Purpose: Build a BST out of a list of numbers using co-recursion
; Examples:
(check-expect (quick-tree
    '(make-Node 2 
        (make-Node 1 (make-Empty) (make-Empty)) 
        (make-Node 3 (make-Empty) (make-Empty))))
    '(1 2 3))

(define (quick-tree lst)
    (cond
        [(empty? lst) (make-Empty)]
        [else (make-Node (car lst)
            (quick-tree (filter (lambda (x) (< x (car lst))) lst))
            (quick-tree (filter (lambda (x) (> x (car lst))) lst)))]))

; Exercise 49

; quick-sort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using the quicksort algorithm
(check-expect (quick-sort '(2 1 3)) '(1 2 3))
(check-expect (quick-sort '(3 3 6 2 4 1)) '(1 2 3 4 6))

(define (quick-sort lst)
    (cond
        [(empty? lst) empty]
        [else (append
            (quick-sort (filter (lambda (x) (< x (car lst))) lst))
            (list (car lst))
            (quick-sort (filter (lambda (x) (> x (car lst))) lst)))]))

; Exercise 50

; merge: List List -> List
; Purpose: Merge two sorted lists, maintaining their order
; Examples:
(check-expect (merge '(1 3 5) '(2 4 6)) '(1 2 3 4 5 6))
(check-expect (merge '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (merge l1 l2)
    (cond
        [(empty? l1) l2]
        [(empty? l2) l1]
        [(<= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))]
        [(> (car l1) (car l2)) (cons (car l2) (merge l1 (cdr l2)))]))

; Exercise 51

; make-lists: (Listof Any) -> (Listof Lists)
; Purpose: Create a list of single-element lists out of the elements of a list
; Examples:
(check-expect (make-lists '(1 2 3)) '((1) (2) (3)))

(define (make-lists lst)
    (cond
        [(empty? lst) empty]
        [else (cons (list (car lst)) (make-lists (cdr lst)))]))

; merge-pairs: (Listof Lists) -> (Listof Lists)
; Purpose: Merge pairs of lists in a list of ordered lists
; Examples:
(check-expect (merge-pairs '((1) (2) (3))) '((1 2) (3)))
(check-expect (merge-pairs '((1) (2) (3) (4))) '((1 2) (3 4)))

(define (merge-pairs lol)
    (cond
        [(empty? lol) empty]
        [(empty? (cdr lol)) lol]
        [else (cons (merge (car lol) (cadr lol)) (merge-pairs (cddr lol)))]))

; merge-many: (Listof Lists) -> (Listof (a) List)
; Purpose: Repeatedly apply merge-pairs until all are merged into one list
; Examples:
(check-expect (merge-many (make-lists '(1 2 3 4))) '((1 2 3 4)))
(check-expect (merge-many (make-lists '(4 3 2 1))) '((1 2 3 4)))

(define (merge-many lsts)
    (cond
        [(empty? (cdr lsts)) lol]
        [else (merge-many (merge-pairs lsts))]))

; mergesort-b: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list using a bottoms-up merge sort algorithm
; Examples:
(check-expect (mergesort-b '(2 1 3)) '(1 2 3))
(check-expect (mergesort-b '(3 3 6 2 4 1)) '(1 2 3 4 6))

(define (mergesort-b lst)
    (cond
        [(empty? lst) empty]
        [else (car (merge-many (make-lists lst)))]))

; Exercise 52

; mergesort: (Listof Numbers) -> (Listof Numbers)
; Purpose: Sort a list of numbers using merge sort
; Examples:
(check-expect (mergesort '(2 1 3)) '(1 2 3))
(check-expect (mergesort '(3 3 6 2 4 1)) '(1 2 3 4 6))

(define (mergesort lst)
    (cond
        [(empty? lst) empty]
        [(empty? (cdr lst)) lst]
        [else (let ([len-list (length lst)])
            (merge 
                (mergesort (keep-first lst (quotient len-list 2))) 
                (mergesort (drop-first lst (quotient len-list 2)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers from chapter 5
; keep-first : (Listof Any) Number -> (Listof Any)
; Purpose: Return a new list containing only the first n elements of lst
; Examples
(check-expect (keep-first (list 1 2 3 4) 2) (list 1 2))
(check-expect (keep-first (list 1 2 3 4) 5) (list 1 2 3 4))

(define (keep-first lst n)
  (cond
    [(or (= n 0) (empty? lst)) empty]
    [else (cons (first lst) (keep-first (rest lst) (- n 1)))]))

; drop-first: (Listof Any) Number -> (Listof Any)
; Purpose: Return a new list containing only elements AFTER the first n elements of lst
; Examples:
(check-expect (drop-first (list 1 2 3 4) 2) (list 3 4))
(check-expect (drop-first (list 1 2 3 4) 5) (list))

(define (drop-first lst n)
  (cond
    [(empty? lst) empty]
    [(> n 0) (drop-first (rest lst) (- n 1))]
    [(= n 0) (cons (first lst) (drop-first (rest lst) 0))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;