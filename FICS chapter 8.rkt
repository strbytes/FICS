#lang htdp/isl+
; Binary tree struct 
(define-struct Empty ())
(define-struct Node (val left right))

; Sample tree for testing
(define sample (make-Node 'A
  (make-Node 'B
    (make-Node 'D (make-Empty) (make-Empty))
    (make-Node 'F (make-Empty) (make-Empty)))
  (make-Node 'C
    (make-Node 'E (make-Empty) (make-Empty))
    (make-Empty))))

; brt-ref: Number Node -> Any
; Purpose: Find the value of an element in a Braun tree by index
; Examples:
(check-expect (brt-ref 0 sample) 'A)
(check-expect (brt-ref 4 sample) 'E)

(define (brt-ref i brt)
  (cond
    [(Empty? brt) (error "index too large")]
    [(zero? i) (Node-val brt)]
    [(odd? i)  (brt-ref (/ (sub1 i) 2) (Node-left brt))]
    [(even? i) (brt-ref (sub1 (/ i 2)) (Node-right brt))]))

; brt-head: Node -> Any
; Purpose: Return the value of the element at index 0
; Examples:
(check-expect (brt-head sample) 'A)
(check-expect (brt-head (make-Node 5 (make-Empty) (make-Empty))) 5)

(define (brt-head n) (Node-val n))

; Exercise 39

; sample-extend: Node
; Purpose: Test case for brt-extend
(define sample-extend (make-Node 'X
  (make-Node 'A
    (make-Node 'C (make-Empty) (make-Empty))
    (make-Node 'E (make-Empty) (make-Empty)))
  (make-Node 'B
    (make-Node 'D (make-Empty) (make-Empty))
    (make-Node 'F (make-Empty) (make-Empty)))))

; brt-extend: Node Any -> Node
; Purpose: Add an element to be beginning of a Braun tree
; Examples:
(check-expect (brt-extend 'X sample) sample-extend)
(check-expect (brt-extend 4 (make-Node 5 (make-Empty) (make-Empty))) 
  (make-Node 4  (make-Node 5 (make-Empty) (make-Empty)) 
                (make-Empty)))

(define (brt-extend val n)
  (cond
    [(Empty? n) (make-Node val (make-Empty) (make-Empty))]
    [else (make-Node val (brt-extend (Node-val n) (Node-right n)) (Node-left n))]))

; sample-tail: Node
; Purpose: Test case for brt-tail
(define sample-tail (make-Node 'B
  (make-Node 'C
    (make-Node 'E (make-Empty) (make-Empty))
    (make-Empty))
  (make-Node 'D 
    (make-Node 'F (make-Empty) (make-Empty)) 
    (make-Empty))))

; brt-tail: Node -> Node
; Purpose: Return the subtree of all elements > 0
; Examples:
(check-expect (brt-tail sample) sample-tail)
(check-expect (brt-tail 
  (make-Node 0 (make-Node 1 (make-Empty) (make-Empty)) (make-Empty)))
               (make-Node 1 (make-Empty) (make-Empty)))

(define (brt-tail n)
  (cond
    [(Empty? n) (error "empty")]
    [(Empty? (Node-left n)) (make-Empty)]
    [else (make-Node (Node-val (Node-left n)) (Node-right n) (brt-tail (Node-left n)))]))

(define sample-update (make-Node 'A
  (make-Node 'B
    (make-Node 'X (make-Empty) (make-Empty))
    (make-Node 'F (make-Empty) (make-Empty)))
  (make-Node 'C
    (make-Node 'E (make-Empty) (make-Empty))
    (make-Empty))))

; brt-update: Node Number Any -> Node
; Purpose: Update the value of a Braun tree at a specified index
; Examples:
(check-expect (brt-update sample 3 'X) sample-update)
(check-expect (brt-update 
  (make-Node 0 (make-Node 1 (make-Empty) (make-Empty)) (make-Empty)) 1 2)
  (make-Node 0 (make-Node 2 (make-Empty) (make-Empty)) (make-Empty)))

(define (brt-update n i val)
  (cond
    [(Empty? n) (error "index out of bounds")]
    [(zero? i) (make-Node val (Node-left n) (Node-right n))]
    [(odd? i) (make-Node (Node-val n) 
      (brt-update (Node-left n) (/ (sub1 i) 2) val) 
      (Node-right n))]
    [(even? i) (make-Node (Node-val n)
      (Node-left n)
      (brt-update (Node-right n) (sub1 (/ i 2)) val))]))

; Exercise 39

; sample-verify: Node
; Purpose: Invalid Braun tree for brt-verify test case
(define sample-verify (make-Node 'A
  (make-Node 'B
    (make-Node 'D 
      (make-Node 'X (make-Empty) (make-Empty)) 
      (make-Empty))
    (make-Node 'F (make-Empty) (make-Empty)))
  (make-Node 'C (make-Empty) (make-Empty))))

; brt-verify: Node -> Boolean or Number
; Purpose: Return false if the Node passed in is not a Braun tree, else return 
;   the height of the tree
; Examples:
(check-expect (brt-verify sample) 3)
(check-expect (brt-verify (make-Node 0 
  (make-Empty) 
  (make-Node 1 (make-Empty) (make-Empty)))) #f)

(define (brt-verify n)
  (cond
    [(Empty? n) 0]
    [else (let ([lheight (brt-verify (Node-left n))] 
                [rheight (brt-verify (Node-right n))])
      (cond
        [(or (false? lheight) (false? rheight)) #f]
        [(or (< lheight rheight) (> (- lheight 1) rheight)) #f]
        [else (+ 1 lheight)]))]))

(check-expect (brt-verify sample-tail) 3)
(check-expect (brt-verify sample-extend) 3)
(check-expect (brt-verify sample-update) 3)
(check-expect (brt-verify sample-verify) #f)

; Exercise 40

; Structs for Braun tree binary implementation
(define-struct Z ())      ; zero
(define-struct C (prev))  ; odd (left)
(define-struct D (prev))  ; even (right)

; to-braun: Number -> Struct 
; Purpose: Convert a Racket number into a Braun-encoded binary structure
; Examples:
(check-expect (to-Braun 5) (make-C (make-D (make-Z))))
(check-expect (to-Braun 8) (make-D (make-C (make-C (make-Z)))))

(define (to-Braun n)
  (cond
    [(zero? n) (make-Z)]
    [(odd? n) (make-C (to-Braun (/ (sub1 n) 2)))]
    [(even? n) (make-D (to-Braun (sub1 (/ n 2))))]))

; from-Braun: Struct -> Number
; Purpose: Convert a Braun-encoded binary structure to a Racket number
; Examples:
(check-expect (from-Braun (to-Braun 5)) 5)
(check-expect (from-Braun (to-Braun 8)) 8)

(define (from-Braun b)
  (cond
    [(Z? b) 0]
    [(C? b) (add1 (* (from-Braun (C-prev b)) 2))]
    [(D? b) (* (add1 (from-Braun (D-prev b))) 2)]))

; add-Braun: Struct Struct -> Struct
; Purpose: Add two natural numbers in a Braun-encoded binary structure
; Examples:
(check-expect (from-Braun (add-Braun (to-Braun 5) (to-Braun 8))) 13)
(check-expect (from-Braun (add-Braun (to-Braun 10) (to-Braun 16))) 26)

(define (add-Braun b1 b2)
  (cond
    [(Z? b1) b2]
    [(Z? b2) b1]
    [(and (C? b1) (C? b2)) 
      (make-D (add-Braun (C-prev b1) (C-prev b2)))]
    [(and (C? b1) (D? b2))
      (make-C (add1-Braun (add-Braun (C-prev b1) (D-prev b2))))]
    [(and (D? b1) (C? b2))
      (make-C (add1-Braun (add-Braun (D-prev b1) (C-prev b2))))]
    [(and (D? b1) (D? b2))
      (make-D (add1-Braun (add-Braun (D-prev b1) (D-prev b2))))]))

; add1-Braun: Struct -> Struct
; Purpose: Increase the value of a Braun-encoded binary natural number by one
; Examples:
(check-expect (from-Braun (add1-Braun (to-Braun 5))) 6)
(check-expect (from-Braun (add1-Braun (to-Braun 10))) 11)

(define (add1-Braun b)
  (cond
    [(Z? b) (make-C (make-Z))]
    [(C? b) (make-D (C-prev b))]
    [(D? b) (make-C (add1-Braun (D-prev b)))]))

; mul-Braun: Struct -> Struct
; Purpose: Multiply two Braun-encoded natural numbers
; Examples
(check-expect (from-Braun (mul-Braun (to-Braun 3) (to-Braun 4))) 12)
(check-expect (from-Braun (mul-Braun (to-Braun 45) (to-Braun 5))) 225)

(define (mul-Braun b1 b2)
  (cond
    [(Z? b1) (make-Z)] ; Probably not efficient but it's the best I could do
    [(C? b1) (add-Braun b2 (mul-Braun (sub1-Braun b1) b2))]
    [(D? b1) (add-Braun b2 (mul-Braun (sub1-Braun b1) b2))]))

; sub-Braun: Struct -> Struct
; Purpose: Decrease the value of a Braun-encoded binary natural number by one
; Examples:
(check-expect (from-Braun (sub1-Braun (to-Braun 5))) 4)
(check-expect (from-Braun (sub1-Braun (to-Braun 54))) 53)

(define (sub1-Braun b)
  (cond
    [(C? b) (if (Z? (C-prev b)) (make-Z)  (make-D (sub1-Braun (C-prev b))))]
    [(D? b) (if (Z? (D-prev b)) (make-C (make-Z)) (make-C (D-prev b)))]))

; Exercise 41

; bst-sample: BST
; Purpose: Binary Search Tree for testing
(define bst-sample 
  (make-Node 5
    (make-Node 3
      (make-Node 1 (make-Empty) (make-Empty))
      (make-Node 4 (make-Empty) (make-Empty)))
    (make-Node 8
      (make-Node 6 (make-Empty) (make-Empty))
      (make-Empty))))

; element-of: BST Any -> Boolean
; Purpose: Return whether a number is a member of a binary search tree
; Examples:
(check-expect (element-of bst-sample 8) #t)
(check-expect (element-of bst-sample 7) #f)

(define (element-of bst val)
  (cond
    [(Empty? bst) #f]
    [(= (Node-val bst) val) #t]
    [(> (Node-val bst) val) (element-of (Node-left bst) val)]
    [(< (Node-val bst) val) (element-of (Node-right bst) val)]))

; bst-sample-add: BST
; Purpose: Test case for bst-add
(define bst-sample-add
  (make-Node 5
    (make-Node 3
      (make-Node 1 (make-Empty) (make-Empty))
      (make-Node 4 (make-Empty) (make-Empty)))
    (make-Node 8
      (make-Node 6 
        (make-Empty)
        (make-Node 7 (make-Empty) (make-Empty)) )
      (make-Empty))))

; add-bst: BST val -> BST
; Purpose: Add a value to a BST in the correct location (removes duplicates)
; Examples:
(check-expect (add-bst bst-sample 7) bst-sample-add)
(check-expect (add-bst bst-sample 1) bst-sample)

(define (add-bst bst val)
  (cond
    [(Empty? bst) (make-Node val (make-Empty) (make-Empty))]
    [(= (Node-val bst) val) bst]
    [(> (Node-val bst) val) 
      (make-Node (Node-val bst) (add-bst (Node-left bst) val) (Node-right bst))]
    [(< (Node-val bst) val) 
      (make-Node (Node-val bst) (Node-left bst) (add-bst (Node-right bst) val))]))

(check-expect (add-bst (add-bst (add-bst (make-Empty) 5) 4) 6)
  (make-Node 5  (make-Node 4 (make-Empty) (make-Empty)) 
                (make-Node 6 (make-Empty) (make-Empty))))

; bst-sample-remove: BST
; Purpose: Test case for bst-remove
(define bst-sample-remove
  (make-Node 5
    (make-Node 3
      (make-Empty)
      (make-Node 4 (make-Empty) (make-Empty)))
    (make-Node 8
      (make-Node 6 (make-Empty) (make-Empty))
      (make-Empty))))

; bst-remove: BST val -> BST
; Purpose: Remove a numeric value from a BST. Raise an error if value is not found.
; Examples:
(check-expect (bst-remove bst-sample 1) bst-sample-remove)
(check-error (bst-remove bst-sample 2))

(define (bst-remove bst val)
  (cond
    [(Empty? bst) (error "value not in tree or tree not well-formed")]
    [(< val (Node-val bst)) 
      (make-Node (Node-val bst) (bst-remove (Node-left bst) val) (Node-right bst))]
    [(> val (Node-val bst))
      (make-Node (Node-val bst) (Node-left bst) (bst-remove (Node-right bst) val))]
    [(= val (Node-val bst))
      (cond
        [(Empty? (Node-left bst)) (Node-right bst)]
        [(Empty? (Node-right bst)) (Node-left bst)]
        [else (let ([max-left (max-val (Node-left bst))])
          (make-Node max-left (bst-remove (Node-left bst) max-left) (Node-right bst)))])]))

; max-val: BST -> Number
; Purpose: Return the highest value in a BST
; Examples:
(check-expect (max-val bst-sample) 8)
(define (max-val bst)
  (cond
    [(Empty? bst) (error "empty bst")]
    [(Empty? (Node-right bst)) (Node-val bst)]
    [else (max-val (Node-right bst))]))

; Exercise 42

; bst-sample-invalid: BST
; Purpose: Test case for valid-bst?(define bst-sample 
(define bst-sample-invalid
  (make-Node 5
    (make-Node 3
      (make-Node 1 (make-Empty) (make-Empty))
      (make-Node 4 (make-Empty) (make-Empty)))
    (make-Node 8
      (make-Empty)
      (make-Node 6 (make-Empty) (make-Empty)))))

; valid-bst?: BST -> Boolean
; Purpose: Returns false if the provided tree is not a valid BST, else returns 
;   the largest value found in a subtree
; Examples:
(check-expect (valid-bst? bst-sample) 8)
(check-expect (valid-bst? bst-sample-add) 8)
(check-expect (valid-bst? bst-sample-invalid) #f)

(define (valid-bst? bst)
  (cond ; should probably alter this to use >= and <=
    [(and (Empty? (Node-left bst)) (Empty? (Node-right bst))) (Node-val bst)]
    [(Empty? (Node-left bst)) 
      (let ([max-right (valid-bst? (Node-right bst))])
        (cond
          [(false? max-right) #f]
          [(> (Node-val bst) max-right) #f]
          [else max-right]))]
    [(Empty? (Node-right bst))
      (let ([max-left (valid-bst? (Node-left bst))])
        (cond
          [(false? max-left) #f]
          [(> max-left (Node-val bst)) #f]
          [else (Node-val bst)]))]
    [else 
      (let ([max-left (valid-bst? (Node-left bst))] 
            [max-right (valid-bst? (Node-right bst))])
        (cond
          [(or (false? max-left) (false? max-right)) #f]
          [(> (Node-val bst) max-right) #f]
          [(> max-left (Node-val bst)) #f]
          [else max-right]))]))