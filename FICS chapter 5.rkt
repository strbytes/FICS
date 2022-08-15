;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |FICS chapter 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; substitute: Any Any (Listof Any): (Listof Any)
; Purpose: Return a copy of lst where any values that are equivalent to x are replaced with y
; Examples:
(check-expect  (substitute 3 "three" (list "four" 3 4 "three" 3))
               (list "four" "three" 4 "three" "three"))

(define (substitute x y lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (if (equal? (first lst) x)
                     (cons y (substitute x y (rest lst)))
                     (cons (first lst) (substitute x y (rest lst))))]))

(check-expect (substitute 1 2 (list 1 1 1 1 1)) (list 2 2 2 2 2))
(check-expect (substitute 1 2 (list 2 2 2 2 2)) (list 2 2 2 2 2))

; filter-item: (Listof Any) Any -> (Listof Any)
; Purpose: Return a copy of lst with any occurrences of x removed
; Examples:
(check-expect (filter-item (list 1 2 3 4) 2) (list 1 3 4))
(check-expect (filter-item (list 1 2 3 4) 5) (list 1 2 3 4))

(define (filter-item lst x)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (if (equal? (first lst) x)
                     (filter-item (rest lst) x)
                     (cons (first lst) (filter-item (rest lst) x)))]))

; unique-left: (Listof Any) -> (Listof Any)
; Purpose: Return a copy of lst with duplicates removed. The earliest occurrences
; of duplicates are retained.
; Examples:
(check-expect (unique-left (list 1 4 2 1 5 4)) (list 1 4 2 5))
(check-expect (unique-left (list 1 1 2 1 1)) (list 1 2))

(define (unique-left lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (cons (first lst) (unique-left (filter-item (rest lst) (first lst))))]))

; in: Any (Listof Any) -> Bool
; Purpose: Returns true if x is in lst, else false.
; Examples:
(check-expect (in (list 1 2 3 4) 2) #t)
(check-expect (in (list 1 2 3 4) 5) #f)

(define (in lst x)
  (cond
    [(empty? lst) #f]
    [(cons? lst) (if (equal? (first lst) x) #t (in (rest lst) x))]))

; unique-right: (Listof Any) -> (Listof Any)
; Purpose: Return a copy of lst with duplicates removed. The last occurrences
; of duplicates are retained.
; Examples:
(check-expect (unique-right (list 1 4 2 1 5 4)) (list 2 1 5 4))
(check-expect (unique-right (list 1 1 2 1 1)) (list 2 1))

(define (unique-right lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst) (if (in (unique-right (rest lst)) (first lst))
                     (unique-right (rest lst))
                     (cons (first lst) (unique-right (rest lst))))]))

; is-leap: Number -> Bool
; Purpose: Return true if year is a leap year
; Examples:
(check-expect (is-leap 2004) #t)
(check-expect (is-leap 2001) #f)

(define (is-leap year)
  (or (= (remainder year 400) 0) (and (= (remainder year 4) 0) (not (= (remainder year 100) 0)))))

(check-expect (is-leap 1600) #t)
(check-expect (is-leap 1700) #f)

; days-in-month: (Listof Numbers)
; Purpose: Reference for number of days in each month
(define days-in-month (list 31 28 31 30 31 30 31 31 30 31 30 31))

; sum-to-index: (Listof Numbers) Number -> Number
; Purpose: Sum the entries in a list prior to the index i
; Examples:
(check-expect (sum-to-index days-in-month 1) 31)
(check-expect (sum-to-index days-in-month 2) 59)

(define (sum-to-index lst i)
  (cond
    [(= i 0) 0]
    [(> i 0) (+ (first lst) (sum-to-index (rest lst) (- i 1)))]))

; day-within-year: Number Number Number -> Number
; Purpose: Returns the number of the day of the year (out of 365) specified by year, month, and day.
; Examples:
(check-expect (day-within-year 2001 9 11) 254)

(define (day-within-year year month day)
  (if (is-leap year) 
      (+ (sum-to-index days-in-month (- month 1)) day 1) ; add leap day if leap year
      (+ (sum-to-index days-in-month (- month 1)) day)))



; Exercise 17
; keep-first : (Listof Any) Number -> (Listof Any)
; Purpose: Return a new list containing only the first n elements of lst
; Examples
(check-expect (keep-first (list 1 2 3 4) 2) (list 1 2))
(check-expect (keep-first (list 1 2 3 4) 5) (list 1 2 3 4))

(define (keep-first lst n)
  (cond
    [(or (= n 0) (empty? lst)) empty]
    [else (cons (first lst) (keep-first (rest lst) (- n 1)))]))

; Exercise 18
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

; Exercise 19
; sublist: (Listof Any) Number Number -> (Listof Any)
; Purpose: Return a new list containing only the elements from lst between index n and m - 1
; Examples:
(check-expect (sublist (list 1 2 3 4) 0 2) (list 1 2))
(check-expect (sublist (list 1 2 3 4) 2 3) (list 3))

(define (sublist lst n m)
  (cond
    [(> n m) (error "n must be greater than m")]
    [(> n 0) (sublist (drop-first lst n) 0 (- m n))]
    [(and (= n 0) (> m 0)) (keep-first lst m)]
    [else empty]))

(check-error (sublist (list 1 2 3) 3 2) "n must be greater than m")
(check-expect (sublist (list 1 2 3) 0 4) (list 1 2 3))
(check-expect (sublist (list 1 2 3) 2 2) (list))

; Exercise 20
; intersection: (Listof Numbers) (Listof Numbers) -> (Listof Numbers)
; Purpose: Return the intersection of two ordered Sets
; Examples:
(check-expect (intersection (list 1 2 3) (list 2 3 4)) (list 2 3))
(check-expect (intersection (list 1 2) (list 3 4)) (list))

(define (intersection lst1 lst2)
  (cond
    [(or (empty? lst1) (empty? lst2)) empty]
    [(= (first lst1) (first lst2)) (cons (first lst1) (intersection (rest lst1) (rest lst2)))]
    [(> (first lst1) (first lst2)) (intersection lst1 (rest lst2))]
    [(< (first lst1) (first lst2)) (intersection (rest lst1) lst2)]))
    
(check-expect (intersection (list 1 2 3) (list 1 2 3)) (list 1 2 3))

; subset: (Listof Numbers) (Listof Numbers) -> Bool
; Purpose: Return true if lst1 is a subset of lst2
; Examples:
(check-expect (subset (list 1 2 4) (list 1 2 3 4)) #t)
(check-expect (subset (list 1 2 5) (list 1 2 3 4)) #f)

(define (subset lst1 lst2)
  (cond
    [(empty? lst1) #t]
    [(empty? lst2) #f]
    [(= (first lst1) (first lst2)) (subset (rest lst1) (rest lst2))]
    [(> (first lst1) (first lst2)) (subset lst1 (rest lst2))]
    [(< (first lst1) (first lst2)) (subset (rest lst1) lst2)]))

(check-expect (subset (list) (list 1 2)) #t)
(check-expect (subset (list 1 2) (list)) #f)
(check-expect (subset (list 2 4 6 8) (list 1 2 3 4 5 6 7 8 9)) #t)

; Exercise 21
; to-ordered: (Listof Numbers) -> (Listof Numbers)
; Purpose: return a copy of lst in numerical order
; Examples:
(check-expect (to-ordered (list 1 3 2)) (list 1 2 3))
(check-expect (to-ordered (list 3 2 1)) (list 1 2 3))

(define (to-ordered lst)
  (if (equal? lst (order-one lst))
      lst
      (to-ordered (order-one lst)))) ; have to call order-one twice since no let in this version

(check-expect (to-ordered (list 3 6 1 23 11 3)) (list 1 3 6 11 23))
(check-expect (to-ordered (list 3 6 1 23 11 -3)) (list -3 1 3 6 11 23))

; order-one: (Listof Numbers) -> (Listof Numbers)
; Purpose: Return a copy of lst with the first out-of-order number moved up
; Examples:
(check-expect (order-one (list 1 3 2)) (list 1 2 3))
(check-expect (order-one (list 3 2 1)) (list 2 3 1))

(define (order-one lst)
  (cond
    [(empty? lst) empty] ; only true if original list is empty
    [(empty? (rest lst)) lst] ; short-circuiting here allows lookahead
    [(< (second lst) (first lst)) (cons (second lst) (cons (first lst) (rest (rest lst))))]
    [(= (second lst) (first lst)) (cons (first lst) (order-one (rest (rest lst))))] ; remove duplicates
    [(> (second lst) (first lst)) (cons (first lst) (order-one (rest lst)))]))

(check-expect (order-one (list 2 3 1)) (list 2 1 3))
(check-expect (order-one (list 2 1 3)) (list 1 2 3))
(check-expect (order-one (list 1 2 3)) (list 1 2 3))
(check-expect (order-one (list 1 2 2 3)) (list 1 2 3))

; Exercise 23
; subsets: (Listof Any) -> (Listof Lists)
; Purpose: Generate all possible subsets of lst and return them in a list
; Examples:
(check-expect (subsets '(1)) '((1) ()))
(check-expect (subsets '(1 2 3)) (list (list 1 2 3) (list 1 2) (list 1 3)
                                       (list 1) (list 2 3) (list 2) (list 3) '()))

(define (subsets lst)
  (cond
    [(empty? lst) '(())]
    [else (append
           (add-to-each (subsets (rest lst)) (first lst))
           (subsets (rest lst)))]))

; add-to-each: (Listof Any) Any -> (Listof Any)
; Purpose: Add the element n to every list in a list of non-nested lists
; Examples
(check-expect (add-to-each '((1) (2) ()) 3) '((3 1) (3 2) (3)))

(define (add-to-each lst n)
  (cond
    [(empty? lst) '()]
    [(empty? (first lst)) (list (list n))]
    [(cons? (first lst)) (cons (list* n (first lst)) (add-to-each (rest lst) n))]
    [else (error lst)]))

; Exercise 24
; combs: (Listof Any) Number -> (Listof Any)
; Purpose: Return all subsets of lst that are of length n
; Examples:
(check-expect (combs '(1 2 3) 2) '((1 2) (1 3) (2 3)))

(define (combs lst n)
  (cond
    [(= n 0) '(())]
    [(empty? lst) '()]
    [else (append
           (add-to-each (combs (rest lst) (- n 1)) (first lst))
           (combs (rest lst) n))]))

; Exercise 25
; perms: (Listof Any) -> (Listof Lists)
; Purpose: Return a list of lists of every permutation of the elements of lst
; Examples:
(check-expect (perms '(1 2 3)) '((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1)))

(define (perms lst)
  (cond
    [(empty? lst) '(())]
    [(cons? lst) (add-at-every-to-each (perms (rest lst)) (first lst))]))
    
; add-at-every: (Listof Any) Any -> (Listof Any)
; Purpose: Return a list of lists with 
;Examples:
(check-expect (add-at-every '(1 2) 3) '((3 1 2) (1 3 2) (1 2 3)))

(define (add-at-every lst item)
  (cond
    [(empty? lst) (list (list item))]
    [(cons? lst) (append
                  (list (cons item lst))
                  (add-to-each (add-at-every (rest lst) item) (first lst)))]))

; add-at-every-to-each: (Listof Lists) Any -> (Listof Lists)
; Purpose: Apply (add-at-every sublist item) to every sublist in a list of lists
; Examples:
(check-expect (add-at-every-to-each '((1) (2)) 3) '((3 1) (1 3) (3 2) (2 3)))

(define (add-at-every-to-each lst item)
  (cond
    [(empty? lst) '()]
    [(cons? lst) (append
                  (add-at-every (first lst) item)
                  (add-at-every-to-each (rest lst) item))]))
                  












  