#lang htdp/isl+
; Binary Natural Number implementation
(define-struct Z ())
(define-struct A (way))
(define-struct B (gone))

; Conversion to and from Racket Numbers
(define (from-Nat nat)
  (cond
    [(Z? nat) 0]
    [(A? nat) (* 2 (from-Nat (A-way nat)))]
    [(B? nat) (add1 (* 2 (from-Nat (B-gone nat))))]))

(define (to-Nat k)
  (cond
    [(zero? k) (make-Z)]
    [(even? k) (make-A (to-Nat (quotient k 2)))]
    [(odd? k)  (make-B (to-Nat (quotient k 2)))]))

; Simple addition of Binary Nat
; (define (plus nat1 nat2)
;   (cond
;     [(Z? nat1) nat2]
;     [(Z? nat2) nat1]
;     [(and (A? nat1) (A? nat2))
;        (make-A (plus (A-way nat1) (A-way nat2)))]
;     [(and (A? nat1) (B? nat2))
;        (make-B (plus (A-way nat1) (B-gone nat2)))]
;     [(and (B? nat1) (A? nat2))
;        (make-B (plus (B-gone nat1) (A-way nat2)))]
;     [(and (B? nat1) (B? nat2))
;        (make-A (plus-one (plus (B-gone nat1) (B-gone nat2))))]))

; helper for plus 
; (define (plus-one nat)
;   (cond
;     [(Z? nat) (make-B (make-Z))]
;     [(A? nat) (make-B (A-way nat))]
;     [(B? nat) (make-A (plus-one (B-gone nat)))]))

; addition tables - describes what plus is doing
; + 0 1     + A B
; 0 0 1     A A B
; 1 1 10    B B A+1

; measure size complexity of a Nat
(define (size nat)
  (cond
    [(Z? nat) 1]
    [(A? nat) (add1 (size (A-way nat)))]
    [(B? nat) (add1 (size (B-gone nat)))]))

; mutually recursive implementation of Nat addition
; (define (plus nat1 nat2)
;   (cond
;     [(Z? nat1) nat2]
;     [(Z? nat2) nat1]
;     [(and (A? nat1) (A? nat2))
;        (make-A (plus (A-way nat1) (A-way nat2)))]
;     [(and (A? nat1) (B? nat2))
;        (make-B (plus (A-way nat1) (B-gone nat2)))]
;     [(and (B? nat1) (A? nat2))
;        (make-B (plus (B-gone nat1) (A-way nat2)))]
;     [(and (B? nat1) (B? nat2))
;         (make-A (plus-s (B-gone nat1) (B-gone nat2)))]))

; ; helper for plus - note shuffling plus-one to the base case and reversing the make calls
; (define (plus-s nat1 nat2)
;   (cond
;     [(Z? nat1) (plus-one nat2)]
;     [(Z? nat2) (plus-one nat1)]
;     [(and (A? nat1) (A? nat2))
;        (make-B (plus (A-way nat1) (A-way nat2)))]
;     [(and (A? nat1) (B? nat2))
;        (make-A (plus-s (A-way nat1) (B-gone nat2)))]
;     [(and (B? nat1) (A? nat2))
;        (make-A (plus-s (B-gone nat1) (A-way nat2)))]
;     [(and (B? nat1) (B? nat2))
;        (make-B (plus-s (B-gone nat1) (B-gone nat2)))]))

;;;;;;;;;;;; moving on from natural numbers to integers

; Z is still 0
; (define-struct Z ())

; N is -1
(define-struct N ())

; smart constructor for A - won't apply A to Z
(define (sA int)
  (cond
    [(Z? int) int]
    [else (make-A int)]))

; smart constructor for B - won't apply B to N
(define (sB int)
  (cond
    [(N? int) int]
    [else (make-B int)]))

(define (plus int1 int2)
  (cond
    [(Z? int1) int2]
    [(Z? int2) int1]
    [(and (N? int1) (N? int2)) (sA (make-N))]
    [(and (B? int1) (N? int2)) (sA (B-gone int1))]
    [(and (N? int1) (B? int2)) (sA (B-gone int2))]
    [(and (A? int1) (N? int2)) (sB (plus (A-way int1) (make-N)))]
    [(and (N? int1) (A? int2)) (sB (plus (make-N) (A-way int2)))]
    [(and (A? int1) (A? int2))
     (sA (plus (A-way int1) (A-way int2)))]
    [(and (A? int1) (B? int2))
     (sB (plus (A-way int1) (B-gone int2)))]
    [(and (B? int1) (A? int2))
     (sB (plus (B-gone int1) (A-way int2)))]
    [(and (B? int1) (B? int2))
     (sA (plus-one (plus (B-gone int1) (B-gone int2))))]))
 
(define (plus-one int)
  (cond
    [(Z? int) (sB (make-Z))]
    [(N? int) (make-Z)]
    [(A? int) (sB (A-way int))]
    [(B? int) (sA (plus-one (B-gone int)))]))

; subtraction and negation
(define (subt int1 int2) (plus int1 (negate int2)))
(define (negate int) (plus-one (flip int)))
 
; simple sign flip (converts N to Z, Z to N, A to B, and B to A)
; note Z is 0 and N is -1, hence the call to plus-one in negate
(define (flip int)
  (cond
    [(N? int) (make-Z)]
    [(Z? int) (make-N)]
    [(A? int) (make-B (flip (A-way int)))]
    [(B? int) (make-A (flip (B-gone int)))]))

; basic exercise (my own) : implement conversion functions to and from signed int structure

; to-int: Number -> Struct
; Purpose: Convert Racket natural numbers to signed int structure
; Examples:
(check-expect (to-int -5) (sB (sB (sA (make-N)))))
(check-expect (to-int 5) (sB (sA (sB (make-Z)))))

(define (to-int n)
  (cond
    [(zero? n) (make-Z)]
    [(= n -1) (make-N)]
    [(and (even? n) (> n 0)) (sA (to-int (quotient n 2)))]
    [(and (odd? n) (> n 0)) (sB (to-int (quotient n 2)))]
    [(and (even? n) (< n 0)) (sA (to-int (quotient (sub1 n) 2)))]
    [(and (odd? n) (< n 0)) (sB (to-int (quotient (sub1 n) 2)))]))

; from-int: Struct -> Number
; Purpose: Convert signed int structure to Racket numbers
; Examples:
(check-expect (from-int (sB (sB (sA (make-N))))) -5)
(check-expect (from-int (sB (sA (sB (make-Z))))) 5)

(define (from-int int)
  (cond
    [(Z? int) 0]
    [(N? int) -1]
    [(A? int) (* 2 (from-int (A-way int)))]
    [(B? int) (add1 (* 2 (from-int (B-gone int))))]))
    
; Exercise 35

; from-slist: (Listof Symbols) -> Number
; Purpose: Convert slist form of natural numbers to Racket numbers
; Examples:
(check-expect (from-slist '(i o i)) 5)
(check-expect (from-slist '(o i o i)) 10)

(define (from-slist s)
 (cond
   [(null? s) 0]
   [(symbol=? (car s) 'o) (* 2 (from-slist (cdr s)))]
   [(symbol=? (car s) 'i) (add1 (* 2 (from-slist (cdr s))))]))

; to-slist: Number -> (Listof Symbols)
; Purpose: Convert Racket natural number to slist
; Examples:
(check-expect (to-slist 5) '(i o i))
(check-expect (to-slist 10) '(o i o i))

(define (to-slist n)
  (cond
    [(zero? n) empty]
    [(even? n) (cons 'o (to-slist (quotient n 2)))]
    [(odd? n) (cons 'i (to-slist (quotient n 2)))]))

; add-s: (Listof Symbols) (Listof Symbols) -> (Listof Symbols)
; Purpose: Add two natural number s-lists
; Examples:
(check-expect (from-slist (add-s (to-slist 5) (to-slist 4))) 9)
(check-expect (from-slist (add-s (to-slist 6) (to-slist 42))) 48)

(define (add-s s1 s2)
  (cond
    [(empty? s1) s2]
    [(empty? s2) s1]
    [(and (symbol=? 'o (car s1)) (symbol=? 'o (car s2)))
     (cons 'o (add-s (cdr s1) (cdr s2)))]
    [(and (symbol=? 'i (car s1)) (symbol=? 'o (car s2)))
     (cons 'i (add-s (cdr s1) (cdr s2)))]
    [(and (symbol=? 'o (car s1)) (symbol=? 'i (car s2)))
     (cons 'i (add-s (cdr s1) (cdr s2)))]
    [(and (symbol=? 'i (car s1)) (symbol=? 'i (car s1)))
     (cons 'o (plus-one-s (add-s (cdr s1) (cdr s2))))]))

; plus-one-s: (Listof Symbols) -> (Listof Symbols)
; Purpose: Increase the value of a natural-number s-list by one. Helper for add-s
; Examples:
(check-expect (plus-one-s '(i o i)) '(o i i))
(check-expect (plus-one-s '(i i i)) '(o o o i))
(define (plus-one-s s)
  (cond
    [(empty? s) (cons 'i empty)]
    [(symbol=? 'o (car s)) (cons 'i (cdr s))]
    [(symbol=? 'i (car s)) (cons 'o (plus-one-s (cdr s)))]))
    
; multiplication table for binary
; * 0 1
; 0 0 0
; 1 0 1 

; multiply slists
; if s1 is empty:
;   return 0
; if car s1 is 'i:
;   add s2 to recursive call on (cdr s1) (cons 'o s2) (???)
; if car s1 is 'o:
;   recursive call (add 0) on (cdr s1) (cons 'o s2) (???)
; ???* - need to increase s2 by an order of magnitude for each digit of s1
;         but I'm not 100% just calling adding an 'o on it is right?? 
;         not sure what else it would be tho

; mult-s: (Listof Symbols) (Listof Symbols) -> (Listof Symbols)
; Purpose: Multiply two natural number s-lists
; Examples: 
(check-expect (from-slist (mult-s (to-slist 4) (to-slist 5))) 20)
(check-expect (from-slist (mult-s (to-slist 11) (to-slist 5))) 55)

(define (mult-s s1 s2)
  (cond
    [(empty? s1) empty]
    [(symbol=? (car s1) 'i) (add-s s2 (mult-s (cdr s1) (cons 'o s2)))]
    [(symbol=? (car s1) 'o) (mult-s (cdr s1) (cons 'o s2))]))

; Problem 36

; to-s-int: Number -> (Listof Symbols)
; Purpose: Convert a Racket number to a list representation of a signed binary integer
; Examples:
(check-expect (to-s-int -5) '(i i o n))
(check-expect (to-s-int 5) '(i o i z))

(define (to-s-int n)
  (cond 
    [(zero? n) '(z)]
    [(= -1 n) '(n)]
    [(> n 0)  (if (even? n)
                (cons 'o (to-s-int (quotient n 2)))
                (cons 'i (to-s-int (quotient n 2))))]
    [(< n 0)  (if (even? n)
                (cons 'o (to-s-int (quotient (sub1 n) 2))) 
                (cons 'i (to-s-int (quotient (sub1 n) 2))))]))

; from-s-int: (Listof Symbols) -> Number
; Purpose: Convert a list representation of a signed binary integer into a Racket number
; Examples:
(check-expect (from-s-int '(i i o n)) -5)
(check-expect (from-s-int '(i o i z)) 5)

(define (from-s-int s)
  (cond
    [(symbol=? (car s) 'z) 0]
    [(symbol=? (car s) 'n) -1]
    [(symbol=? (car s) 'o) (* 2 (from-s-int (cdr s)))]
    [(symbol=? (car s) 'i) (add1 (* 2 (from-s-int (cdr s))))]))

; add-s-int: (Listof Symbols) (Listof Symbols) -> (Listof Symbols)
; Purpose: Add two signed integer s-lists
; Examples:
(check-expect (from-s-int (add-s-int (to-s-int 5) (to-s-int 4))) 9)
(check-expect (from-s-int (add-s-int (to-s-int 5) (to-s-int -4))) 1)

(define (add-s-int s1 s2)
  (cond
    [(symbol=? (car s1) 'z) s2]
    [(symbol=? (car s2) 'z) s1]
    [(symbol=? (car s1) 'n) (cond
      [(symbol=? (car s2) 'n) '(o n)]
      [(symbol=? (car s2) 'o) (cons 'i (add-s-int '(n) (cdr s2)))]
      [(symbol=? (car s2) 'i) (cons 'o (cdr s2))])]
    [(symbol=? (car s1) 'o) (cond
      [(symbol=? (car s2) 'n) (cons 'i (add-s-int '(n) (cdr s1)))]
      [(symbol=? (car s2) 'o) (cons 'o (add-s-int (cdr s1) (cdr s2)))]
      [(symbol=? (car s2) 'i) (cons 'i (add-s-int (cdr s1) (cdr s2)))])]
    [(symbol=? (car s1) 'i) (cond
      [(symbol=? (car s2) 'n) (cons 'o (cdr s1))]
      [(symbol=? (car s2) 'o) (cons 'i (add-s-int (cdr s1) (cdr s2)))]
      [(symbol=? (car s2) 'i) (cons 'o (add-one-s-int (add-s-int (cdr s1) (cdr s2))))])]))

; add-one-s-int: (Listof Symbols) -> (Listof Symbols)
; Purpose: Add one to a signed integer s-list
; Examples:
(check-expect (from-s-int (add-one-s-int (to-s-int -5))) -4)
(check-expect (from-s-int (add-one-s-int (to-s-int 5))) 6)

(define (add-one-s-int s)
  (cond
    [(symbol=? (car s) 'z) '(i z)]
    [(symbol=? (car s) 'n) '(z)]
    [(symbol=? (car s) 'o) (cons 'i (cdr s))]
    [(symbol=? (car s) 'i) (cons 'o (add-one-s-int (cdr s)))]))

; sub-s: (Listof Symbols) (Listof Symbols) -> (Listof Symbols)
; Purpose: Subtract signed integer s-lists
; Examples:
(check-expect (from-s-int (sub-s-int (to-s-int 5) (to-s-int 4))) 1)
(check-expect (from-s-int (sub-s-int (to-s-int 4) (to-s-int 5))) -1)

(define (sub-s-int s1 s2) (add-s-int s1 (neg-s-int s2)))

; neg-s-int: (Listof Symbols) -> (Listof Symbols)
; Purpose: negate a signed integer s-list
; Examples
(check-expect (from-s-int (neg-s-int (to-s-int 5))) -5)
(check-expect (from-s-int (neg-s-int (to-s-int -5))) 5)

(define (neg-s-int s) (add-one-s-int (flip-s-int s)))

; flip-s-int: (Listof Symbols) -> (Listof Symbols)
; Purpose: Flip all bits of a signed int s-list
(define (flip-s-int s)
  (cond
    [(symbol=? (car s) 'z) '(n)]
    [(symbol=? (car s) 'n) '(z)]
    [(symbol=? (car s) 'o) (cons 'i (flip-s-int (cdr s)))]
    [(symbol=? (car s) 'i) (cons 'o (flip-s-int (cdr s)))]))

; mul-s-int: (Listof Symbols) (Listof Symbols) -> (Listof Symbols)
; Purpose: Multiply two signed int s-lists
(check-expect (from-s-int (mul-s-int (to-s-int -4) (to-s-int 5))) -20)
(check-expect (from-s-int (mul-s-int (to-s-int -11) (to-s-int -5))) 55)

(define (mul-s-int s1 s2)
  (if (neg-s-int? s1) ; cheated and looked this up
    (mul-s-int (neg-s-int s1) (neg-s-int s2))
    (cond
      [(symbol=? (car s1) 'z) '(z)]
      [(symbol=? (car s1) 'i) (add-s-int s2 (mul-s-int (cdr s1) (cons 'o s2)))]
      [(symbol=? (car s1) 'o) (mul-s-int (cdr s1) (cons 'o s2))])))

; neg-s-int?: (Listof Symbols) -> Boolean
; Purpose: Return if an s-int is negative. Helper for mul-s-int
; Examples:
(check-expect (neg-s-int? (to-s-int -5)) #t)
(check-expect (neg-s-int? (to-s-int 5)) #f)

(define (neg-s-int? s)
  (cond
    [(symbol=? (car s) 'z) #f]
    [(symbol=? (car s) 'n) #t]
    [else (neg-s-int? (cdr s))]))


; CHAPTER 4 exercise 9 - imported to use with exercise 37
;a QFE is a (make-qfe rat irrat base) representing a quadratic field equation
(define-struct qfe (rat irrat base))

;add-qfe: qfe -> qfe
;Purpose: Adds two quadratic field equations with the same base
;Examples
(check-expect (add-qfe (make-qfe 1 -1 5) (make-qfe 1 -1 5))
              (make-qfe 2 -2 5))

(define (add-qfe a b)
  (if
    (not (= (qfe-base a) (qfe-base b))) (error "The two equations must share the same base")
    (make-qfe (+ (qfe-rat a) (qfe-rat b)) (+ (qfe-irrat a) (qfe-irrat b)) (qfe-base a))))

(check-expect (add-qfe (make-qfe -1 -1 5) (make-qfe 1 -1 5))
              (make-qfe 0 -2 5))


;sub-qfe: qfe -> qfe
;Purpose: Subtracts two quadratic field equations with the same base
;Examples
(check-expect (sub-qfe (make-qfe 3 -1 5) (make-qfe 1 1 5))
              (make-qfe 2 -2 5))

(define (sub-qfe a b)
  (if
    (not (= (qfe-base a) (qfe-base b))) (error "The two equations must share the same base")
    (make-qfe (- (qfe-rat a) (qfe-rat b)) (- (qfe-irrat a) (qfe-irrat b)) (qfe-base a))))

(check-expect (sub-qfe (make-qfe -3 -1 5) (make-qfe 1 -1 5))
              (make-qfe -4 0 5))


;mul-qfe: qfe -> qfe
;Purpose: Multiplies two quadratic field equations with the same base
;Examples
(check-expect (mul-qfe (make-qfe 3 -1 5) (make-qfe 1 1 5))
              (make-qfe -2 2 5))

(define (mul-qfe a b)
  (if
    (not (= (qfe-base a) (qfe-base b))) (error "The two equations must share the same base")
    (make-qfe
     (+ (* (qfe-rat a) (qfe-rat b)) (* (qfe-base a) (qfe-irrat a) (qfe-irrat b)))
     (+ (* (qfe-rat a) (qfe-irrat b)) (* (qfe-rat b) (qfe-irrat a)))
     (qfe-base a))))

(check-expect (mul-qfe (make-qfe 1 -1 5) (make-qfe 1 -1 5)) (make-qfe 6 -2 5))

;div-qfe: qfe -> qfe
;Purpose: Diivides two quadratic field equations with the same base
;Examples
(check-expect (div-qfe (make-qfe 1 -1 5) (make-qfe 1 -1 5))
              (make-qfe 1 0 5))

(define (div-qfe a b)
  (cond
    [(not (= (qfe-base a) (qfe-base b))) 
      (error "The two equations must share the same base")]
    [(and (zero? (qfe-rat b)) (zero? (qfe-irrat b))) (error "Divide by zero")]
    [(zero? (qfe-rat b)) 
      (make-qfe  (/ (qfe-irrat a) (qfe-irrat b)) (/ (qfe-rat a) (qfe-irrat b)) (qfe-base b))]
    [(zero? (qfe-irrat b)) 
      (make-qfe (/ (qfe-rat a) (qfe-rat b)) (/ (qfe-irrat a) (qfe-rat b)) (qfe-base b))]
    [else (error "Cannot simplify")]))

(check-expect (div-qfe (make-qfe 2 2 5) (make-qfe 1 1 5))
              (make-qfe 2 2 5))
(check-expect (div-qfe (make-qfe 1 1 5) (make-qfe 3 -1 5))
              (make-qfe 1/3 -1 5))

; Exercise 37

; exp-qfe: qfe Number -> qfe
; Purpose: Raise a quadratic field equation to the power k
; Examples:
(check-expect (exp-qfe (make-qfe 1 -1 5) 2) (make-qfe 6 -2 5))
(check-expect (exp-qfe (make-qfe 1 -1 5) 3) (make-qfe 16 -8 5))

(define (exp-qfe x k)
  (cond
    [(< k 0)    (error "Negative QFE exponents not available")]
    [(zero? k)  (make-qfe 1 0 (qfe-base x))]
    [(= k 2)    (mul-qfe x x)]
    [(even? k)  (exp-qfe (exp-qfe x (quotient k 2)) 2)]
    [(odd? k)   (mul-qfe x (exp-qfe x (- k 1)))]))


; theta: qfe
; Purpose: Constant theta for use in fib-closed
(define theta (div-qfe (make-qfe 1 1 5) (make-qfe 2 0 5)))
; one: qfe
(define one5 (make-qfe 1 0 5))
; root5: qfe
(define root5 (make-qfe 0 1 5))

; fib-closed: Number -> Number
; Purpose: Solve Fib for n using the closed form equation and QFE manipulation
; Examples:
(check-expect (fib-closed 1) 0)
(check-expect (fib-closed 10) 55)

(define (fib-closed n)
  (qfe-rat
    (div-qfe (sub-qfe (exp-qfe theta n) (exp-qfe (sub-qfe one5 theta) n)) root5)))