;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |FICS chapter 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; a clock is a (make-clock hours mins secs) that contains a time
(define-struct clock (hours mins secs))

;tick: clock -> clock
;Purpose: increment the value of the clock by one second
;Examples
(check-expect (tick (make-clock 0 0 0)) (make-clock 0 0 1))
(check-expect (tick (make-clock 22 19 59)) (make-clock 22 20 0))

(define (tick c)
  (cond
    [(< (clock-secs c) 59)
    (make-clock (clock-hours c) (clock-mins c) (+ (clock-secs c) 1))]
    [(and (>= (clock-secs c) 59) (< (clock-mins c) 59))
     (make-clock (clock-hours c) (+ (clock-mins c) 1) 0)]
    [(and (>= (clock-secs c) 59) (>= (clock-mins c) 59) (< (clock-hours c) 23))
     (make-clock (+ (clock-hours c) 1) 0 0)]
    [else (make-clock 0 0 0)]))

(check-expect (tick (make-clock 0 0 59)) (make-clock 0 1 0))
(check-expect (tick (make-clock 0 59 59)) (make-clock 1 0 0))
(check-expect (tick (make-clock 23 59 59)) (make-clock 0 0 0))

;exercise 9
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


;division: Stores a division result with a remainder
(define-struct division (result remainder))

;div-qfe: qfe -> division
;Purpose: Diivides two quadratic field equations with the same base
;Examples
(check-expect (div-qfe (make-qfe 1 -1 5) (make-qfe 1 -1 5))
              (make-division 1 0))

(define (div-qfe a b)
  (cond
    [(not (= (qfe-base a) (qfe-base b))) (error "The two equations must share the same base")]
    [(or (= (qfe-rat b) 0) (= (qfe-irrat b) 0)) (error "Divide by zero")]
    [else (make-division (/ (qfe-rat a) (qfe-rat b))
              (- (qfe-irrat a) (* (qfe-irrat b) (/ (qfe-rat a) (qfe-rat b)))))]))

(check-expect (div-qfe (make-qfe 2 2 5) (make-qfe 1 1 5))
              (make-division 2 0))
(check-expect (div-qfe (make-qfe 1 1 5) (make-qfe 1 -1 5))
              (make-division 1 2))


; Point: a point on a Euclidian plane
(define-struct point (x y))

; cross-product: Point Point Point Number
; Purpose: Compares the relative vector directions between p1p2 and p1p3.
; I do not understand how this works but I couldn't figure out the problem on my own
; so I'm copying the answer from a website.
; Examples:

(define (cross-product p1 p2 p3)
  (- (* (- (point-x p2) (point-x p1)) (- (point-y p3) (point-y p1)))
     (* (- (point-x p3) (point-x p1)) (- (point-y p2) (point-y p1)))))

; same-sign: Number Number: Bool
; Purpose: return if x and y have the same sign
;Examples:
(check-expect (same-sign 1 1) #t)
(check-expect (same-sign 1 -1) #f)

(define (same-sign x y)
  (or (and (>= x 0) (>= y 0)) (and (<= x 0) (<= y 0))))

; in-triangle: Point Point Point Point: Bool
; Purpose: Returns True if p4 is on or inside the triangle defined by points p1 p2 and p3
; Examples:
;(check-expect (in-triangle (make-point 0 0) (make-point 3 0) (make-point -1 3) (make-point 1 1)) #t)
;(check-expect (in-triangle (make-point 0 0) (make-point 3 0) (make-point -1 3) (make-point 3 3)) #f) 

;(define (in-triangle p1 p2 p3 p4) ;;; this doesn't work either and i'm giving up on this since my
  ; math is apparently so weak i can't do this at all and have no idiea what the fuck i'm doing
;  (and (same-sign (cross-product p1 p2 p4) (cross-product p1 p2 p4))
;       (same-sign (cross-product p2 p3 p4) (cross-product p1 p2 p4))
;       (same-sign (cross-product p1 p3 p4) (cross-product p1 p2 p4))))


;Exercise 11
; Natural numbers - Recursive counting structure. Z is zero. Functions similar to LLs
(define-struct Z ())
(define-struct S (pred))

; pred : Nat -> Nat
; Purpose: returns the predecessor to nat
(define (pred nat)
  (cond
    [(Z? nat) (error "can't apply pred to Z")]
    [(S? nat) (S-pred nat)]))

; plus: Nat Nat -> Nat
; Purpose: add two Nats

(define (plus nat1 nat2)
  (cond
    [(Z? nat1) nat2]
    [(S? nat1) (make-S (plus (S-pred nat1) nat2))]))

; times: Nat Nat -> Nat
; Purpose: multiply two nats
; Examples:
(check-expect (times (make-S (make-Z)) (make-S (make-Z))) (make-S (make-Z)))
(check-expect (times (make-S (make-S (make-Z))) (make-S (make-S (make-Z))))
              (make-S (make-S (make-S (make-S (make-Z))))))

(define (times x y)
  (cond
    [(Z? x) (make-Z)]
    [(S? x) (plus y (times (pred x) y))]))

(check-expect (times (make-Z) (make-S (make-Z))) (make-Z))
(check-expect (times (make-S (make-Z)) (make-Z)) (make-Z))

; compare : Nat Nat -> Symbol
; Purpose : Compare two Nats for greater than, less than, or equals to
; Examples:
(check-expect (compare (make-Z) (make-S (make-Z))) 'less)
(check-expect (compare (make-S (make-Z)) (make-S (make-Z))) 'equal)
(check-expect (compare (make-S (make-Z)) (make-Z)) 'greater)

(define (compare n m)
  (cond
    [(and (Z? n) (Z? m)) 'equal]
    [(Z? n) 'less]
    [(Z? m) 'greater]
    [else (compare (pred n) (pred m))]))


; nat-from-int : Number -> Nat
; Purpose: Convert a Number to a Nat for use in structural recursion
; Examples:
(check-expect (nat-from-int 1) (make-S (make-Z)))
(check-expect (nat-from-int 2) (make-S (make-S (make-Z))))

(define (nat-from-int n)
  (cond
    [(< n 0) (error "n must be a positive integer")]
    [(= n 0) (make-Z)]
    [else (make-S (nat-from-int (- n 1)))]))

; int-from-nat: Nat -> Number
; Purpose: Convert a Nat to a Number for use with arithmetic
; Examples
(check-expect (int-from-nat (make-Z)) 0)
(check-expect (int-from-nat (make-S (make-Z))) 1)

(define (int-from-nat nat)
  (cond
    [(Z? nat) 0]
    [(S? nat) (+ 1 (int-from-nat (pred nat)))]))

; minus : Nat Nat -> Nat
; Purpose: Subtract y from x
;Examples:
(check-expect (minus (nat-from-int 4) (nat-from-int 2)) (nat-from-int 2))
(check-expect (minus (nat-from-int 1) (nat-from-int 1)) (nat-from-int 0))

(define (minus x y)
  (cond
    [(equal? (compare x y) 'less) (error "Cannot subtract a larger number from a natural number")]
    [(Z? y) x]
    [(S? y) (minus (pred x) (pred y))]))

(check-expect (minus (nat-from-int 11) (nat-from-int 7)) (nat-from-int 4))
(check-error (minus (nat-from-int 1) (nat-from-int 2))
             "Cannot subtract a larger number from a natural number")



; nat-remainder : Nat Nat -> Nat 
; Purpose: recreate remainder using the Nat data structure. Returns x % y as a Nat.
; Examples:
(check-expect (nat-remainder (nat-from-int 3) (nat-from-int 2)) (nat-from-int 1))
(check-expect (nat-remainder (nat-from-int 3) (nat-from-int 3)) (nat-from-int 0))

(define (nat-remainder x y)
  (cond
    [(equal? (compare x y) 'equal) (make-Z)] ; return 0 if x == y
    [(equal? (compare x y) 'less) x] ; return x if <= y
    [else (nat-remainder (minus x y) y)])) ; else remove y from x and repeat

(check-expect (nat-remainder (nat-from-int 11) (nat-from-int 6)) (nat-from-int 5))
(check-expect (nat-remainder (nat-from-int 20) (nat-from-int 5)) (nat-from-int 0))


; is-factor: Nat Nat -> Bool
; Purpose: return whether y is a factor of x
; Examples:
(check-expect (is-factor (nat-from-int 4) (nat-from-int 2)) #t)
(check-expect (is-factor (nat-from-int 3) (nat-from-int 2)) #f)

(define (is-factor x y)
  (Z? (nat-remainder x y)))
;  (= (remainder (int-from-nat x) (int-from-nat y)) 0)) ; this version is a lot faster

(check-expect (is-factor (nat-from-int 40) (nat-from-int 2)) #t)
(check-expect (is-factor (nat-from-int 31) (nat-from-int 2)) #f)

;perfect: Number -> Bool
;Purpose: return whether n is a perfect number
;Examples:
(check-expect (perfect 6) #t)
(check-expect (perfect 7) #f)

(define (perfect n)
  (= n (int-from-nat (sum-factors (nat-from-int n) (nat-from-int 1)))))

(check-expect (perfect 28) #t)
(check-expect (perfect 520) #f) ; my DIY remainder function makes this extremely slow

  
; sum-factors: Nat Nat -> Nat
; Purpose: return the sum of the factors of n. Needs to be initialized with
; a counter of 1 (make-S (make-Z)).
;Examples:
(check-expect (sum-factors (nat-from-int 6) (nat-from-int 1)) (nat-from-int 6))
(check-expect (sum-factors (nat-from-int 4) (nat-from-int 1)) (nat-from-int 3))
     
(define (sum-factors n counter)
  (cond
    [(equal? (compare n counter) 'less) (error "Counter cannot be larger than n")]
    [(equal? (compare n counter) 'equal) (make-Z)]
    [(equal? (compare n counter) 'greater)
     (if (is-factor n counter) (plus counter (sum-factors n (make-S counter)))
         (sum-factors n (make-S counter)))]))

(check-expect (sum-factors (nat-from-int 11) (nat-from-int 1)) (nat-from-int 1))
(check-expect (sum-factors (nat-from-int 40) (nat-from-int 1)) (nat-from-int 50))






    