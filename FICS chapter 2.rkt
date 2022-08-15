;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |FICS chapter 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; middle-of-three: Number Number Number -> Number
; Purpose: if one of the numbers is between
; the other two, return it
; Examples:
(check-expect (middle-of-three 3 1 2) 2)
(check-expect (middle-of-three 4 2 6) 4)

(define (middle-of-three x y z)
  (cond
    [[or [and (>= x y) (<= x z)] [and (>= x z) (<= x y)]] x]
    [[or [and (>= y x) (<= y z)] [and (>= y z) (<= y x)]] y]
    [[or [and (>= z x) (<= z y)] [and (>= z y) (<= z x)]] z]))

(check-expect (middle-of-three 1 1 1) 1)
(check-expect (middle-of-three 1 1 2) 1)
(check-expect (middle-of-three 1 2 2) 2)

; interval-proper-subset?: Number Number Number Number: Boolean
; Purpose: returns true if the interval [a, b]
; is a proper subset of the interval [x, y],
; otherwise returns false
; Examples
(check-expect (interval-proper-subset? 4.5 5.5 2 6) #true)
(check-expect (interval-proper-subset? 2 6 4.5 5.5) #false)

(define (interval-proper-subset? a b x y)
  (if (and (> a x) (< b y)) #true #false))

(check-expect (interval-proper-subset? 2 4 2 6) #false)
(check-expect (interval-proper-subset? 3 6 2 6) #false)
(check-expect (interval-proper-subset? -1 1 -2 2) #true)
