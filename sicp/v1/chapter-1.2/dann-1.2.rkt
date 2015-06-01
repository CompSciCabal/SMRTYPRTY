#lang racket


;;; SICP CHAPTER 1.2

;;; SICP EX 1.9

; A is recursive because you have to retain the inc's
; B is iterative because each call contains all needed knowledge

;;; SICP EX 1.10 

(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y)) 
        ((= y 1) 2) 
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; power tower of 2s n high

;;; ;;;; also fib dim

;;; EX 1.11

(define (f-rec n)
    (if (< n 3) 
        n
        (+ (f-rec (- n 1)) 
           (* 2 (f-rec (- n 2))) 
           (* 3 (f-rec (- n 3))))))

(define (ff n)
    (define (f-iter a b c count)
      (if (= count 2)
          a
          (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
    (if (< n 3)
        n
        (f-iter 2 1 0 n)))


;;; EX 1.12

(define (pascal row col)
    (cond ((= row 1) 1)
          ((= col 1) 1)
          ((= row col) 1)
          ((> col row) 0)
          (else (+ (pascal (- row 1) (- col 1)) 
                   (pascal (- row 1) col)))))

;;; EX 1.13

; prove base cases 0 and 1
; phi squared is one plus phi
; psi squared is one minus psi
; some algebra
; take the limit


;;; EX 1.14

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (= kinds-of-coins 0)) 0) 
        (else (+ (cc amount
                     (- kinds-of-coins 1)) 
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1) 
        ((= kinds-of-coins 2) 5) 
        ((= kinds-of-coins 3) 10) 
        ((= kinds-of-coins 4) 25) 
        ((= kinds-of-coins 5) 50)))

; for one kind of coin, linear space and time.
; for k kinds of coins, the time complexity is O(n^k).
; (think of each kind as having its own "dimension" of sorts)

; space is proportional to depth of tree (because we're only 
; in one place in the tree at a time, so only our direct
; ancestors are on the stack), which never exceeds 
; the all pennies case: hence linear.

; (time (count-change 100))
; (time (count-change 1000))

;;; EX 1.15

(/ 12.15 3 3 3 3 3)
; order of log base 3 of a in time and space
; (linear recursion instead of tree, so time ~= space)

;;; EX 1.16

(define (fast-expt b n)
    (define (square n) 
      (* n n))
    (define (even? n) 
      (= (remainder n 2) 0))
    (define (fe-iter b n a)
      (cond ((= n 0) a)
            ((even? n) (fe-iter (square b) (/ n 2) a))
            (else (fe-iter b (- n 1) (* a b)))))
    (fe-iter b n 1))


;;; EX 1.17

(define (double a)
  (+ a a))
(define (halve a)
  (/ a 2))
(define (even? n) 
  (= (remainder n 2) 0))

(define (mul a b)
    (cond ((= b 0) a)
          ((even? b) (double (mul a (halve b))))
          (else (+ a (mul a (- b 1))))))

;;; EX 1.18

(define (muli a b)
    (define (mul-iter a b n)
      (cond ((= b 0) n)
            ((even? b) (mul-iter (double a) (halve b) n))
            (else (mul-iter a (- b 1) (+ a n)))))
    (mul-iter a b 0))

;;; EX 1.19

(define (fib n) 
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count) 
  (cond ((= count 0) b)
        ((even? count) 
         (fib-iter a 
                   b
                   (+ (* q q) (* p p)) 
                   (+ (* q q) (* p q 2))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                        (+ (* b p) (* a q))
                        p 
                        q 
                        (- count 1)))))

;;; EX 1.20

; GCD(206, 40) 
; (gcd 40 (rem 206 40))
; (gcd (rem 206 40) (rem 40 (rem 206 40)))
;      ^ eval this for the if
; yada yada 18 times vs 4 for applicative order

;;; EX 1.21

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))

; commented out for 1.23 (uncomment for 1.21)
;(define (find-divisor n test-divisor) 
;  (cond ((> (square test-divisor) n) n) 
;        ((divides? test-divisor n) test-divisor) 
;        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

; (smallest-divisor 199) ;199
; (smallest-divisor 1999) ; 1999
; (smallest-divisor 19999) ; 7


;;; EX 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n) 
  (newline) 
  (display n) 
  (start-prime-test n (current-inexact-milliseconds)))

; commented out for 1.24
;(define (start-prime-test n start-time) 
;  (if (prime? n)
;      (report-prime (- (current-inexact-milliseconds) start-time))
;      0)) 

(define (report-prime elapsed-time)
  (display " *** ") 
  (display elapsed-time))

(define (search-for-primes start stop)
    (define (sfp-rec start stop current)
      (timed-prime-test current)
      (if (< stop current)
          123
          (sfp-rec start stop (+ current 2))))
    (sfp-rec start stop start))

; (search-for-primes 1000001 1000050)

;     1 000: 0.01
;    10 000: 0.03
;   100 000: 0.09
; 1 000 000: 0.27

; --> timing supports both hypotheses


;;; EX 1.23

(define (next n)
    (if (= n 2)
        3
        (+ n 2)))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (next test-divisor)))))

;   100 000: 0.05
; 1 000 000: 0.15

; about twice as fast... but limited resolution



;;; EX 1.24

(define (expmod base exp m) 
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder
          (square (expmod base (/ exp 2) m))
          m)) 
        (else
         (remainder 
          (* base (expmod base (- exp 1) m)) 
          m))))

(define (fermat-test n) 
  (define (try-it a)
    (= (expmod a n n) a)) 
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1))) 
        (else false)))

(define (start-prime-test n start-time) 
  (if (fast-prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      0)) 

;     1 000: 0.006
; 1 000 000: 0.15

; you'd expect ~27 times more, which is what we found


;;; EX 1.25

; by taking the remainder each time we reduce the amount of work


;;; EX 1.26

; square takes a single argument and doubles it
; Louis's code does two exps instead of one, which trees out
; instead of staying linear -- so O(n) instead of O(log n)


;;; EX 1.27

(define (carm-test n)
    (define (rec n a)
      (if (= a n)
          #t
          (if (= (remainder (expt a n) n) a)
              (rec n (+ a 1))
              #f)))
    (rec n 1))

; yep, the Carmichael numbers do pass this test (and fail the other)


;;; EX 1.28

