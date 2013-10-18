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

; for one kind of coin, linear space and time.
; for k kinds of coins, the time complexity is O(n^k).
; (think of each kind as having its own "dimension" of sorts)

; space is proportional to depth of tree (because we're only 
; in one place in the tree at a time, so only our direct
; ancestors are on the stack), which never exceeds 
; the all pennies case: hence linear.

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
    (cond ((= b 1) a)
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