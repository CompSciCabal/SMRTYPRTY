;; ----------------------------------------------
;; exercise 1.9
(define (+ a b)
	(if (= a 0)
		b
		(inc (+ (dec a) b))))

;; recursive

(define (+ a b)
	(if (= a 0)
		b
		(+ (dec a) (inc b))))

;; iterative

;; ----------------------------------------------
;; exercise 1.10
(define (A x y)
	(cond ((= y 0) 0)
		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1)
			  (A x (- y 1))))))

; (A 1 10) => 1024
; (A 2 4) => 65536
; (A 3 3) => 65536

(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2^n
(define (h n) (A 2 n)) ; (2^n)^n
(define (k n) (* 5 n n)) ; 5n^2

;; ----------------------------------------------
;; exercise 1.11
;; A function f is defined by the rule that f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n -3) if n> 3. 
;; Write a procedure that computes f by means of a recursive process. 
;; Write a procedure that computes f by means of an iterative process.

;; recursive
(define (f n)
	(if (< n 3) 
		n
		(+ (f (- n 1)) 
		   (* 2 (f (- n 2))) 
		   (* 3 (f (- n 3))))))

;; iterative
(define (f n)
	(if (< n 3)
        n
        (f-iter 2 1 0 n)))

(define (f-iter x y z n)
      (if (= n 2)
          x
          (f-iter 
          	(+ x (* 2 y) 
          		 (* 3 z)) 
          	x 
          	y 
          	(- n 1))))

;; ----------------------------------------------
;; exercise 1.12
(define (pascals x n)
	(if (or (= n 1) 
			(= x n))
		1
		(+ (pascal (dec x) n)
		   (pascal (dec x) (dec n)))))

;; ----------------------------------------------
;; exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
	(if (not (> (abs angle) 0.1))
		angle
		(p (sine (/ angle 3.0)))))

;; a) 5
;; b) space: log a (unsure?)
;; b) steps: also log a (unsure?)

;; ----------------------------------------------
;; exercise 1.16
(define (expn x n)
	(define (expn-iter i x n)
		(cond
			((= n 0) i)
			((even? n) (expn-iter a (* x x) (/ n 2))
			(else (expn-iter (* a x) x (dec n))))

	(expn-iter 1 x n))

;; ----------------------------------------------
;; exercise 1.17
(define (double x)
	(* 2 x))

(define (halve x)
	(/ x 2))

(define (multi x y)  
	(define (multi-iter x y i)
		(cond
			((= y 0) i)
			((even? y) (multi-iter (double x) (halve y) i))
			(else (multi-iter x (dec y) (+ x i))))))


;; ----------------------------------------------
;; exercise 1.18

;; see 1.17?