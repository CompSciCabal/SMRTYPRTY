;;;; exercises from section 1.3

;;; Basic Functions

(define (inc i)
  (+ 1 i))
(define (identity x) x)
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
(sum identity 1 inc 10)
(sum square 1 inc 10)
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
;;; 1.29 Simpson's Rule: Integral of f from a to b

;;; original Integral example

(integral cube 0 1 0.01)
; result 0.2499875

(integral cube 0 1 0.001)
; result 0.249999875

;;; Simpson's Rule

(define (integral-simpson f a b n)
  (define (y k)
    (f (+ a (* k h))))
  (define (v k)
    (if (= (remainder k 2) 0)
      (* 2 (y k))
      (* 4 (y k))))
  (define h
    (/ (- b a) n))
  (*
    (/ h 3)
    (+ (y 0) (sum v 1 inc (- n 1)) (y n))))
(integral-simpson cube 0 1 10)
; result 0.25

(integral-simpson cube 0 1 100)
; result 0.25

(integral-simpson cube 0 1 1000)
; result 0.25

; Simpsons seems exact for this example.  

; Wikipedia says this should be h^4 so 10 should only give 10^-4 

; im suspecting if you do the math maybe its better than that for low order polynomials.

;;; 1.30 Iterative sum implementation

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
(sum identity 1 inc 10)
;;; 1.32 a

;;; Implement product, factorial and approximate pi using the formula.

;;; pi/4 = (2*4*4*6*6*8...)/(3*3*5*5*7...)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))
;;; this is a factorial

(product identity 1 inc 4)
(define (factorial n)
  (product identity 1 inc n))
(factorial 4)
;;; as an aside, i wanted to avoid dividing 2 big numbers, so i tried to use rational numbers

;;; first i found the cons type which is a dotted pair

(define pair (cons 2 3))
(/ (car pair) (cdr pair))
;;; then i saw you could implement the pair using functions. 

;;; ive done it by copying this is still a bit confusing to me.

(define (rational-number p q)
  (lambda (f)
    (f p q)))
(rational-number 1 2)
(define (numerator r)
  (r (lambda (p q)
    p)))
(numerator (rational-number 3 5))
(define (denominator r)
  (r (lambda (p q)
    q)))
(denominator (rational-number 3 5))
;;; but now syntax highlighting shows that lisp already supports rational numbers :)

;;; oh well ill use mine for now

(define (pi-wallis n)
  (define a
    (rational-number 2 3))
  (define (term a)
    (/ (numerator a) (denominator a)))
  (define (next a)
    (define p (numerator a))
    (define q (denominator a))
    (if (< p q)
      (rational-number (+ p 2) q)
      (rational-number p (+ q 2))))
  (define (iter a r i)
    (if (> i n)
      r
      (iter (next a) (* r (term a)) (+ i 1))))
  (* 4 (iter a 1 1))
  ;; (product term a next b).
  
  ;; how to compare rational numbers?  i.e can i add an overload for less than?
  
  ;; that is the only stumbling block to using the generic product
  )
(pi-wallis 1)
(pi-wallis 2)
(pi-wallis 3)
(pi-wallis 50)
(pi-wallis 500)
(pi-wallis 5000)
;; Seems like it is very slowly converging... 5000 only gave 4 digits

;;; 1.31b recursive product

(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a) (product-rec term (next a) next b))))
(product-rec identity 1 inc 6)
;;; 1.32 generic accumulate

;;; a) recursive

;;; b) iterative

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate-rec combiner null-value term (next a) next b))))
(accumulate-rec + 0 identity 1 inc 10)
(define (accumulate combiner null-value term a next b)
  (define (iter a r)
    (if (<= a b)
      (iter (next a) (combiner (term a) r))
      r))
  (iter a null-value))
(accumulate * 1 identity 1 inc 6)
(define (product term a next b)
  (accumulate * 1 term a next b))
(product identity 1 inc 6)
(define (sum term a next b)
  (accumulate + 0 term a next b))
(sum identity 1 inc 10)
;;; 1.33

(define (filtered-accumulate combiner null-value pred term a next b)
  (define (iter a r)
    (cond
      ((> a b) r)
      ((pred a) (iter (next a) (combiner (term a) r)))
      (else (iter (next a) r))))
  (iter a null-value))
(filtered-accumulate + 0 even? identity 1 inc 10)
;;; a) Finding primes, copying vlads prime finder (also in the text)

; Deterministic primality test

; Time complexity: Θ(√n)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x)
  (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (define (next td)
    (+ td 1))
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))
;;; prime squares sum

(filtered-accumulate + 0 prime? square 1 inc 10)
;;; b) product of all relatively prime integers

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(define (relatively-prime? a b)
  (= (gcd a b) 1))
(define (func-1-33-b n)
  (define (pred i)
    (relatively-prime? i n))
  (filtered-accumulate * 1 pred identity 1 inc (- n 1)))
(func-1-33-b 6)
(func-1-33-b 7)
;;; 1.34

(define (f g)
  (g 2))
(f square)
;;; (f f)

;;; This doesn't work since when you evaluate it,

;;; you get (2 2) and 2 is not a valid operator name

