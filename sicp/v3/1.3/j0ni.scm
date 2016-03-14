(require-extension sicp)
(require-extension numbers)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

;; Exercise 1.29: Simpson’s Rule is a more accurate method of
;; numerical integration than the method illustrated above. Using
;; Simpson’s Rule, the integral of a function f between a and b is
;; approximated as

;; h/3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn)

;; where h=(b−a)/n, for some even integer n, and yk=f(a+kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson’s Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n=100 and
;; n=1000), and compare the results to those of the integral procedure
;; shown above.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* h k))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (yk k))
          ((even? k) (* 2 (yk k)))
          (else (* 4 (yk k)))))
  (/ (* h (sum term 0 inc n))
     3.0))

;; Exercise 1.30: The sum procedure above generates a linear
;; recursion. The procedure can be rewritten so that the sum is
;; performed iteratively. Show how to do this by filling in the
;; missing expressions in the following definition:

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31:

;; 1. The sum procedure is only the simplest of a vast number of
;; similar abstractions that can be captured as higher-order
;; procedures. Write an analogous procedure called product that
;; returns the product of the values of a function at points over a
;; given range. Show how to define factorial in terms of product. Also
;; use product to compute approximations to π using the formula

;; π/4 = 2⋅4⋅4⋅6⋅6⋅8/3⋅3⋅5⋅5⋅7⋅7.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-val n)
  (define (even-term n)
    (if (odd? n)
        (+ n 1)
        (+ n 2)))
  (define (odd-term n)
    (if (odd? n)
        (+ n 2)
        (+ n 1)))
  (define (term a)
    (/ (even-term a) (odd-term a)))
  (* 4.0 (product term 1 inc n)))

;; 2. If your product procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (factorial-recursive n)
  (product-recursive identity 1 inc n))

;; Exercise 1.32:

;; Show that sum and product (Exercise 1.31) are both special cases of
;; a still more general notion called accumulate that combines a
;; collection of terms, using some general accumulation function:

;; Accumulate takes as arguments the same term and range
;; specifications as sum and product, together with a combiner
;; procedure (of two arguments) that specifies how the current term is
;; to be combined with the accumulation of the preceding terms and a
;; null-value that specifies what base value to use when the terms run
;; out. Write accumulate and show how sum and product can both be
;; defined as simple calls to accumulate.

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate-recur combiner null-value term (next a) next b)
                (term a))))

;; If your accumulate procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise 1.33: You can obtain an even more general version of
;; accumulate (Exercise 1.32) by introducing the notion of a filter on
;; the terms to be combined. That is, combine only those terms derived
;; from values in the range that satisfy a specified condition. The
;; resulting filtered-accumulate abstraction takes the same arguments
;; as accumulate, together with an additional predicate of one
;; argument that specifies the filter. Write filtered-accumulate as a
;; procedure. Show how to express the following using
;; filtered-accumulate:

(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

;; 1. the sum of the squares of the prime numbers in the interval a to
;; b (assuming that you have a prime? predicate already written)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-squares-of-primes a b)
  (define inc2 (compose inc inc))
  (define (next a)
    (if  (or (= a 1) (even? a))
         (inc a)
         (inc2 a)))
  (filtered-accumulate prime? + 0 square a next b))

;; 2. the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i<n such that
;; GCD(i,n)=1).

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum-of-relative-primes n)
  (define (relatively-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))
