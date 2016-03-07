(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
        	(iter (next a) (combiner (term a) result))
        	(iter (next a) result))))
  (iter a null-value))

; a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)

(define (sum-primes a b)
	(filtered-accumulate + 0 prime? identity a inc b))

(sum-primes 1 10)

; b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).

(define (product-relatively-prime n)
	(define (relatively-prime? i) (= (gcd i n) 1))
	(filtered-accumulate * 1 relatively-prime? identity 1 inc n))

(product-relatively-prime 6)

;;; aux functions

(define (identity x) x)

(define (inc x) (+ x 1))

(define (square x) (* x x))

(define (divides? p q) (= (remainder p q) 0))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

