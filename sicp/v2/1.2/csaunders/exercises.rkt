#lang racket

;; Provided by SICP
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m))
                 m))
    (else
      (remainder (* base (expmod base (- exp 1) m))
                 m))))

;; Helpers / Printers

(define (odd? x)
  (= 1 (remainder x 2)))

(define (runtime)
  (current-inexact-milliseconds))

(define (display-smallest-divisors items)
  (unless (empty? items)
    (let* ((i (car items))
           (d (smallest-divisor i)))
      (begin
        (fprintf (current-output-port)
                 "Smallest divisor of ~a is ~a~%" i (smallest-divisor i))
        (display-smallest-divisors (cdr items))))))

;; Exercise 1.21
(displayln "Exercise 1.21")
(display-smallest-divisors '(199 1999 19999))

;; Exercise 1.22
(displayln "Exercise 1.22")
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime) prime?))

(define (start-prime-test n start-time pred)
  (if (pred n)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (display " *** ")
  (displayln elapsed-time)
  true)

(define (search-for-primes n found bound)
  (cond
    ((>= found bound))
    ((odd? n) (search-for-primes
                (+ n 1)
                (if (timed-prime-test n)
                  (+ found 1)
                  found)
                bound))
    (else (search-for-primes (+ n 1) found bound))))

(search-for-primes 1000 0 3)
(search-for-primes 10000 0 3)
(search-for-primes 100000 0 3)
(search-for-primes 1000000 0 3)

;; Exercise 1.23
(displayln "Exercise 1.23")
(define (better-find-divisor n test-divisor)
  (let ([increment (if (= 2 test-divisor) 1 2)])
    (cond
      ((> (square test-divisor) n) n)
      ((divides? test-divisor n) test-divisor)
      (else (find-divisor n (+ test-divisor increment))))))
(set! find-divisor better-find-divisor)

(search-for-primes 1000 0 3)
(search-for-primes 10000 0 3)
(search-for-primes 100000 0 3)
(search-for-primes 1000000 0 3)

;; Observed Difference is about 3/5 to 2/3 speed
;; improvement over the original implementation.
;; I think it's because we aren't testing *half*
;; of the search space

;; Exercise 1.24
(displayln "Exercise 1.24")
(define known-primes
  '(1009 1013 1019 10007 10009 10037 100003 100019 100043 1000003 1000033 1000037
    1000039 1000081 1000099 1000117 1000121 1000133 1000151
    1000000007 1000000009 1000000021 1000000033 1000000087 1000000093 1000000097
    1000000103 1000000123 1000000181))

(define (timed-primes-test primes)
  (define (prime-predicate n)
    (fast-prime? n 100))
  (unless (empty? primes)
    (display (car primes))
    (start-prime-test (car primes) (runtime) prime-predicate)
    (timed-primes-test (cdr primes))))

(search-for-primes 1000000000 0 10)
(timed-primes-test known-primes)
;; Determining if 1000000181 was Prime using: 
;;  - search-for-primes: 2.2939453125
;;  - fast-prime:        0.739013671875
;; Though at the smaller numbers it appears that the Fermat Test
;; performs very badly in comparison. For example, trying to find
;; the prime for 1000037:
;; - search-for-primes: 0.058837890625
;; - fast-prime:        0.35009765625
;;
;; This appears much much slower, but as we get into very large
;; numbers (in the Billions) the fast-primes using the fermat
;; test performs much faster.
;;
;; There are a number of ways the fermat test can be slow. One of
;; those is simply the number of times we test a number. As was noted
;; in the book, if we decrease that value we'll be faster but won't be
;; as confident in the result.


