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
(display-smallest-divisors '(199 1999 19999))

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (displayln elapsed-time)
  #t)

(define (search-for-primes n found)
  (cond
    ((>= found 3))
    ((odd? n) (search-for-primes
                (+ n 1)
                (if (timed-prime-test n)
                  (+ found 1)
                  found)))
    (else (search-for-primes (+ n 1) found))))

(search-for-primes 1000 0)
(search-for-primes 10000 0)
(search-for-primes 100000 0)
(search-for-primes 1000000 0)

