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
(prime? 67)
;;; 1.11

(define (f n)
  (if (< n 3)
    n
    (+
      (f (- n 1))
      (* 2 (f (- n 2)))
      (* 3 (f (- n 3))))))
(f 1)
(f 4)
(define (g n)
  (if (< n 3)
    n
    (g-iter 0 1 2 (- n 2))))
(define (g-iter a b c count)
  (if (= count 0)
    c
    (g-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
(g 4)
(define (test-functions f1 f2 n)
  (cond
    ((= n 0) (display "Success!"))
    ((= (f1 n) (f2 n)) (test-functions f1 f2 (- n 1)))
    (else (display "Failure"))))
(test-functions f g 8)
;;; 1.12

(define (binomial-coefficient i n)
  (cond
    ((= i 0) 1)
    ((= i n) 1)
    (else (+
      (binomial-coefficient (- i 1) (- n 1))
      (binomial-coefficient i (- n 1))))))
(binomial-coefficient 1 1)
;;; 1.16

(define (my-expt b n)
  (expt-iter 1 b b 1 n))
(define (expt-iter a b c i n)
  (cond
    ((= n 0) a)
    ((<= i n)
      (expt-iter (* a b) (* b b) c (* 2 i) (- n i)))
    (else (expt-iter a c c 1 n))))
(my-expt 3 6)
;;; This is my weird approach with restarts
;;; Still O(logn) but I think the constant may be higher
;;; it goes:
; 1b^n 
; -> bb^(n-1)
; -> b^3b^(n-3)
; -> b^7b^(n-7)
; ... until b^(2^i) is too big and then it restarts, so here if n-7 is less than 8 the next step is
; -> b^7b^(n-7)
; -> b^8b^(n-8)
; -> b^10b^(n-10)
; ...

(define (expt-iterative base n)
  (define (iter val step power exponent)
    (cond
      ((= exponent 0) val)
      ((<= power exponent) (iter
        (* val step)
        (* step step)
        (* 2 power)
        (- exponent power)))
      (else (iter val base 1 exponent))))
  (iter 1 base 1 n))
(expt-iterative 4 7)
