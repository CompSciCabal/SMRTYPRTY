#lang racket

; Exercise 1.10, p.36
; Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(module+ test
  (require rackunit)
  (check-equal? (A 1 10) 1024)
  (check-equal? (A 2 4) 65536)
  (check-equal? (A 3 3) 65536))

; Exercise 1.11, p.42
; Tree recursion vs linear iteration
(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (* 1 (f1 (- n 1)))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

(define (f2 n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

; Exercise 1.12, p.42
; Pascal's triangle
(define (pascal m n)
  (if (or (= n 1) (= m n))
      1
      (+ (pascal (- m 1) (- n 1))
         (pascal (- m 1) n))))

; Exercise 1.15, p.44
(define (cube x) (* x x x))
(define (p x)
  (display " *** ")
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; Exercise 1.16, p.46
; Iterative exponentiation by successive squaring
; Time complexity Θ(log n), space Θ(1)
(define (expt x n)
  (define (iter a x n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* x x) (/ n 2)))
          (else (iter (* a x) x (- n 1)))))
  (iter 1 x n))

; Exercise 1.17, p.46
; Recursive multiplication by successive doubling
; Time and space complexity Θ(log n)
(define (double n) (+ n n))
(define (halve n) (quotient n 2))

(define (recur-mult m n)
  (cond ((= n 0) 0)
        ((= n 1) m)
        ((even? n) (recur-mult (double m) (halve n)))
        (else (+ m (recur-mult m (- n 1))))))

; Exercise 1.18, p.47
; Iterative multiplication by successive doubling
; Time complexity Θ(log n), space Θ(1)
(define (russian m n)
  (define (iter a m n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double m) (halve n)))
          (else (iter (+ a m) m (- n 1)))))
  (iter 0 m n))

; p.50
; Deterministic primality test
; Time complexity: Θ(√n)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (define (next td)
;    (if (= td 2) 3 (+ td 2))) ; Exercise 1.23, p.54
    (+ td 1))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1)
       (= (smallest-divisor n) n)))

; p.51
; Fast modular exponentiation by repeated squaring.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; Probabilistic primality test
; Time complexity: Θ(log n)
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.22, p.54
; Primality tests: order of growth
(define (search-for-primes n count)
  (cond ((= count 0) (newline) (display "done"))
        ((even? n) (search-for-primes (+ n 1) count))
;        ((fast-prime? n 3) (timed-prime-test n) ; Exercise 1.24, p.55
        ((prime? n) (timed-prime-test n)
                    (search-for-primes (+ n 2) (- count 1)))
        (else (search-for-primes (+ n 2) count))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
;  (cond ((fast-prime? n 3) ; Exercise 1.24, p.55
  (cond ((prime? n)
         (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " ** ")
  (display elapsed-time))

;(timed-prime-test 37) ; to warm interpreter
;(timed-prime-test 1009)
;(timed-prime-test 10007)
;(timed-prime-test 100003)
;(timed-prime-test 1000003)

; Exercise 1.27, p.55
(define (carmichael-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter a prod)
    (if (= 1 a)
        prod
        (iter (- a 1) (and (try-it a) prod))))
  (iter (- n 1) #t))

;(carmichael-test 561)
;(carmichael-test 1105)
;(carmichael-test 1729)
;(carmichael-test 2465)
;(carmichael-test 2821)
;(carmichael-test 6601)


; Exercise 1.28, p.56
; http://en.wikipedia.org/wiki/Miller–Rabin_primality_test

(define (devides? d n)
  (= (remainder n d) 0))

; Return a random integer from the range [lower..upper].
(define (rand lower upper)
  (+ lower (random (+ (- upper lower) 1))))

; Return two values D and S such that NUMBER = DIVISOR^S * D,
; and D is not divisible by DIVISOR.
(define (factor-out number divisor)
  (define (iter d s)
    (if (devides? divisor d)
        (iter (/ d divisor) (+ s 1))
        (values d s)))
  (iter number 0))

; Test N for primality by performing the Miller-Rabin test K times.
; Return #f if N is composite, and #t if N is probably prime.
(define (miller-rabin-test n k)
  (define-values (d s) (factor-out (- n 1) 2))
  (define (strong-liar? a)
    (define (iter c)
      (if (<= s c)
          #f
          (let ([m (expmod a (* d (expt 2 c)) n)])
            (cond ((= m 1) #f)
                  ((= m (- n 1)) #t)
                  (else (iter (+ c 1)))))))
    (let ([x (expmod a d n)])
      (if (or (= x 1) (= x (- n 1)))
          #t
          (iter 1))))
  (define (test count)
    (if (= count k)
        #t
        (if (strong-liar? (rand 2 (- n 2)))
            (test (+ 1 count))
            #f)))
  (cond ((= n 1) #f)
        ((< n 4) #t)
        ((even? n) #f)
        (else (test 0))))


(module+ test
  (check-false (miller-rabin-test 1729 3)))
