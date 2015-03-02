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

;; fast-expt p45
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

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

(define (prime-test-runner primes testfn)
  (unless (empty? primes)
    (display (car primes))
    (start-prime-test (car primes) (runtime) testfn)
    (prime-test-runner (cdr primes) testfn)))

(displayln "Exercise 1.21")
(display-smallest-divisors '(199 1999 19999))

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

(displayln "Exercise 1.25")
(define (alsyp-expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (alsyp-fermat-test n)
  (define (try-it a)
    (= (alsyp-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Computation is slow, wrap in parens to actually run
(lambda ()
   (define (fast-prime? n times)
       (cond
         ((= times 0) true)
         ((alsyp-fermat-test n) (fast-prime? n (- times 1)))
         (else false)))
   (prime-test-runner known-primes (lambda (n) (fast-prime? n 10))))
;; Looking at the performance of using expmod there appears to be a massive
;; performance hit, even when trying to only test primes a couple of times.
;; After inspecting both the functions, it appears that the expmod function
;; is faster simply because it keeps the numbers we are working with small.
;; Alyssa's results in working with larger numbers that don't get reduced
;; until much later, resulting in unnecessary computation of large numbers.
;; See footnote 1.46 on page 52 for more details about why expmod is fast

;; Exercise 1.26
;; The reason why it is slow is because Louis is causing the computer to
;; computer expmod twice before multiplying them. We know that Scheme/Racket
;; uses applicative order for computation. Let's look at the two code snippets:
;; 1.26-a. (square (expmod base (/ exp 2) m))
;; vs
;; 1.26-b. (* (expmod base (/ exp 2) m) (expmod base (/ exp 2) m))
;; 
;; All we really need to do is to do the expansion for 1.26-a to see the gains.
;; Let's assign: base=2 exp=8 m=3
;; 1.26-a (square (expmod 2 (/ 8 2) 3))
;;        (square (expmod 2 4 3))
;; 1.26-b (* (expmod 2 (/ 8 2) 3) (expmod 2 (/ 8 2) 3))
;;        (* (expmod 2 4 3) (expmod 2 4 3))
;; 
;; I'd expand it further but it's annoying to do it. As we can see, 1.26-b is wasting
;; time computing a value it already computed.

(displayln "Exercise 1.27")
(define (test-is-prime n expected)
  (fprintf (current-output-port)
           "Expected the primality test of ~a to be ~a. Actual result: ~a~%"
           n
           expected
           (fast-prime? n 100)))

(test-is-prime 10007 true)
(test-is-prime 10009 true)
(test-is-prime 10037 true)
(test-is-prime 100003 true)
(test-is-prime 100019 true)
(test-is-prime 100043 true)

(test-is-prime 561 false)
(test-is-prime 1105 false)
(test-is-prime 1729 false)
(test-is-prime 2465 false)
(test-is-prime 2821 false)
(test-is-prime 6601 false)

(displayln "Exercise 1.28")
;; I'm having a hard time understanding what they are explaining
;; in the question. I also have a real hard time understanding
;; the math/pseudocode on wikipedia :(
;; Working through/reading the solution on schemewiki :(
(define (miller-rabin-expmod base exp m)
  (define (squaremod-with-check x)
    (define (check-nontrivial-sqrt1 x square)
      (if (and (= square 1)
               (not (= x 1))
               (not (= x (- m 1))))
          0
          square))
    (check-nontrivial-sqrt1 x (remainder (square x) m)))
  (cond
    ((= exp 0) 1)
    ((even? exp)
      (remainder (squaremod-with-check (miller-rabin-expmod base (/ exp 2) m))
                 m))
    (else
      (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                 m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define (check-it x)
      (and (not (= x 1)) (= x 0)))
    (check-it (miller-rabin-expmod a (- n 1) n)))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-fast-prime? n times)
  (cond
    ((= times 0) true)
    ((miller-rabin-test n) (miller-fast-prime? n (- times 1)))
    (else false)))

(define (test-is-prime-miller n expected)
  (fprintf (current-output-port)
           "Expected the primality test of ~a to be ~a. Actual result: ~a~%"
           n
           expected
           (miller-fast-prime? n 100)))

(test-is-prime-miller 10007 true)
(test-is-prime-miller 10009 true)
(test-is-prime-miller 10037 true)
(test-is-prime-miller 100003 true)
(test-is-prime-miller 100019 true)
(test-is-prime-miller 100043 true)

(test-is-prime-miller 561 false)
(test-is-prime-miller 1105 false)
(test-is-prime-miller 1729 false)
(test-is-prime-miller 2465 false)
(test-is-prime-miller 2821 false)
(test-is-prime-miller 6601 false)