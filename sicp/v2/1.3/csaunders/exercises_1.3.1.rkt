#lang racket

;; Provided from SICP
(define (gcd m n) 
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n))))) 

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? x)
  (= (remainder x 2) 0))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (identity x) x)

;; From section 1.2 Exercises
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((= n 1) false)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(define (prime? n)
  (fast-prime? n 100))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "exercise 1.29")
(define (simpsons-integral fn lower upper steps)
  (define h (/ (- upper lower) steps))
  (define (yk k) (fn (+ lower (* k h))))
  (define (multfor k)
    (cond [(or (= k 0) (= k steps)) 1]
          [(= (remainder k 2) 1) 4]
          [else 2]))
  (define (simpson-term k)
    (* (multfor k) (yk k)))
  (* (/ h 3)
     (sum simpson-term 0 inc steps)))

(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)

(displayln "exercise 1.30")
(define (tail-rec-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(displayln "exercise 1.31 a.")
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 3)
(factorial 10)

(define (pi-term x)
  (if (even? x)
      (/ (+ x 2) (+ x 1))
      (/ (+ x 1) (+ x 2))))

(* (product pi-term 1 inc 6) 4)
(* (product pi-term 1 inc 100) 4)

(displayln "exercise 1.31 b.")
(define (tail-rec-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(* (tail-rec-product pi-term 1 inc 6) 4)
(* (tail-rec-product pi-term 1 inc 100) 4)

(displayln "exercise 1.32 a.")
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))

(define (accum-sum term a next b)
  (accumulate + 0 term a next b))

(define (accum-prod term a next b)
  (accumulate * 1 term a next b))

(displayln "1.32 a. before redifining sum")
(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)

(displayln "1.32 a. after redifining sum")
(set! sum accum-sum)
(simpsons-integral cube 0 1 100)
(simpsons-integral cube 0 1 1000)

(displayln "1.32 a. before redifining product")
(factorial 3)
(factorial 10)

(displayln "1.32 a. after redifining product")
(set! product accum-prod)

(factorial 3)
(factorial 10)

(displayln "1.32 b.")
(define (tail-rec-accum combiner term a next b result)
  (if (> a b)
      result
      (tail-rec-accum combiner
                      term
                      (next a)
                      next
                      b
                      (combiner (term a) result))))

(define (tr-accum-prod term a next b)
  (tail-rec-accum * term a next b 1))
(set! product tr-accum-prod)

(factorial 3)
(factorial 10)

(displayln "exercise 1.33")
(define (filtered-accumulate combiner filterfn null-value term a next b)
  (define (no-more?)
    (> a b))
  (if (no-more?)
      null-value
      (combiner (if (filterfn a)
                   (term a)
                   null-value)
               (filtered-accumulate combiner
                                    filterfn
                                    null-value
                                    term
                                    (next a)
                                    next
                                    b))))

(displayln "exercise 1.33 a.")
(define (sum-of-square-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))
(sum-of-square-primes 1 5)

(displayln "exercise 1.33 b.")
(define (relative-prime? m n)
  (= (gcd m n) 1))
(define (product-of-relative-primes n)
  (define (filter x) (relative-prime? x n))
  (filtered-accumulate * filter 1 identity 1 inc n))
(product-of-relative-primes 10)