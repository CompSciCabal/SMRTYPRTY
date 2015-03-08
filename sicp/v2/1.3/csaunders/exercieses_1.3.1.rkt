#lang racket

;; Provided from SICP
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (even? x)
  (= (remainder x 2) 0))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (identity x) x)

(displayln "exercise 1.29")
(define (simpsons-integral fn lower upper steps)
  (define h (/ (- upper lower) steps))
  (define (yk k) (fn (+ lower (* k h))))
  (define (multfor k)
    (cond [(or (= k 0) (= k steps)) 1]
          [(= (remainder k 2) 1) 4]
          [else 2]))
  (define (simpson-term x)
    (* (multfor x) (yk x)))
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