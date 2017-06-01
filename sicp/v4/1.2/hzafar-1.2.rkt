#lang racket

(require rackunit
         rackunit/log)

;; 1.9
; (+ 4 5)
; => (inc (+ (dec 4) 5))
; => (inc (+ 3 5))
; => (inc (inc (+ (dec 3) 5)))
; => (inc (inc (+ 2 5)))
; => (inc (inc (inc (+ (dec 2) 5))))
; => (inc (inc (inc (+ 1 5))))
; => (inc (inc (inc (inc (+ (dec 1) 5)))))
; => (inc (inc (inc (inc (+ 0 5)))))
; => (inc (inc (inc (inc 5))))
; => (inc (inc (inc 6)))
; => (inc (inc 7))
; => (inc 8)
; => 9
;; This process is recursive.

; (+ 4 5)
; (+ (dec 4) (inc 5))
; => (+ 3 6)
; => (+ (dec 3) (inc 6))
; => (+ 2 7)
; => (+ (dec 2) (inc 7))
; => (+ 1 8)
; => (+ (dec 1) (inc 8))
; => (+ 0 9)
; => 9
;; This process is iterative.

;; 1.10
; (define (f n) (A 0 n))
; (f n)
; => (A 0 n)
; => (* 2 n)
;; f(n) = 2n

; (define (g n) (A 1 n))
; (g n)
; => (A 1 n)
; => (A 0 (A 1 (- n 1)))
; We know from above that this is (* 2 (A 1 (- n 1)))
; (* 2 (A 1 (- n 1)))
; => (* 2 (A 0 (A 1 (- n 2))))
; => (* 2 (* 2 (A 1 (- n 2))))
; =>* (* 2 (* 2 ... (* 2 (A 1 1))))
; => (* 2 (* 2 ...(* 2 2))) ; n expansions
;; g(n) = 2^n

; (define (h n) (A 2 n))
; (h n)
; => (A 2 n)
; => (A 1 (A 2 (- n 1)))
; We know from above that this is (expt 2 (A 2 (- n 1)))
; (expt 2 (A 2 (- n 1)))
; => (expt 2 (A 1 (A 2 (- n 2))))
; => (expt 2 (expt 2 (A 2 (- n 2))))
; =>* (expt 2 (expt 2 ... (expt 2 (A 2 1))))
; => (expt 2 (expt 2 ... (expt 2 2))) ; n expansions
;; h(n) = 2↑n

;; 1.11
(define (f-recur n)
  (if (< n 3) n (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (f-iter-helper i n1 n2 n3)
    (if (< i 0) n1
        (f-iter-helper (- i 1) (+ n1 (* 2 n2) (* 3 n3)) n1 n2)))
  (if (< n 3) n (f-iter-helper (- n 3) 2 1 0)))

(check-equal? (f-recur 2) 2)
(check-equal? (f-iter 2) 2)
(check-equal? (f-recur 4) 11)
(check-equal? (f-iter 4) 11)
(check-equal? (f-recur 10) 1892)
(check-equal? (f-iter 10) 1892)

;; 1.12
(define (pascal-row n)
  (define (sum-row r)
    (if (empty? (cdr r)) r (cons (+ (car r) (cadr r)) (sum-row (cdr r)))))
  (if (= 0 n) '(1) (cons 1 (sum-row (pascal-row (- n 1))))))

(check-equal? (pascal-row 0) '(1))
(check-equal? (pascal-row 2) '(1 2 1))
(check-equal? (pascal-row 5) '(1 5 10 10 5 1))

;; 1.16
(define (fast-expt-iter a b n)
  (cond [(= 0 n) 1]
        [(= 1 n) (* a b)]
        [(even? n) (fast-expt-iter a (* b b) (/ n 2))]
        [else (fast-expt-iter (* a b) b (- n 1))]))

(check-equal? (fast-expt-iter 1 5 0) 1)
(check-equal? (fast-expt-iter 1 2 5) 32)
(check-equal? (fast-expt-iter 1 3 4) 81)
(check-equal? (fast-expt-iter 1 2 50) (expt 2 50))

;; 1.17
(define (fast-mult a b)
  (cond [(= b 0) 0]
        [(even? b) (fast-mult (* 2 a) (/ b 2))]
        [else (+ a (fast-mult a (- b 1)))]))

(check-equal? (fast-mult 2 4) 8)
(check-equal? (fast-mult 90 0) 0)
(check-equal? (fast-mult 1902 778902) (* 1902 778902))

;; 1.18
(define (fast-mult-iter r a b)
  (cond [(= 0 b) 0]
        [(= 1 b) (+ r a)]
        [(even? b) (fast-mult-iter r (* 2 a) (/ b 2))]
        [else (fast-mult-iter (+ r a) a (- b 1))]))

(check-equal? (fast-mult-iter 0 5 5) 25)
(check-equal? (fast-mult-iter 0 8 6) 48)
(check-equal? (fast-mult-iter 0 4 9) 36)
(check-equal? (fast-mult-iter 0 50 7) 350)

;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* 2 q p) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(check-equal? (fib 1) 1)
(check-equal? (fib 8) 21)
(check-equal? (fib 27) 196418)
(check-equal? (fib 50) 12586269025)

(test-log #:display? #t #:exit? #t)