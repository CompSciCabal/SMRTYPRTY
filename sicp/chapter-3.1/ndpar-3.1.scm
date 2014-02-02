#lang racket

(define (square x) (* x x))

;; -------------------------------------------------------------------
;; Introducing assignment. p.223
;; -------------------------------------------------------------------

;; Exercise 3.3, 3.4, p.225

(define (make-account balance password)
  (let ((count 0))
    (define (reset-count) (set! count 0))
    (define (inc-count) (set! count (+ 1 count)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pswd m)
      (cond ((not (eq? pswd password))
             (inc-count)
             (if (> count 3)
                 (const "Calling cops...")
                 (const "Incorrect password")))
            ((eq? m 'withdraw)
             (reset-count)
             withdraw)
            ((eq? m 'deposit)
             (reset-count)
             deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

;; Tests

(define acc (make-account 100 'secret-password))
(= 30 ((acc 'secret-password 'withdraw) 70))
(= 80 ((acc 'secret-password 'deposit) 50))
(= 10 ((acc 'secret-password 'withdraw) 70))
(eq? "Incorrect password" ((acc 'some-other-password 'withdraw) 5))

;; Exercise 3.1, p.224

(define ((make-accumulator acc) n)
  (set! acc (+ acc n))
  acc)

(define A (make-accumulator 5))
(= 15 (A 10))
(= 25 (A 10))

;; Exercise 3.2, p.224

(define (make-monitored f)
  (let ((count 0))
    (lambda (param)
      (cond ((eq? param 'how-many-calls?) count)
            ((eq? param 'reset-count)
             (set! count 0) count)
            (else
             (set! count (+ 1 count))
             (f param))))))

(define S (make-monitored sqrt))
(= 5 (S 25))
(= 1 (S 'how-many-calls?))
(= 0 (S 'reset-count))
(= 0 (S 'how-many-calls?))

;; -------------------------------------------------------------------
;; Random numbers, p.225
;; -------------------------------------------------------------------

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials 1.0))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

;; Exercise 3.5, p.228

(define (estimate-integral p x1 y1 x2 y2 trials)
  (* (- x2 x1) (- y2 y1) (monte-carlo trials p)))

(define (estimate-pi trials)
  (define (unit-circle)
    (<= (+ (square (random-in-range -1 1))
           (square (random-in-range -1 1)))
        1))
  (estimate-integral unit-circle -1 -1 1 1 trials))

(estimate-pi 1e5)

;; Exercise 3.6, p.229

(define (rand-update n)
  (remainder (+ (* 103 n) 701) 101))

(define rand
  (let ((x 100))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (new-value)
               (set! x new-value)
               x))))))

(= 93 (rand 'generate))
(= 79 (rand 'generate))
(= 51 (rand 'generate))
((rand 'reset) 100)
(= 93 (rand 'generate))