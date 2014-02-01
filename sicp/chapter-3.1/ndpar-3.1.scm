#lang racket

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
