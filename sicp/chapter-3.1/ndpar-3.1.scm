#lang racket

;; -------------------------------------------------------------------
;; Introducing assignment. p.223
;; -------------------------------------------------------------------

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

;; Tests

(define acc (make-account 100))
(= 30 ((acc 'withdraw) 70))
(= 80 ((acc 'deposit) 50))
(= 10 ((acc 'withdraw) 70))
