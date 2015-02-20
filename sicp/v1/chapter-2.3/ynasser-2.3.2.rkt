#lang racket
(require "ynasser-sicp.rkt")

;; Exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) 
  (car (cdr x)))

(define (exponent x)
  (car (cdr (cdr x))))

(define (make-exponentiation b e)
  (cond
    [(equal? 0 e) 1]
    [(equal? 1 e) b]
    [(andmap number? (list b e)) (expt b e)]
    [else (list '** b e)]))

(define (deriv-2.57 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-2.57 (addend exp) var)
                   (deriv-2.57 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-2.57 (multiplicand exp) var))
          (make-product (deriv-2.57 (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp) 
         (make-product
          (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv-2.57 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Exercise 2.57
;; See "ynasser-sicp.rkt"


;; Exercise 2.58a
(define (new-addend s) (cadr s))
(define (new-augmend s) (caddr s))
(define new-multiplier new-addend)
(define new-multiplicand new-augmend)

;; Exercise 2.58b
;; (x + 3 * (x + y + 2))
