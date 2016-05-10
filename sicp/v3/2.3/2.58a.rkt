#lang racket

(require test-engine/racket-tests)

(define (=number? x n) (and (number? x) (= x n)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation x p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) x)
        ((and (number? x) (number? p)) (expt x p))
        (else (list x '** p))))

(define (sum? x) (and (list? x) (eq? (cadr x) '+)))
(define (addend x) (car x))
(define (augend x) (caddr x))

(define (product? x) (and (list? x) (eq? (cadr x) '*)))
(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))

(define (exponentiation? x) (and (list? x) (eq? (cadr x) '**)))
(define (base x) (car x))
(define (exponent x) (caddr x))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (multiplicand expr)
                         (deriv (multiplier expr) var))))
        ((exponentiation? expr)
         (make-product
            (make-product (exponent expr)
                         (make-exponentiation (base expr) (make-sum (exponent expr) -1)))
           (deriv (base expr) var)))
        (else (error "unknown expression type -- DERIV" expr))))

;; Tests
(check-expect (deriv '(x + 3) 'x) 1)
(check-expect (deriv '(x * y) 'x) 'y)
(check-expect (deriv '((x * y) * (x + 3)) 'x) '((x * y) + ((x + 3) * y)))
(check-expect (deriv '(x ** 7) 'x) '(7 * (x ** 6)))
(check-expect (deriv '(2 * (x ** 2)) 'x) '(2 * (2 * x)))
(test)