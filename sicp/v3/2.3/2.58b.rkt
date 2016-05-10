#lang racket

(require test-engine/racket-tests)

(define (=number? x n) (and (number? x) (= x n)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 . as)
  (cond ((empty? as) a1)
        ((=number? a1 0) (apply make-sum (car as) (cdr as)))
        ((=number? (car as) 0) (apply make-sum a1 (cdr as)))
        ((and (number? a1) (number? (car as))) (apply make-sum (+ a1 (car as)) (cdr as)))
        (else (cons a1 (cons '+ as)))))

(define (make-product m1 . ms)
  (cond ((empty? ms) m1)
        ((or (=number? m1 0) (=number? (car ms) 0)) 0)
        ((=number? m1 1) (apply make-product (car ms) (cdr ms)))
        ((=number? (car ms) 1) (apply make-product m1 (cdr ms)))
        ((and (number? m1) (number? (car ms))) (apply make-product (* m1 (car ms)) (cdr ms)))
        (else (cons m1 (cons '* ms)))))

(define (make-exponentiation x p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) x)
        ((and (number? x) (number? p)) (expt x p))
        (else (list x '** p))))

(define (find-until s los)
  (cond ((eq? (car los) s) empty)
        (else (cons (car los) (find-until s (cdr los))))))

(define (sum? x) (memq '+ x))

(define (addend x)
  (define result (find-until '+ x))
  (if (empty? (cdr result)) (car result) result))

(define (augend x)
  (define result (cdr (memq '+ x)))
  (if (empty? (cdr result)) (car result) result))

(define (product? x) (and (memq '* x) (not (memq '+ x))))

(define (multiplier x)
  (define result (find-until '* x))
  (if (empty? (cdr result)) (car result) result))

(define (multiplicand x)
  (define result (cdr (memq '* x)))
  (if (empty? (cdr result)) (car result) result))

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
(check-expect (deriv '(x * y * (x + 3 + y)) 'x) '((x * y) + (y * (x + 3 + y))))
(check-expect (deriv '(x ** 7) 'x) '(7 * (x ** 6)))
(check-expect (deriv '(2 * (x ** 2)) 'x) '(2 * (2 * x)))
(test)