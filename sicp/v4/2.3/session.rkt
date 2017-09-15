#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? x y)
  (display x)
  (display y)
  (newline)
  (cond ((and (symbol? x)
              (symbol? y))
         (eq? x y))
        ((and (pair? x)
              (pair? y))
         (and (equal? (car x)
                      (car y))
              (equal? (cdr x)
                      (cdr y))))
        (else (and (null? x)
                   (null? y)))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend2 s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand2 p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) 
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0)
         1)
        ((=number? exponent 1)
         base)
        (else
         (list '** base exponent))))

;; 2.57

(define (make-sum-more x y . z)
  (if (null? z)
      (list '+ x y)
      (cons '+
            (cons x
                  (make-sum-more)))))

(define (applesauce constructor expression)
  (if (null? (cdddr expression))
      (caddr expression)
      (constructor (caddr expression)
                   (applesauce constructor
                               (cons 'applesauce
                                     (cddr expression))))))

(define (augend expression)
  (applesauce make-sum expression))

(define (multiplicand expression)
  (applesauce make-product expression))
