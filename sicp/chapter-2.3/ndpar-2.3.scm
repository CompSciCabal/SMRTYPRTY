#lang racket

;; -------------------------------------------------------------------
;; Symbolic Data
;; -------------------------------------------------------------------

;; Exercise 2.53, p.144
(list 'a 'b 'c) ;=> '(a b c)
(list (list 'george)) ;=> '((george))
(cdr '((x1 x2) (y1 y2))) ;=> '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;=> '(y1 y2)
(pair? (car '(a short list))) ;=> #f
(memq 'red '((red shoes) (blue socks))) ;=> #f
(memq 'red '(red shoes blue socks)) ;=> '(red shoes blue socks)

;; Exercise 2.54, p.145
;; Symbol equality (same as built-in equal?)
(define (my-equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (pair? x) (pair? y))
         (and (my-equal? (car x) (car y))
              (my-equal? (cdr x) (cdr y))))
        ((and (symbol? x) (symbol? y))
         (eq? x y)) (else false)))

;; Tests
(my-equal? '(this is a list) '(this is a list))
(not (my-equal? '(this is a list) '(this (is a) list)))

;; Exercise 2.55, p.145
;; Double quotation
(eq? (car ''abracadabra)
     (car (quote (quote abracadabra))))

;; -------------------------------------------------------------------
;; Symbolic Differentiation, p.147
;; -------------------------------------------------------------------

(define (deriv sexp var)
  (cond ((number? sexp) 0)
        ((variable? sexp)
         (if (same-variable? sexp var) 1 0))
        ((sum? sexp)
         (make-sum (deriv (addend sexp) var)
                   (deriv (augend sexp) var)))
        ((product? sexp)
         (make-sum (make-product (multiplier sexp)
                                 (deriv (multiplicand sexp) var))
                   (make-product (deriv (multiplier sexp) var)
                                 (multiplicand sexp))))
        ((exponentiation? sexp)
         (let ((u (base sexp))
               (n (exponent sexp)))
           (make-product (make-product n (deriv u var))
                         (make-exponentiation u (- n 1)))))
        (else (error "unknown s-expression type -- DERIV" sexp))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)
(define augend caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define multiplier cadr)
(define multiplicand caddr)

(define (=number? sexp num)
  (and (number? sexp) (= sexp num)))

;; Tests
(eq? 1 (deriv '(+ x 3) 'x))

(eq? 'y (deriv '(* x y) 'x))

;; Exercise 2.56, p.150
;; Derivative of polynomial
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define base cadr)
(define exponent caddr)

;; Tests
(equal? '(* 5 (** x 4))
        (deriv '(** x 5) 'x))

(equal? '(* 2 x)
        (deriv '(** x 2) 'x))

(= 1 (deriv '(** x 1) 'x))
