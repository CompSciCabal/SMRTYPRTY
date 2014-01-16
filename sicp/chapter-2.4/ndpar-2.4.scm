#lang racket

;; -------------------------------------------------------------------
;; Data-Directed Programming, p.181
;; -------------------------------------------------------------------

;; The following two procedures will be defined in Chapter 3.3.3
;; TODO: Come back later and test 'deriv' procedure

(define (put op type item) null)

(define (get op type) null)

;; Exercise 2.73, p.184

(define (deriv sexp var)
  (cond ((number? sexp) 0)
        ((variable? sexp)
         (if (same-variable? sexp var) 1 0))
        (else ((get 'deriv (operator sexp)) (operands sexp) var))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define operator car)
(define operands cdr)

;; b.

(define (install-deriv-package)
  ; Internal procedures
  (define (deriv-sum sexp var)
    (make-sum (deriv (addend sexp) var)
              (deriv (augend sexp) var)))
  
  (define (deriv-prod sexp var)
    (make-sum (make-product (multiplier sexp)
                            (deriv (multiplicand sexp) var))
              (make-product (deriv (multiplier sexp) var)
                            (multiplicand sexp))))
  
  ; Interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  
  'done)

;; c.

(define (install-deriv-exp-package)
  ; Internal procedures
  (define base cadr)
  (define exponent caddr)
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          (else (list '** b e))))
  
  (define (deriv-exp sexp var)
    (let ((u (base sexp))
          (n (exponent sexp)))
      (make-product (make-product n (deriv u var))
                    (make-exponentiation u (- n 1)))))
  
  ; Interface to the rest of the system
  (put 'deriv '** deriv-exp)
  
  'done)

;; Auxiliary procedures

(define addend cadr)

(define (augend s) (combine-rest s '+))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? sexp num)
  (and (number? sexp) (= sexp num)))

(define (combine-rest sexp op)
  (let ((rest (cddr sexp)))
    (if (= (length rest) 1)
        (car rest)
        (cons op rest))))

(define multiplier cadr)

(define (multiplicand p) (combine-rest p '*))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
