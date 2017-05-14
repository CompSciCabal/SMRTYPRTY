#lang racket

;;; Prelude

(define (memq item x) 
  (cond ((null? x) false)
        ((eq? item (car x)) x) 
        (else (memq item (cdr x)))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2) 
        ((=number? a2 0) a1) 
        ((and (number? a1) (number? a2))
         (+ a1 a2)) 
        (else (list '+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0) 
        ((=number? m1 1) m2) 
        ((=number? m2 1) m1) 
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list '* m1 m2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

;;; Ex 2.53

; > (list 'a 'b 'c)
; '(a b c)
; > (list (list 'george)) 
; '((george))
; > (cdr '((x1 x2) (y1 y2))) 
; '((y1 y2))
; > (cadr '((x1 x2) (y1 y2))) 
; '(y1 y2)
; > (pair? (car '(a short list))) 
; #f
; > (memq 'red '((red shoes) (blue socks))) 
; #f
; > (memq 'red '(red shoes blue socks))
; '(red shoes blue socks)

;;; Ex 2.54

(define (equal?? a b)
  (cond ((and (pair? a) (pair? b)) 
         (and (equal?? (car a) (car b))
              (equal?? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else #f)))

;;; Ex 2.55

; ''abracadabra -> (quote (quote abracadabra))
; which is '(quote abracadabra)
; and (car '(foo bax)) is 'foo
; so (car ''abracadabra) is 'quote 
; NOTE: in DrRacket it returns 'quote not quote as in SICP

;;; Ex 2.56

(define (deriv exp var) 
  (cond ((number? exp) 0) 
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) 
         (make-sum
          (make-product (multiplier exp) 
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var) 
                        (multiplicand exp))))
        ((exponentiation? exp) 
         (make-product
          (exponent exp)
          (deriv
           (make-exponentiation 
            (base exp)
            (- (exponent exp) 1))
           var)))
        (else 
         (error "unknown expression type: DERIV" exp))))


(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (make-exponentiation m1 m2) 
  (cond ((=number? m1 0) 0) 
        ((=number? m2 0) 1) 
        ((=number? m1 1) 1) 
        ((=number? m2 1) m1) 
        ((and (number? m1) (number? m2)) (expt m1 m2)) 
        (else (list '** m1 m2))))
