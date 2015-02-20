#lang racket

;;; 2.5.3

;;; PRELUDE

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (add-poly p1 p2) 
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) 
                 (add-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var: ADD-POLY" (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2)) 
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2))) 
      (error "Polys not in same var: MUL-POLY" (list p1 p2))))

(define (install-polynomial-package) 
  ;; internal procedures 
  ;; representation of poly 
  (define (make-poly variable term-list) (cons variable term-list)) 
  (define (variable p) (car p))
  (define (term-list p) (cdr p)) 
  ;<procedures same-variable? and variable? from section 2.3.2⟩ 
  ;; representation of terms and term lists 
  ;⟨procedures adjoin-term . . . coeff from text below⟩ 
  (define (add-poly p1 p2) . . .) 
  ;⟨procedures used by add-poly⟩ 
  (define (mul-poly p1 p2) . . .) 
  ;⟨procedures used by mul-poly⟩ 
  ;; interface to rest of the system 
  (define (tag p) (attach-tag 'polynomial p)) 
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2)))) 
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2)))) 
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))) 
  'done)

(define (adjoin-term term term-list) 
  (if (=zero? (coeff term))
      term-list
      (cons term term-list))) 

(define (the-empty-termlist) '()) 
(define (first-term term-list) (car term-list)) 
(define (rest-terms term-list) (cdr term-list)) 
(define (empty-termlist? term-list) (null? term-list)) 
(define (make-term order coeff) (list order coeff))

(define (order term) (car term)) 
(define (coeff term) (cadr term))

(define (make-polynomial var terms) 
  ((get 'make 'polynomial) var terms))

