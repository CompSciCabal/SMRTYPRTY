#lang racket
;; Prereqs
(define dispatch-lookup '())
(define (new-dispatch!)
	(set! dispatch-lookup (make-hash)))

(define (put type sig proc)
	(hash-set! dispatch-lookup (cons type sig) proc))

(define (get type sig)
	(hash-ref dispatch-lookup (cons type sig)))

(define (apply-generic op . args)
  (let* [(type-tags (map type-tag args))
         (proc (get op type-tags))]
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))


(displayln "exercise 2.77")
;; Internally complex numbers are backed by polar or rectangular co-ordinates
;; i.e. ('complex ('polar ( ...data... ) )
;; When we add those methods, the outer tag gets stripped off via the method
;; dispatch and we are left with the underlying coordinate object.
;; To fully resolve these procedures, there will need to be 2 dispatches.

(displayln "exercise 2.78")
(define (attach-tag type-tag contents)
  (cond [(number? contents) contents]
        [(symbol? contents) contents]
        [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond [(pair? datum) (car datum)]
        [(number? datum) 'scheme-number]
        [(symbol? datum) 'scheme-symbol]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(pair? datum) (cdr datum)]
        [(number? datum) datum]
        [(symbol? datum) datum]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(displayln "exercise 2.79")
(define (equ? x y) (apply-generic 'equ? x y))

(displayln "exercise 2.80")
(define (=zero? x) (apply-generic '=zero? x))

(define (install-rat)
  (define (make-rat numer denom)
    (cons numer denom))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  
  ;; Actual Homework 2.79
  (define (equ? x y)
    (= 0 (numer (sub-rat x y))))
  
  (put 'equ? 'rational equ?)
  
  ;; Actual Homework 2.80
  (define (=zero? x)
    (= 0 (numer x)))
  
  (put '=zero? 'rational =zero?)
  )

(define (install-complex)
  (define (sub-complex x y) '()) ;; I'm not copying all the code over again...
  (define (real-part x) 0)
  (define (imag-part x) 0)
  
  ;; Actual Homework 2.79
  (define (equ? x y)
    (let [(diff (sub-complex x y))]
      (= 0 (real-part diff) (imag-part diff))))
  
  (put 'equ? 'complex equ?)
  ;; Actual Homework 2.80
  
  (define (=zero? x)
    (and (= 0 (real-part x) (imag-part x))))
  
  (put '=zero? 'complex =zero?)
  )

(define (install-scheme-numbers)
  ;; Actual Homework 2.79
  (define (equ? x y)
    (= 0 (- x y)))
  
  (put 'equ? 'scheme-number equ?)
  
  ;; Actual Homework 2.80
  (put '=zero? 'scheme-number (lambda (x) (= 0 x)))
  )