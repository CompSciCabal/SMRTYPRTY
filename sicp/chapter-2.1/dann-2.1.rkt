#lang racket

;;; PRELUDE

(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x))) 
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y) 
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y) 
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y) 
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;(define (make-rat n d) (cons n d)) 
(define (numer x) (car x)) 
(define (denom x) (cdr x))

; (define (make-rat n d) 
;   (let ((g (gcd n d)))
;     (cons (/ n g) (/ d g))))

(define (print-rat x) 
  (newline) 
  (display (numer x)) 
  (display "/") 
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;;; EX 2.1

(define (make-rat n d) 
  (let ((g (abs (gcd n d)))
        (ns (if (< (* n d) 0)
                (* -1 (abs n))
                (abs n)))
        (ds (abs d)))
    (cons (/ ns g) (/ ds g))))

;;; EX 2.2

; ugh their names are horrible so we'll use our own

(define (make-segment startp endp) (cons startp endp))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-coord point) (car point))
(define (y-coord point) (cdr point))

(define (mid-point segment)
  (let ((sp (start-point segment))
        (ep (end-point segment)))
    (make-point (/ (+ (x-coord sp) (x-coord ep)) 2)
                (/ (+ (y-coord sp) (y-coord ep)) 2))))

;;; EX 2.3

(define (make-rect top-left bot-right) (cons top-left bot-right))
(define (get-top-left rect) (car rect))
(define (get-bot-right rect) (cdr rect))

(define (rect-perim rect)
  (let ((top-left (get-top-left rect))
        (bot-right (get-bot-right rect)))
    (+ (* 2 (abs (- (x-coord top-left) (x-coord bot-right))))
       (* 2 (abs (- (y-coord top-left) (y-coord bot-right)))))))

(define (rect-area rect)
  (let ((top-left (get-top-left rect))
        (bot-right (get-bot-right rect)))
    (* (abs (- (x-coord top-left) (x-coord bot-right)))
       (abs (- (y-coord top-left) (y-coord bot-right))))))

; did anyone make this for general rects?
; todo: add width and height procedures as abstractions, rewrite perim & area


;;; EX 2.4

(define (cons1 x y) (lambda (m) (m x y)))
(define (car1 z) (z (lambda (p q) p)))
(define (cdr1 z) (z (lambda (p q) q)))

; (define x (cons1 123 654))
; (car1 x)
;  123
; (cdr1 x)
;  654

;;; EX 2.5

(define (cons-nat a b) (* (expt 2 a) (expt 3 b)))

(define (car-nat x)
  (define (car-nat-iter x acc)
    (if (= 0 (remainder x 2))
        (car-nat-iter (/ x 2) (+ 1 acc))
        acc))
  (car-nat-iter x 0))


(define (cdr-nat x)
  (if (= 0 (remainder x 3))
        (+ 1 (cdr-nat (/ x 3)))
        0))

; (define x (cons-nat 13 17))
; (car-nat x)
;   13
; (cdr-nat x)
;   17

;;; EX 2.6

(define id (lambda (x) x))
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f (((lambda (z) z) f) x)))))

; ((((add-1 zero) zero) zero) 333)
;   333
; (((one zero) zero) 333)
;   333

(define two
  (lambda (ff) 
    (lambda (x) 
      (ff (((lambda (f) 
              (lambda (x) 
                (f (((lambda (z) z) f) x)))) ff) x)))))

 
