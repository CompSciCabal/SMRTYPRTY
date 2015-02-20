#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; Exercise 2.7, p.94
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; Exerces 2.8, p.95
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9, p.95
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;(define int1 (make-interval 2 7))
;(define int2 (make-interval 3 11))

;(= (width (add-interval int1 int2))
;   (+ (width int1) (width int2)))

;(not (= (width (mul-interval int1 int2))
;        (+ (width int1) (width int2))))

;; Exercise 2.10, p.95
(define (div-interval x y)
  (let ((ly (lower-bound y))
        (uy (upper-bound y)))
    (if (> (* ly uy) 0)
        (mul-interval x (make-interval (/ uy) (/ ly)))
        (error "Interval spans zero"))))

;; Exercise 2.11, p.95
(define (mul x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond ((<= 0 lx) (cond ((<= 0 ly) (make-interval (* lx ly) (* ux uy)))
                           ((<= 0 uy) (make-interval (* ux ly) (* ux uy)))
                           ((< uy 0)  (make-interval (* ux ly) (* lx uy)))))
          ((<= 0 ux) (cond ((<= 0 ly) (make-interval (* lx uy) (* ux uy)))
                           ((<= 0 uy) (make-interval (min (* lx uy) (* ux ly))
                                                     (max (* lx ly) (* ux uy))))
                           ((< uy 0)  (make-interval (* ux ly) (* lx ly)))))
          ((< ux 0)  (cond ((<= 0 ly) (make-interval (* lx uy) (* ux ly)))
                           ((<= 0 uy) (make-interval (* lx uy) (* lx ly)))
                           ((< uy 0)  (make-interval (* ux uy) (* lx ly))))))))

;(define (eq x y)
;  (and (= (lower-bound x) (lower-bound y))
;       (= (upper-bound x) (upper-bound y))))

;(define (mul-test x y)
;  (eq (mul-interval x y) (mul x y)))

;(mul-test (make-interval 1 4) (make-interval 2 5))
;(mul-test (make-interval 1 4) (make-interval -2 5))
;(mul-test (make-interval 1 4) (make-interval -5 -2))
;(mul-test (make-interval -1 4) (make-interval 2 5))
;(mul-test (make-interval -1 4) (make-interval -2 5))
;(mul-test (make-interval -1 4) (make-interval -5 -2))
;(mul-test (make-interval -4 -1) (make-interval 2 5))
;(mul-test (make-interval -4 -1) (make-interval -2 5))
;(mul-test (make-interval -4 -1) (make-interval -5 -2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12, p.96
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;; Exercise 2.14, p.97
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one (add-interval (div-interval one r1)
                                    (div-interval one r2)))))

(define A (make-center-percent 100 2))
(define B (make-center-percent 200 1))
;(par1 A B)
;(par2 A B)

;(div-interval A B)

;; Interval arithmetic with constant value
(define O (make-interval 20 20))
;(percent (add-interval A O))
;(percent (sub-interval A O))
;(percent (mul-interval A O))
;(percent (div-interval A O))
;(percent (mul-interval O A))
;(percent (div-interval O A))

;; Interval arithmetic with itself
;(percent (add-interval A A))
;(percent (sub-interval A A))
;(percent (mul-interval A A))
;(percent (div-interval A A))
