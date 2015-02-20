#lang racket

;;; PRELUDE

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

(define (div-interval x y) 
  (mul-interval
   x 
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; EX 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; EX 2.8

(define (sub-interval x y)
  (add-interval
   x
   (mul-interval
    y
    (make-interval -1 -1))))

; EX 2.9

; given x,y intervals, w(x) := (x.high - x.low)/2, and z = x+y, then
; w(z) = ((x.high+y.high) - (x.low+y.low))/2 
;      = (x.high-x.low)/2 + (y.high-y.low)/2 
;      = w(x) + w(y)

; given q = x-y, then 
; w(q) = ((x.high-y.low) - (x.low-y.high))/2 
;      = (x.high-x.low)/2 + (y.high-y.low)/2
;      = w(x) + w(y)

; > (define x (make-interval 0.1 0.2))
; > (define y (make-interval 1 2))
; > (mul-interval x y)
; '(0.1 . 0.4)
; (div-interval x y)
; '(0.05 . 0.2)

; (define x (make-interval 1.1 1.2))
; > (mul-interval y x)
; '(1.1 . 2.4)
; > (div-interval x y)
; '(0.55 . 1.2)

; (define x (make-interval -.05 .05))
; > (mul-interval y x)
; '(-0.1 . 0.1)
; > (div-interval x y)
; '(-0.05 . 0.05)

; x and y have the same width each time, so the product's width  
; can not be a function of the width of x and y.


;;; EX 2.10

(define (div-interval2 x y) 
  (define error "you done goofed")
  (cond ((and (< (lower-bound x) 0)
              (> (upper-bound x) 0))
         error)
        ((and (< (lower-bound y) 0)
              (> (upper-bound y) 0))
         error)         
        (else
         (mul-interval
          x 
          (make-interval (/ 1.0 (upper-bound y))
                         (/ 1.0 (lower-bound y)))))))


;;; EX 2.11

(define (mul-interval2 x y) 
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (let ((xlp (>= xl 0))
          (xhp (>= xh 0))
          (ylp (>= yl 0))
          (yhp (>= yh 0)))
    (cond ((or (and xlp xhp ylp yhp)
               (and xlp xhp     yhp)
               (and     xhp ylp yhp)
               (and (not xlp) (not xhp) (not ylp) (not yhp)))
           (make-interval (* xl yl) (* xh yh)))
          ((and xlp xhp        )
           (make-interval (* xl yh) (* xh yl)))
          ((and         ylp yhp)
           (make-interval (* xh yl) (* xl yh)))          
          ((and     xhp     yhp)
           (make-interval (min (* xh yl)(* xl yh)) 
                          (max ((* xl yl) (* xh yh))))) 
          ((and     xhp        )
           (make-interval (* xh yl) (* xl yl)))
          ((and             yhp)
           (make-interval (* xl yh) (* xl yl)))))))

; TODO: add test cases


;;; EX 2.12

(define (make-center-percent c p)
  (make-interval (- c (* c p)) (+ c (* c p))))
(define (center i) 
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (let ((c (center i))
        (u (upper-bound i)))
    (/ (- u c) c)))


;;; EX 2.13

; given intervals x and y, and z = x*y, 
; and xl, xh, yl, yh for low and high points, then
; percent(z) = (xh*yh - xl*yl) / (xh*yh + xl*yl)
;            = 


;;; EX 2.14

(define (par1 r1 r2) 
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2) 
  (let ((one (make-interval 1 1)))
    (div-interval 
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))


