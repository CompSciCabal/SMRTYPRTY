;; 2.1
(define (better-make-rat n d)
 (let ((g (gcd n d)))
  (if (>= d 0)
   (cons (/ n g) (/ d g))
   (cons (/ (* -1 n) g) (/ (* -1 d) g)))))

;; 2.4
(define (cons x y)
 (lambda (m) (m x y)))

(define (car z)
 (z (lambda (p q) p)))

(define (cdr z)
 (z (lambda (p q) q)))

;; 2.7
(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

;; 2.8

(define (make-interval a b) (cons a b))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y)
                  (- (upper-bound x) (lower-bound y)))))

;; 2.9
; width = (upper-bound - lower-bound) / 2
; let xlower = 1, xupper=3, ylower=2, yupper= 4
; width = ((3 + 4) - (1 + 2)) / 2
;       = (7 - 3) / 2 = 2
; width of x = 1
; width of y = 1
; width of x + y = 2
; I guess a logical example would be that make-interval still gives you an interval, i.e.
; even if you have 10+ vars that you're combining into an interval
; you still only get one interval that a width is derived from
; .. yeah don't really know how to -show- this, but it follows logically

;; 2.10
(define (mul-interval x y)
 (let ((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (lower-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (upper-bound x) (upper-bound y)))
       (make-interval (min p1 p2 p3 p4)
        (max p1 p2 p3 p4)))))

(define (better-div-interval x y)
  (if (or (= (upper-bound y) 0)
         (= (lower-bound y) 0))
      (error "Herp derp, divide by 0 error has appeared")
      (mul-interval x
         (make-interval (/ 1.0 (upper-bound y))
                        (/ 1.0 (lower-bound y))))))

;; 2.11
(define (make-center-width c w)
 (make-interval (- c w) (+ c w)))

(define (center i)
 (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
 (/ (- (upper-bound i) (lower-bound i)) 2))

;; 2.12
(define (make-center-percent center tolerance)
 (make-center-width center (* center tolerance)))

(define (percent n)
 (/ (width n) (center n)))
