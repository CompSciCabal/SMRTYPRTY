;; Here are the givens
(define (interval-/ x y)
  (interval-* x (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (interval-* x y)
  (let ((a (* (lower-bound x) (lower-bound y)))
        (b (* (lower-bound x) (upper-bound y)))
        (c (* (upper-bound x) (lower-bound y)))
        (d (* (upper-bound x) (upper-bound y))))
    (make-interval (min a b c d)
                   (max a b c d))))

(define (interval-+ x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;;; 2.7

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

;;; 2.8
(define (interval-- x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; 2.9
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; > (interval-width (make-interval 3 5))
;; 1
;; > (interval-width (make-interval 2 8))
;; 3
;; > (interval-width (make-interval 2 -2))
;; -2
;; > (interval-width (interval-+ (make-interval 3 5) (make-interval 2 8)))
;; 4
;; > (interval-width (interval-+ (make-interval 3 5) (make-interval 2 -2)))
;; -1

;; (interval-width (interval-+ a b)) is equivalent to (+ (interval-width a) (interval-width b)). The same looks true of interval-- and - ...

;; > (interval-width (interval-* (make-interval 3 5) (make-interval 2 -2)))
;; 10
;; > (interval-width (interval-/ (make-interval 3 5) (make-interval 2 -2)))
;; 2.5

;; but not of interval-*||interval-/ and *||/


;;; 2.10
(define (interval-/ x y)
  (if (= (upper-bound y) (lower-bound y))
      (error "Divide by zero-interval")
      (interval-* x (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;; 2.11
 ;; -_-

;;; 2.12
(define (make-center-width center w)
  (make-interval (- center w) (+ center w)))

(define (make-center-percent center percent)
  (make-center-width center (* center (/ percent 100))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;;; 2.13

;; No.

;;; 2.14
(define (par r1 r2)
  (interval-/ (interval-* r1 r2)
              (interval-+ r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (interval-/ one
                (interval-+ (interval-/ one r1)
                            (interval-/ one r2)))))

;; > (par (make-interval 2 5) (make-interval 2 -2))
;; (-3.33333333333333 . 3.33333333333333)
;; > (par2 (make-interval 2 5) (make-interval 2 -2))
;; (-3.33333333333333 . 1.0)

; He's right. As to the cause, I was going to blame floating point accuracy, except this

;; > (par (make-interval 2 5) (make-interval 2 8))
;; (0.307692307692308 . 10.0)
;; > (par2 (make-interval 2 5) (make-interval 2 8))
;; (1.0 . 3.07692307692308)

; looks like a bigger variance than you'd get as a result of that cause.

;;; 2.15

;;; 2.16
