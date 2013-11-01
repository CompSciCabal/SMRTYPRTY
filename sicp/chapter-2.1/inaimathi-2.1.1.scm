(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; 2.1
(define (make-rat n d)
  (let ((g (if (> d 0) (gcd n d) (- (gcd n d)))))
    (cons (abs (/ n g)) (/ d g))))

;;; 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (map display (list "(" (x-point p) ", " (y-point p) ")"))
  (newline)
  (display ""))

(define (make-segment a b)
  ;; We should probably assert that a and b are both things that look like points
  (cons a b))
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segm)
  (make-point
   (average (x-point (start-segment segm))
            (x-point (end-segment segm)))
   (average (y-point (start-segment segm))
            (y-point (end-segment segm)))))

;;; 2.3
(define (make-rectangle a c)
  (list a (make-point (x-point a) (y-point c))
        c (make-point (x-point c) (y-point a))))
(define a-rect car)
(define b-rect cadr)
(define c-rect caddr)
(define d-rect cadddr)

(define (width-rect rect)
  (abs (- (x-point (a-rect rect)) (x-point (b-rect rect)))))

(define (height-rect rect)
  (abs (- (y-point (a-rect rect)) (y-point (d-rect rect)))))

(define (perimeter-rect rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

;;; 2.4
;; a) Yup.
;; b)

(define (cdr-closure z)
  (z (lambda (p q) q)))

;;; 2.5

;; -_-'

(define (cons-int a b)
  (* (expt 2 a) (expt 3 b)))

(define (divides? divisor num) (zero? (remainder num divisor)))

(define (count-divisions num divisor)
  (define (rec num ct)
    (if (divides? divisor num)
        (rec (/ num divisor) (+ ct 1))
        ct))
  (rec num 0))

(define (car-int cint) (count-divisions cint 2))
(define (cdr-int cint) (count-divisions cint 3))

;; (thanks, Dann)

;;; 2.6
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; zero
(define zero (lambda (f) (lambda (x) x)))

;; expansion
; (add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

;; so ...

(define one
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x) x)) f) x)))))

; (add-1 (add-1 zero))
; (add-1 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) f) x))))

;; and ...

(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               (f (((lambda (f)
                      (lambda (x) x)) f)
                   x))))
           f) x)))))

;; Church, you crazy motherfucker.