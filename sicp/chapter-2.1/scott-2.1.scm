;; Definitions from the text

(define (make-rat n d)
  (cons n d))
(define (numer q)
  (car q))
(define (denom q)
  (cdr q))
(define (display-rat q)
  (display (numer q))
  (display "/")
  (display (denom q))
  (newline))
; (display-rat (make-rat 5 8))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(gcd 60 33)
(gcd (- 33) (- 21))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(make-rat 50 80)
;; 2.1 better make-rat

(define (make-rat n d)
  (define (sign a)
    (if (positive? a)
      1
      -1))
  (let ((g (gcd (abs n) (abs d)))
  (s (* (sign n) (sign d)))
  (n (abs n))
  (d (abs d)))
    (cons (* s (/ n g)) (/ d g))))
(make-rat -5 -6)
(make-rat -5 6)
(make-rat 5 -6)
(make-rat 17 -8)
(make-rat -17 8)
;; 2.2 line segments

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (display-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))
(define (combine-points binary-op p q)
  (make-point
    (binary-op (x-point p) (x-point q))
    (binary-op (y-point p) (y-point q))))
(define (scale-point s p)
  (make-point (* s (x-point p)) (* s (y-point p))))
; (display-point (make-point 1.4 4.5))

(define (make-segment x y)
  (cons x y))
(define (start-segment p)
  (car p))
(define (end-segment p)
  (cdr p))
(define (midpoint-segment s)
  (scale-point
    0.5
    (combine-points + (start-segment s) (end-segment s))))
(define seg-1
  (make-segment (make-point 5 7) (make-point -2 9)))
seg-1
(midpoint-segment seg-1)
;; 2.3 rectangles

;; first representation, bottom-left -top-right

(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))
(define (bottom-left-rect r)
  (car r))
(define (top-right-rect r)
  (cdr r))
(define (width-rect r)
  (x-point
    (combine-points - (top-right-rect r) (bottom-left-rect r))))
(define (height-rect r)
  (y-point
    (combine-points - (top-right-rect r) (bottom-left-rect r))))
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define rect-1
  (make-rect (make-point 3 4) (make-point 5 9)))
rect-1
(area-rect rect-1)
(perimeter-rect rect-1)
;; second representation bottom-left - width-height

(define (make-rect bottom-left top-right)
  (cons bottom-left (combine-points - top-right bottom-left)))
(define (bottom-left-rect r)
  (car r))
(define (top-right-rect r)
  (combine-points + (car r) (cdr r)))
(define (width-rect r)
  (x-point (cdr r)))
(define (height-rect r)
  (y-point (cdr r)))
;; 2.4 alternative pair implementation

(define (cons-alt x y)
  (lambda (m)
    (m x y)))
(define (car-alt p)
  (p (lambda (x y)
    x)))
(define (cdr-alt p)
  (p (lambda (x y)
    y)))
(define sm
  (cons-alt "super" "man"))
sm
(car-alt sm)
(cdr-alt sm)
;; 2.5 integer pairs representing a pair a,b as 2^a3^b

(define (divide-int a b)
  (define (iter acc i)
    (let ((r (remainder acc b)))
      (if (> r 0)
        i
        (iter (/ acc b) (+ i 1)))))
  (iter a 0))
(divide-int 12 3)
(define (expt-int a b)
  (define (iter i r)
    (if (= i b)
      r
      (iter (+ i 1) (* r a))))
  (iter 0 1))
(expt-int 2 5)
(integer? (expt-int 2 32))
(define (cons-int a b)
  (* (expt-int 2 a) (expt-int 3 b)))
(define (car-int p)
  (divide-int p 2))
(define (cdr-int p)
  (divide-int p 3))
(define int-pair-1
  (cons-int 5 4))
(integer? int-pair-1)
(car-int int-pair-1)
(cdr-int int-pair-1)
;; 2.6 Church Numerals

(define zero
  (lambda (f)
    (lambda (x)
      x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
;; church numerals answer the question, can we use numbers without having 

;; numbers in our system?

;; defining addition and multiplication using church numerals

; for addition you replace the zero of a with b

(define (add-church a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
(define (inc a)
  (+ 1 a))
((one inc) 0)
(define three
  (add-church one two))
((three inc) 0)
;; multiplication

; for multiplication you replace the function of a with the function of b

(define (mul-church a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))
(define six
  (mul-church two three))
((six inc) 0)
