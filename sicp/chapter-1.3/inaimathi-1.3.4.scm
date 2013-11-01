;; STILL in Chicken
;;; Stuff we still need
(define (square n) (* n n))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x))
   1.0))

;;; 1.40
(define (cube n) (expt n 3))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;;; 1.41
(define (inc a) (+ a 1))

(define (double fn)
  (lambda (n) (fn (fn n))))

;; It returns 21.

;;; 1.42
(define (compose f g)
  (lambda (n)
    (f (g n))))

;;; 1.43
(define (repeated fn reps)
  (define (rec acc ct)
    (if (= ct reps)
        acc
        (rec (compose acc fn) (+ ct 1))))
  (rec fn 1))

;;; 1.44
;; a)
(define (smooth fn)
  (lambda (x)
    (/ (+ (fn (- x dx)) (fn x) (fn (+ x dx))) 
       3)))

;; b) (repeated fn times)

;;; 1.45
;; first up, earlier functions
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; As stated in the problem, naively:
(define (nth-root x n)
  (fixed-point (lambda (y) (average y (/ x (expt y (- n 1))))) 1.0))

;; That'll never terminate for roots where n>3
;; Allegedly, this works well enough for 4th-roots
(define (4th-root x)
  (fixed-point
   (lambda (y)
     (average y (average y (/ x (expt y 3)))))
   1.0))

;; (4th-root 16) returns 2.0000000000022

;; TODO

;; (load "inaimathi-1.3.4.scm")

;;; 1.46
;; a)
(define (iterative-improve improve good-enough?)
  (define (rec guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          guess
          (rec next))))
  rec)

;; b)
(define (old-sqrt n)
  (fixed-point (lambda (y) (average y (/ n y))) 1.0))

(define (new-fixed-point fn first-guess)
  ((iterative-improve
    fn
    (lambda (g n) ;;; tolerance is a top-level binding here (ideally, it'd be an optional argument to new-sqrt)
      (< (abs (- v1 v2)) tolerance)))
   first-guess))

(define (new-sqrt n)
  ((iterative-improve
    (lambda (g)
      (average g (/ n g)))
    (lambda (g n) ;;; tolerance is a top-level binding here (ideally, it'd be an optional argument to new-sqrt)
      (< (abs (- v1 v2)) tolerance))) 1.0))