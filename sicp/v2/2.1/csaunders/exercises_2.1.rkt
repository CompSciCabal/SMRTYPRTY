#lang racket
;; provided from SICP
(define (gcd m n)
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
        (else (gcd n (remainder m n)))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer r)
  (car r))

(define (denom r)
  (cdr r))

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

(define (eql-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat r)
  (fprintf (current-output-port)
           "~a/~a~%"
           (numer r)
           (denom r)))

(displayln "exercise 2.1")
(define (impr-make-rat n d)
  (let* ([abs-n (abs n)]
         [abs-d (abs d)]
         [sign-n (/ n abs-n)]
         [sign-d (/ d abs-d)]
         [g (gcd abs-n abs-d)])
     (cons
      (* (* sign-n sign-d) (/ abs-n g))
      (/ abs-d g))))
(set! make-rat impr-make-rat)

(print-rat (make-rat -2 -4))
(print-rat (make-rat 2 -4))
(print-rat (make-rat -1 5))
(print-rat (make-rat 9 -3))

(displayln "exercise 2.2")
(define (make-point x y) (cons x y))
(define (p-x p) (car p))
(define (p-y p) (cdr p))
(define (p-print p)
  (fprintf (current-output-port)
           "(~a,~a)~%"
           (p-x p)
           (p-y p)))

(define (make-segment start end) (cons start end))
(define (seg-start s) (car s))
(define (seg-end s) (cdr s))

(define (seg-midpoint s)
  (let ([start (seg-start s)]
        [end (seg-end s)]
        [avg (lambda (x y) (/ (+ x y) 2.0))])
    (make-point (avg (p-x start) (p-x end))
                (avg (p-y start) (p-y end)))))

(define a (make-point 0 0))
(define b (make-point 0 5))
(define c (make-point 2 7))
(define d (make-point 3 2))
(define e (make-point 2 3))
(define f (make-point 10 15))
(define g (make-point 10 0))

(p-print (seg-midpoint (make-segment a b)))
(p-print (seg-midpoint (make-segment a c)))
(p-print (seg-midpoint (make-segment b c)))
(p-print (seg-midpoint (make-segment c d)))
(p-print (seg-midpoint (make-segment e f)))

(displayln "exercise 2.3")
;; Design with wishful thinking
;; -- call the functions we want
;; -- define them when they are missing

;; I'm defining rectangles as an origin and their two component
;; vectors. This should allow rectangles of any orientation to work.
;; Granted, I'm not doing anything to verify that vh and vw are
;; perpendicular.
;; (0, 5)<- vh
;;
;;
;; (0, 0)<- origin        (10, 0)<- vw
(define (make-rect origin vw vh)
  (cons (make-segment origin vw)
        (make-segment origin vh)))

(define (rect-perim r)
  (* 2 (+ (rect-width r) (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

;; Implement required functions -- note: need to
;; add segment-length function. Putting here to
;; associate with this question. Otherwise it would
;; be placed with all the other segment code.
(define (rect-width r)
  (seg-length (car r)))

(define (rect-height r)
  (seg-length (cdr r)))

(define (seg-length s)
  (let ([start (seg-start s)]
        [end (seg-end s)]
        [square (lambda (x) (expt x 2))])
    (sqrt (+ (square (- (p-x end) (p-x start)))
             (square (- (p-y end) (p-y start)))))))

(define rect-a (make-rect a g b))
(rect-width rect-a)
(rect-height rect-a)
(rect-perim rect-a)
(rect-area rect-a)

(displayln "exercise 2.3 ext: origin focused")
;; Instead simply base a rectangle by it's upper-right most edge
;; This makes rectangles origin based though
(define (alt-make-rect upper-right-x upper-right-y)
  (cons upper-right-x upper-right-y))

(define (alt-rect-width r) (car r))
(define (alt-rect-height r) (cdr r))
(set! rect-width alt-rect-width)
(set! rect-height alt-rect-height)

(define rect-b (alt-make-rect 10 5))
(rect-width rect-b)
(rect-height rect-b)
(rect-perim rect-b)
(rect-area rect-b)

(displayln "exercise 2.3 ext: message passing")
;; Can we go even further such that how our rect-width
;; and rect-height are even abstracted? We could do it
;; using the message passing style
(define (make-msg-passing-rect origin vw vh)
  (lambda (msg)
    (cond [(= msg 0) (seg-length (make-segment origin vw))]
          [(= msg 1) (seg-length (make-segment origin vh))])))
(define (msg-pass-rect-width r) (r 0))
(define (msg-pass-rect-height r) (r 1))

(set! rect-width msg-pass-rect-width)
(set! rect-height msg-pass-rect-height)

(define rect-c (make-msg-passing-rect a g b))
(rect-width rect-c)
(rect-height rect-c)
(rect-perim rect-c)
(rect-area rect-c)

(displayln "exercise 2.4")
(define (a-cons x y)
  (lambda (m) (m x y)))

(define (a-car z)
  (z (lambda (p q) p)))

;; What does the substitution of (a-car (a-cons 3 4)) look like?
;; (a-car (lambda (m) (m 3 4))
;;       A                  B
;; ((lambda (m) (m 3 4)) (lambda (p q) p))
;; m from A gets replaced with the provided lambda in B
;; Since m is applied, we need to pass in 3 and 4 to the
;; lambda in B
;; (lambda (3 4) 3)
;; > 3

;; How could we do the same for a-cdr?
(define (a-cdr z)
  (z (lambda (p q) q)))

(a-car (a-cons 3 4))
(a-cdr (a-cons 3 4))

(displayln "exercise 2.5")
;; I don't really _get_ this question
(define (num-cons a b) (* (expt 2 a) (expt 3 b)))
(define (num-car x) (count-0-remainder-divisions x 2))
(define (num-cdr x) (count-0-remainder-divisions x 3))

(define (count-0-remainder-divisions n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (expt divisor try-exp)))
        (iter (+ try-exp 1))
        (- try-exp 1)))
  (iter 1))

(num-car (num-cons 3 4))
(num-cdr (num-cons 3 4))

(displayln "exercise 2.6")
;; I'm not super super sure on this one.
;; Would be great to cover at the book club.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(add-1 zero)
;; Substitution Model:
;;                  Z
;; (add-1 (lambda (f) (lambda (x) x)))
;; In add-1 replace n with the lambda function identified by Z, to reduce
;; confusion let's rename the variable f in zero to g
;; (add-1 (lambda (g) (lambda (x) x)))
;;(lambda (f)
;;  (lambda (x)
;;    (f (((lambda (g) (lambda (x) x)) f) x))))
;; Apply f to Z (lambda (g)....)
;; (lambda (f)
;;  (lambda (x)
;;    (f ((lambda (x) x) x))))
;; Now apply x to (lambda (x) ...)
;; (lambda (f) (lambda (x) (f x)))
;; So... add-1 is really just (f x). Therefore, to create one
;; and two all we need to do is define them as the application of the function
;; a number of times
(define (one f) (lambda (x) (f x)))
(define (two f) (lambda (x) (f (f x))))

;; If we look at the definition of Church Numerals we can see
;; that plus(m, n) is just (mf(nf(x))
;; https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals
(define (church-sum m n)
  ;; First we need our f and x, which are just lambda functions
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
  