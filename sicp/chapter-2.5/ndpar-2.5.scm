#lang racket

(require "put-get.scm")

(define (square x) (* x x))

;; -------------------------------------------------------------------
;; Tag-based type system
;; -------------------------------------------------------------------

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    ;(printf "(~a ~a)~n" proc type-tags)
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))

;; -------------------------------------------------------------------
;; Generic Arithmetic Operations, p.189
;; -------------------------------------------------------------------

(define (equ? x y) (apply-generic 'equ? x y))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic angle z))

;; Scheme numbers

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

;; Rational numbers

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rational-package)
  ; internal procedures
  (define numer car)
  (define denom cdr)
  (define (make n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add x y)
    (make (+ (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))
  (define (sub x y)
    (make (- (* (numer x) (denom y))
             (* (numer y) (denom x)))
          (* (denom x) (denom y))))
  (define (mul x y)
    (make (* (numer x) (numer y))
          (* (denom x) (denom y))))
  (define (div x y)
    (make (* (numer x) (denom y))
          (* (denom x) (numer y))))
  ;; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make n d))))
  (put 'equ? '(rational rational) equal?)
  'done)

;; Complex numbers

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-complex-package)
  ; internal procedures
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  (define (add z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;interface
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (install-rectangular-package)
  (install-polar-package)
  'done)

;; Rectangular representation of complex numbers

(define (install-rectangular-package)
  ; internal procedures
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define make-from-real-imag cons)
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar representation of complex numbers

(define (install-polar-package)
  ; internal procedures
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define magnitude car)
  (define angle cdr)

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define make-from-mag-ang cons)
  ; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Exercise 2.79, p.193

(install-scheme-number-package)

(define s1 (make-scheme-number 6))
(define s2 (make-scheme-number 2))

(equ? (make-scheme-number 8) (add s1 s2))
(equ? (make-scheme-number 4) (sub s1 s2))
(equ? (make-scheme-number 12) (mul s1 s2))
(equ? (make-scheme-number 3) (div s1 s2))

(install-rational-package)

(define r1 (make-rational 2 3))
(define r2 (make-rational 1 2))

(equ? (make-rational 7 6) (add r1 r2))
(equ? (make-rational 1 6) (sub r1 r2))
(equ? (make-rational 1 3) (mul r1 r2))
(equ? (make-rational 4 3) (div r1 r2))

(install-complex-package)

(define c1 (make-complex-from-real-imag 3 4))
(define c2 (make-complex-from-real-imag 1 1))

(equ? (make-complex-from-real-imag 4 5) (add c1 c2))
(equ? (make-complex-from-real-imag 2 3) (sub c1 c2))

(define p1 (make-complex-from-mag-ang 4 (/ pi 2)))
(define p2 (make-complex-from-mag-ang 2 0))

(equ? (make-complex-from-mag-ang 8 (/ pi 2)) (mul p1 p2))
(equ? (make-complex-from-mag-ang 2 (/ pi 2)) (div p1 p2))

;; Exercise 2.77, p.192

(= 5 (magnitude c1))

;; Exercise 2.78, p.193

(= (add 6 2) 8)
(= (sub 6 2) 4)
(= (mul 6 2) 12)
(= (div 6 2) 3)