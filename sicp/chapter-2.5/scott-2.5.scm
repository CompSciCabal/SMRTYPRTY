;;; copying in the code from chapter 2

;; we need a functioning get and put to do this

;;;from chapter 1

(define (square x)
  (* x x))
;;;SECTION 2.4.2

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))
;; Generic selectors

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
;;;-----------

;;;from section 3.3.3 for section 2.4.3

;;; to support operation/type table for data-directed dispatch

(define false #f)
(define (assoc key records)
  (cond
    ((null? records) false)
    ((equal? key (caar records)) (car records))
    (else (assoc key (cdr records)))))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
          (set-cdr!
            local-table
            (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        ((eq? m 'lookup-proc) lookup)
        ((eq? m 'insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;;;-----------

;;;SECTION 2.5.1

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))
;; uses get/put (from 3.3.3) -- see ch2support.scm

(define (install-rectangular-package)
  ;; internal procedures
  
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+
      (square (real-part z))
      (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put
    'make-from-real-imag
    'rectangular
    (lambda (x y)
      (tag (make-from-real-imag x y))))
  (put
    'make-from-mag-ang
    'rectangular
    (lambda (r a)
      (tag (make-from-mag-ang r a))))
  'done)
(install-rectangular-package)
(define (install-polar-package)
  ;; internal procedures
  
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons
      (sqrt (+ (square x) (square y)))
      (atan y x)))
  ;; interface to the rest of the system
  
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put
    'make-from-real-imag
    'polar
    (lambda (x y)
      (tag (make-from-real-imag x y))))
  (put
    'make-from-mag-ang
    'polar
    (lambda (r a)
      (tag (make-from-mag-ang r a))))
  'done)
(install-polar-package)
;;; scheme number package

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put
    'add
    '(scheme-number scheme-number)
    (lambda (x y)
      (tag (+ x y))))
  (put
    'sub
    '(scheme-number scheme-number)
    (lambda (x y)
      (tag (- x y))))
  (put
    'mul
    '(scheme-number scheme-number)
    (lambda (x y)
      (tag (* x y))))
  (put
    'div
    '(scheme-number scheme-number)
    (lambda (x y)
      (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x)
    (tag x)))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(install-scheme-number-package)
;; rational number package

(define (install-rational-package)
  ;; internal procedures
  
  (define (numer x)
    (car x))
  (define (denom x)
    (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat
      (+
        (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat
      (-
        (* (numer x) (denom y))
        (* (numer y) (denom x)))
      (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat
      (* (numer x) (numer y))
      (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat
      (* (numer x) (denom y))
      (* (denom x) (numer y))))
  (define (equ-rat p q)
    (= 0 (numer (sub-rat p q))))
  ;; interface to rest of the system
  
  (define (tag x)
    (attach-tag 'rational x))
  (put
    'add
    '(rational rational)
    (lambda (x y)
      (tag (add-rat x y))))
  (put
    'sub
    '(rational rational)
    (lambda (x y)
      (tag (sub-rat x y))))
  (put
    'mul
    '(rational rational)
    (lambda (x y)
      (tag (mul-rat x y))))
  (put
    'div
    '(rational rational)
    (lambda (x y)
      (tag (div-rat x y))))
  (put 'equ '(rational rational) (lambda (x y)
    (equ-rat x y)))
  (put 'make 'rational (lambda (n d)
    (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))
(install-rational-package)
;; complex number package

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  
  (define (add-complex z1 z2)
    (make-from-real-imag
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
  (define (equ-complex z1 z2)
    (and
      (= (real-part z1) (real-part z2))
      (= (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  
  (define (tag z)
    (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put
    'add
    '(complex complex)
    (lambda (z1 z2)
      (tag (add-complex z1 z2))))
  (put
    'sub
    '(complex complex)
    (lambda (z1 z2)
      (tag (sub-complex z1 z2))))
  (put
    'mul
    '(complex complex)
    (lambda (z1 z2)
      (tag (mul-complex z1 z2))))
  (put
    'div
    '(complex complex)
    (lambda (z1 z2)
      (tag (div-complex z1 z2))))
  (put
    'equ
    '(complex complex)
    (lambda (z1 z2)
      (equ-complex z1 z2)))
  (put
    'make-from-real-imag
    'complex
    (lambda (x y)
      (tag (make-from-real-imag x y))))
  (put
    'make-from-mag-ang
    'complex
    (lambda (r a)
      (tag (make-from-mag-ang r a))))
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(install-complex-package)
;; EXERCISE 2.77

(define z
  (make-complex-from-real-imag 3 4))
(magnitude z)
;; the complex module, calls out to apply generic, which then goes to the correct module.

;; exercise 2.78

;; using schemes types

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((number? datum) 'scheme-number)
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum))))
(define (install-scheme-number-package)
  (put
    'add
    '(scheme-number scheme-number)
    (lambda (x y)
      (+ x y)))
  (put
    'sub
    '(scheme-number scheme-number)
    (lambda (x y)
      (- x y)))
  (put
    'mul
    '(scheme-number scheme-number)
    (lambda (x y)
      (* x y)))
  (put
    'div
    '(scheme-number scheme-number)
    (lambda (x y)
      (/ x y)))
  (put
    'equ
    '(scheme-number scheme-number)
    (lambda (x y)
      (= x y)))
  (put 'make 'scheme-number (lambda (x)
    x))
  'done)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(install-scheme-number-package)
(define a 6)
(define b 7)
(add a b)
(div a b)
;; 2.79 Generic Equality

(define (equ x y)
  (apply-generic 'equ x y))
(equ a b)
(equ a a)
(define r1
  (make-rational 1 10))
(define r2
  (make-rational 10 10))
(equ r1 r2)
(equ r1 r1)
(define z1
  (make-complex-from-real-imag 1 10))
(define z2
  (make-complex-from-real-imag 1 100))
(equ z1 z2)
(equ z1 z1)
;; 2.80 generic zero
