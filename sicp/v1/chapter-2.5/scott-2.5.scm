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
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
  (put 'equ '(rational rational) (lambda (x y)
    (equ-rat x y)))
  (put 'zero? '(rational) (lambda (x)
    (= (numer x) 0)))
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
    'zero?
    '(complex)
    (lambda (z1)
      (equ-complex z1 (make-from-real-imag 0 0))))
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
  (put 'zero? '(scheme-number) (lambda (x)
    (= x 0)))
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

(define (=zero? x)
  (apply-generic 'zero? x))
(=zero? 0.0)
(=zero? r1)
(=zero? (sub r1 r1))
(=zero? z1)
(=zero? (sub z1 z1))
;; 2.81

;; Louis idea is unnecessary since the table used to lookup the operations is indexed by having the same type as both arguments.  no coercion is necessary.

;; 2.82

;; Generalizing coercion of function arguments when there are more than two arguments.

;; in that case you would extend apply-generic to try and match the types of more than two of the arguments.

;; without knowing anything about the relations between the types, any search needs to be 

;; exhaustive in order to guarantee it checks for all possible coercions.

;; in the text their search method isnt exhaustive so it could miss some.  

;; for instance it would fail in any instance where none of the arguments could be coerced to each other, 

;; but all could be coerced to some other type.

;; so for types A, B, and C, if A and B can be coerced to C but not to each other, then their approach will miss it.

;; Coercion Table

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)
(get-coercion 'rational 'obscure)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
          (type2 (cadr type-tags))
          (a1 (car args))
          (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
            (t2->t1 (get-coercion type2 type1)))
              (cond
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else
                  (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list op type-tags)))))))
;(add r1 0)

(add r1 r2)
(sub 3 z1)
;; 2.83 raise procedure

(define (numer r)
  (apply-generic 'numer r))
(define (denom r)
  (apply-generic 'denom r))
(numer r1)
(define (integer->rational n)
  (make-rational (inexact->exact (contents n)) 1))
;; temporary definition of make-real

(define (make-real x) x)
(define (rational->real r)
  (if (=zero? r)
    (make-real 0.0)
    (make-real (/ (numer r) (denom r)))))
(define (real->complex r)
  (scheme-number->complex r))
;; Defining a list of our type tower and a function to get the next type for each type

(define type-tower
  '(integer rational real complex))
(define (next-type x)
  (define (iter x L)
    (cond
      ((null? L) '())
      ((equal? x (car L)) (if (pair? (cdr L))
        (car (cdr L))
        '()))
      (else (iter x (cdr L)))))
  (iter x type-tower))
(next-type 'integer)
(null? (next-type 'complex))
(null? (next-type 'monkey))
;; Insert all of the raise coercions into the coercion table

(put-coercion 'integer 'rational integer->rational)
(put-coercion 'rational 'real rational->real)
(put-coercion 'real 'complex real->complex)
(define (raise x)
  (let ((current (type-tag x)))
    (let ((next (next-type current)))
      (if (null? next)
        x
        ((get-coercion current next) x)))))
(raise 3)
(make-rational 3 5)
(raise (make-rational 3 5))
((get-coercion 'rational 'real) (make-rational 3 5))

(raise 3.4)
(raise z1)
;; 2.84 getting apply-generic to use the type tower

;; We need to add integer and real to our type system

;; and modify our types so that scheme number is only selected after integer and real.  

;; NOTE: Using the default scheme types doesnt work for this exercise, since they implement dropping

;; which we dont want to do yet.

;; i.e. the call (integer? 3.0) returns true, instead of false as we expect for now.

;; instead I am returning to using the explicit tagging scheme

;; basic-number-package will use the untagged number types

(define (install-basic-number-package tag)
  (put 'add (list tag tag) (lambda (x y)
    (+ x y)))
  (put 'sub (list tag tag) (lambda (x y)
    (- x y)))
  (put 'mul (list tag tag) (lambda (x y)
    (* x y)))
  (put 'div (list tag tag) (lambda (x y)
    (/ x y)))
  (put 'equ (list tag tag) (lambda (x y)
    (= x y)))
  (put 'zero? tag (lambda (x)
    (= x 0)))
  (put 'make tag (lambda (x)
    x))
  'done)
;(install-basic-number-package 'integer)

;(install-basic-number-package 'real)

(define (install-tagged-number-package name)
  (define (tag x)
    (attach-tag name x))
  (put
    'add
    (list name name)
    (lambda (x y)
      (tag (+ x y))))
  (put 'sub (list name name) (lambda (x y)
    (tag (- x y))))
  (put 'mul (list name name) (lambda (x y)
    (tag (* x y))))
  (put 'div (list name name) (lambda (x y)
    (tag (/ x y))))
  (put 'equ (list name name) (lambda (x y)
    (= x y)))
  (put 'zero? (list name) (lambda (x)
    (= x 0)))
  (put 'make name (lambda (x)
    (tag x)))
  'done)
(install-tagged-number-package 'integer)
(install-tagged-number-package 'real)
(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-real n)
  ((get 'make 'real) n))
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond
    ((pair? datum) (car datum))
    ((integer? datum) 'integer)
    ((real? datum) 'real)
    ((number? datum) 'scheme-number)
    (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond
    ((pair? datum) (cdr datum))
    ((number? datum) datum)
    (else (error "Bad tagged datum -- CONTENTS" datum))))
;; Last we can modify apply-generic so it has knowledge of raise

;; we need to be able to compare the position of types

(define (rank-type x)
  (define (iter i x L)
    (cond
      ((null? L) 0)
      ((equal? x (car L)) i)
      (else (iter (+ i 1) x (cdr L)))))
  (iter 1 x type-tower))
(rank-type 'integer)
(rank-type 'rational)
(rank-type 'real)
(define (get-raising-coercion t1 t2)
  (define (raise-n n)
    (define (iter i f)
      (if (= i n)
        f
        (iter (+ i 1) (lambda (x)
          (raise (f x))))))
    (iter 1 raise))
  (let ((r1 (rank-type t1))
  (r2 (rank-type t2)))
    (cond
      ((or (= 0 r1) (= 0 r2)) (get-coercion t1 t2))
      ((< r1 r2) (raise-n (- r2 r1)))
      (else #f))))
((get-raising-coercion 'integer 'rational) 6)
((get-raising-coercion 'integer 'real) 7)
(get-raising-coercion 'real 'integer)
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
          (type2 (cadr type-tags))
          (a1 (car args))
          (a2 (cadr args)))
            (let ((t1->t2 (get-raising-coercion type1 type2))
            (t2->t1 (get-raising-coercion type2 type1)))
              (cond
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else
                  (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list op type-tags)))))))
(rational->real (make-rational 4 5))
(add (make-integer 2) (make-integer 4))
(add (make-integer 5) (make-real 3.4))
(add (make-integer 1) (make-real 3.5))
(add
  (make-integer 5)
  (make-complex-from-real-imag 1 2))
(equ
  (make-integer 2)
  (make-complex-from-real-imag 2 0))
;; 2.85 Implementing Drop

;; drop needs to push a type as low as possible in the hierarchy

;; we can do it similar to raise, but the argument must pass a predicate test before falling through

(define (rational->integer r)
  (make-integer (round (/ (numer r) (denom r)))))
;; my interpreter doesnt support rationalize, numerator, or denominator.  so this is a bit challenging.

;;im simply setting the denominator to be 100 and truncating the value.

;; this could be greatly improved

(define (real->rational x)
  (define precision 100)
  (cond
    ((integer? (contents x))
      (make-rational (inexact->exact (contents x)) 1))
    (else
      (make-rational
        (inexact->exact (round (* (contents x) precision)))
        precision))))
(define (complex->real c)
  (make-real (real-part c)))
(rational->integer (make-rational 3 6))
(real->rational (make-real 99))
(real->rational (make-real 0.99))
(real->rational (make-real 19.99999))
(complex->real (make-complex-from-real-imag 2.4 5.9))
;; Insert all of the lower coercions into the coercion table

(put-coercion 'rational 'integer rational->integer)
(put-coercion 'real 'rational real->rational)
(put-coercion 'complex 'real complex->real)
(define (prev-type x)
  (define (iter p x L)
    (cond
      ((null? L) '())
      ((equal? x (car L)) p)
      (else (iter (car L) x (cdr L)))))
  (iter '() x type-tower))
(prev-type 'integer)
(prev-type 'rational)
(prev-type 'complex)
(prev-type 'notatype)
(define (lower x)
  (let ((current (type-tag x)))
    (let ((prev (prev-type current)))
      (if (null? prev)
        x
        ((get-coercion current prev) x)))))
(define (drop x)
  (let ((lx (lower x))
  (rlx (raise (lower x))))
    (cond
      ((equal? (type-tag x) (type-tag lx)) x)
      ((equ x rlx) (drop lx))
      (else x))))
(drop (make-real 3.5))
(drop (make-real 3.555))
(drop (make-real 3.0))
(lower (lower (make-real 3.0)))
(define (apply-generic op . args)
  (let ((result (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
          (type2 (cadr type-tags))
          (a1 (car args))
          (a2 (cadr args)))
            (let ((t1->t2 (get-raising-coercion type1 type2))
            (t2->t1 (get-raising-coercion type2 type1)))
              (cond
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else
                  (error "No method for these types" (list op type-tags))))))
          (error "No method for these types" (list op type-tags))))))))
    (if (pair? result)
      (drop result)
      result)))
(add (make-real 3.5) (make-real 2.5))
(sub
  (make-complex-from-real-imag 1 1)
  (make-integer 2))
(sub (make-real 8) (make-rational 64 8))
(=zero? (make-real 0.0))
(=zero? (make-integer 0))
(=zero? (make-rational 0 1))
;; 2.86 generic components of complex numbers.

;; this means changing the interface for complex numbers so that the constructor and selectors give back generics.

;;;

;;;

;;;

;;;
