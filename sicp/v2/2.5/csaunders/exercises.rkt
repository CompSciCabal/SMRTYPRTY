#lang racket
;; Prereqs
(define dispatch-lookup (make-hash))
(define (new-dispatch!)
	(set! dispatch-lookup (make-hash)))

(define (put type sig proc)
	(hash-set! dispatch-lookup (cons type sig) proc))

(define (get type sig)
  (if (hash-has-key? dispatch-lookup (cons type sig))
      (hash-ref dispatch-lookup (cons type sig))
      #f))

(define (apply-generic op . args)
  (let* [(type-tags (map type-tag args))
         (proc (get op type-tags))]
    (if proc
        (apply proc (map contents args))
        (error "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))


(displayln "exercise 2.77")
;; Internally complex numbers are backed by polar or rectangular co-ordinates
;; i.e. ('complex ('polar ( ...data... ) )
;; When we add those methods, the outer tag gets stripped off via the method
;; dispatch and we are left with the underlying coordinate object.
;; To fully resolve these procedures, there will need to be 2 dispatches.

(displayln "exercise 2.78")
(define (attach-tag type-tag contents)
  (cond [(number? contents) contents]
        [(symbol? contents) contents]
        [else (cons type-tag contents)]))

(define (type-tag datum)
  (cond [(pair? datum) (car datum)]
        [(number? datum) 'scheme-number]
        [(symbol? datum) 'scheme-symbol]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(define (contents datum)
  (cond [(pair? datum) (cdr datum)]
        [(number? datum) datum]
        [(symbol? datum) datum]
        [else (error "Bad tagged datum -- TYPE-TAG" datum)]))

(displayln "exercise 2.79")
(define (equ? x y) (apply-generic 'equ? x y))

(displayln "exercise 2.80")
(define (=zero? x) (apply-generic '=zero? x))

(define (install-rat)
  (define (make-rat numer denom)
    (cons numer denom))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  
  ;; Actual Homework 2.79
  (define (equ? x y)
    (= 0 (numer (sub-rat x y))))
  
  (put 'equ? 'rational equ?)
  
  ;; Actual Homework 2.80
  (define (=zero? x)
    (= 0 (numer x)))
  
  (put '=zero? 'rational =zero?)
  )

(define (install-complex)
  (define (sub-complex x y) '()) ;; I'm not copying all the code over again...
  (define (real-part x) 0)
  (define (imag-part x) 0)
  
  ;; Actual Homework 2.79
  (define (equ? x y)
    (let [(diff (sub-complex x y))]
      (= 0 (real-part diff) (imag-part diff))))
  
  (put 'equ? 'complex equ?)
  ;; Actual Homework 2.80
  
  (define (=zero? x)
    (and (= 0 (real-part x) (imag-part x))))
  
  (put '=zero? 'complex =zero?)
  )

(define (install-scheme-numbers)
  ;; Actual Homework 2.79
  (define (equ? x y)
    (= 0 (- x y)))
  
  (put 'equ? 'scheme-number equ?)
  
  ;; Actual Homework 2.80
  (put '=zero? 'scheme-number (lambda (x) (= 0 x)))
  )

(displayln "exercise 2.81")
;; 2.81 a.
;; there isn't an exp method defined, and because we have coercions that
;; continually resolve (we added identity methods) we can continuously
;; call apply-generic forever without ever reaching an end case.

;; 2.81 b.
;; No Louis is incorrect. Apply generic will work fine because if it doesn't
;; find a method for data of the same type then it means that the method hasn't
;; been defined

;; 2.81 c.
(define coercions (make-hash))
(define (put-coercion from to proc)
  (hash-set! coercions (cons from to) proc))
(define (get-coercion from to)
  (if (hash-has-key? (cons from to))
      (hash-ref coercions (cons from to))
      #f))
(define (apply-generic-w-coercion op . args)
  (let* [(type-tags (map type-tag args))
         (proc (get op type-tags))]
    (define (coercion-error)
      (error "No method for these types" (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let* [(t1 (car type-tags))
                   (t2 (cadr type-tags))
                   (a1 (car args))
                   (a2 (cadr args))
                   (throw-err (if (eq? t1 t2) (coercion-error) '()))
                   (t1->t2 (get-coercion t1 t2))
                   (t2->t1 (get-coercion t2 t1))]
              (cond [t1->t2 (apply-generic-w-coercion op (t1->t2 a1) a2)]
                    [t2->t1 (apply-generic-w-coercion op a1 (t2->t1 a2))]
                    ;; 2.84: Code will go in Here
                    [else (coercion-error)]))
            (coercion-error)))))

(displayln "exercise 2.82") ;; -- Still in progress
;; Apply generic will call a function that coerces a list of items to the desired type
(define (coerce-all-to desired others)
  (define (any? test items)
    (cond [(empty? items) #f]
          [(test (car items)) #t]
          [else (any? test (cdr items))]))
  
  (define (all? test items)
    (cond [(empty? items) #t]
          [(test (car items)) (all? test (cdr items))]
          [else #f]))
  
  (let [(coercions (map (lambda (other) (get-coercion other desired)) others))]
    (if (any? (lambda (x) (eq? #f x)) coercions)
        #f
        coercions)))

(define (find-available-coercion types)
  ;; This is a kinda shitty way of holding position in the list
  ;; basically we are going to be moving a pointer through the list
  ;; by simply keeping the head of the list in the types-in-front list
  ;; so we basically end up with something like this:
  ;; (a b c |d e f g) - | <- our current position (d is our current value)
  ;;                         the items in before the | are the types-in-front
  (define (coercions-iter position types-in-front)
    (if (empty? position)
        (error "No coercions could be found -- FIND-AVAILABLE-COERCION" types)
    (let* [(desired-type (car position))
           (coerce-rest (coerce-all-to desired-type (cdr position)))
           (coerce-front (coerce-all-to desired-type types-in-front))]
          (if (and coerce-rest coerce-front)
              (append coerce-front '(no-coerce) coerce-rest)
              (coercions-iter (cdr position) (append types-in-front (list (car position))))))))
  (coercions-iter types '()))

;; ^^^ Isn't done but I need more time to think about it.

(displayln "exercise 2.83")
;; Int Package
(define (int->rat i)
  ((get 'make 'rational) i 1))
(put 'raise 'scheme-number int->rat)

;; Hax
(define (numer x) x)
(define (denom x) x)
;; Rational Package
(define (rat->real r)
  ((get 'make 'real) (/ (numer r) (denom r))))
(put 'raise 'rational rat->real)

;; Real Package
(define (real->complex r)
  ((get 'make-from-real-imag 'complex) r 0))
(put 'raise 'real real->complex)

;; Primary Raise Procedure
(define (raise x) (apply-generic 'raise x))

(displayln "exercise 2.84")
;; This funciton has problems. First it needs to try and find all the
;; coercions, apply them and each time do a count. So if we find a
;; coercion we need to coerce the number twice in order to perform
;; our computation. This continues until we are able to actually do
;; the computation. So we do the test, something like n^2 or more times.
;; A better way to change this would be to make it coerce-up or something
;; that returns the correct coercion or returns false or something.
(define (is-subtype? item possible-parent)
  (define (raise-count x)
    (let [(proc (get 'raise (type-tag x)))]
      (if proc
          (+ 1 (raise-count (proc (cdr x))))
          0)))
  (> (raise-count item) (raise-count possible-parent)))

((lambda ()
   (new-dispatch!)
   (put 'raise 'int (lambda (x) (list 'rational (car x) 1)))
   (put 'raise 'rational (lambda (x) (list 'real (/ (car x) (cadr x)))))
   (put 'raise 'real (lambda (x) (list 'complex (car x) 0)))
   (displayln (is-subtype? '(int 7) '(complex 8 1)))
   (displayln (is-subtype? '(real 8.2) '(int 2)))))

;; Inside apply-generic within the cond you'd have something like this
(lambda (op t1 a1 t2 a2)
  (cond
    [(is-subtype? a1 a2) (apply-generic op ((get 'raise t1) a1) a2)]
    [(is-subtype? a2 a1) (apply-generic op a1 ((get 'raise t2) a2))]))

(displayln "exercise 2.85")
(lambda (real-part imag-part equ? make-real make-complex numer denom)
  (define (complex->real x) (make-real (real-part x)))
  (define (real->int x) (round x))
  (define (rat->int x) (round (/ (numer x) (denom x))))
  
  (put 'project '(complex) complex->real)
  (put 'project '(real) real->int)
  (put 'project '(rational) rat->int)
  
  (define (drop x)
    (let [(projection (get 'project (type-tag x)))]
          (if (and projection (equ? x (raise (projection x))))
              (drop (projection x))
              x)))
  
  'done)