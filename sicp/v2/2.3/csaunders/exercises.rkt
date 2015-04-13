#lang racket
(displayln "exercise 2.54")
(define (my-equal? first second)
  (cond [(and (null? first) (null? second)) #t]
        [(and (pair? (car first)) (pair? (car second)))
              (and (my-equal? (car first) (car second))
                   (my-equal? (cdr first) (cdr second)))]
        [(and (eq? (car first) (car second))
              (my-equal? (cdr first) (cdr second)))]
        [else #f]))

(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list (with more (lists)))
           '(this is a list (with more (lists))))
(my-equal? '(this (is a) list) '(this is a list))

(displayln "exercise 2.55")
(define abra ''abracadabra)
(car abra)
;; > 'quote -- Why?
;; Internally ' gets expanded to (quote <whatever I entered>) so if
;; we were to do an expansion on ''abracadabra we'd get something that
;; looks like: (quote (quote abracadabra)). Because the outmost "quote" causes
;; the rest of the form to not be evaluated we end up with (quote abracadabra) left
;; and by calling car on that list we grab the first thing which is the "quote".
;; Other items that can get pulled out of the above:
;; (car abra) > 'quote
;; (cdr abra) > '(abracadabra)
;; (cadr abra) > 'abracadabra
;; (cddr abra) > '()

(displayln "exercise 2.56")
;; From SICP
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [(exponent? exp)
         (make-exponent
          (make-product (exponent exp) (base exp))
          (make-sum (exponent exp) -1))]
        [else (error "unknown expresstion type -- DERIV" exp)]))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2 . rest)
  (cond
    [(=number? a1 0) a2]
    [(or (empty? a2) ;; Modification for 2.57
         (=number? a2 0)) a1]
    [(and (number? a1) (number? a2)) (+ a1 a2)]
    [(pair? rest) (list '+ a1 ;; Modification for 2.57
                        (make-sum a2 (car rest) (cdr rest)))]
    [else (list '+ a1 a2)]))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend x) (cadr x))
(define (augend x) (caddr x))

(define (make-product m1 m2 . rest)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(or (=number? m2 1) ;; Added for 2.57
         (empty? m2)) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [(pair? rest) (list '* m1 ;; Added for 2.57
                        (make-product m2 (car rest) (cdr rest)))]
    [else (list '* m1 m2)]))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

(define (make-exponent base power)
  (cond [(eq? 0 power) 1]
        [(eq? 1 power) base]
        [else (list '** base power)]))

(define (exponent? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(displayln "exercise 2.57")