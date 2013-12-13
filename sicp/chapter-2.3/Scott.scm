(define (memq item x)
  (cond
    ((null? x) #f)
    ((eq? item (car x)) x)
    (else (memq item (cdr x)))))
;; 2.53

(list 'a 'b 'c)
'(a b c)
'(list 1 2 3)
(list (quote a) (quote b) (quote c))
(list (list 'george))
'((x1 x2)
(y1 y2))
(cdr '((x1 x2)
(y1 y2)))
(cadr '((x1 x2)
(y1 y2)))
(pair? '(a short list))
(memq 'red '((red shoes)
(blue socks)))
(memq 'red '(red shoes blue socks))
;; 2.54

(eq? 1 1.0)
(equal? 1 1.0)
(= 1 1.0)
(symbol? 1)
(symbol? 'hello)
(symbol? (list 1 2 3))
(symbol? '())
(define (equal? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((and (number? a) (number? b)) (= a b))
    ((and (symbol? a) (symbol? b)) (eq? a b))
    ((and (pair? a) (pair? b))
      (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
    (else #f)))
(equal? 'a 'a)
(equal? (cons 'a 'b) (cons 'a 'b))
(equal? (list 1 2 3) (list 1 2 3))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this
(is a)
list))
;; 2.55

(car ' 'abracadabra)
(quote '5)
(car (quote (quote abracadabra)))
(cdr ' 'abracadabra)
(cdr (quote (quote abracadabra)))
(car ' ' 'abracadabra)
(cdr ' ' 'abracadabra)
(cadr ' ' 'abracadabra)
(cddr ' ' 'abracadabra)
;; You get quote back because the first quote quotes the second

;; symboloic manipulation

;; SECTION 2.3.2

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var)
      1
      0))
    ((sum? exp) (make-sum
      (deriv (addend exp) var)
      (deriv (augend exp) var)))
    ((product? exp) (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (deriv (multiplier exp) var)
        (multiplicand exp))))
    ((exponentiation? exp)
      (make-product
        (exponent exp)
        (make-product
          (make-exponent (base exp) (make-sum (exponent exp) -1))
          (deriv (base exp) var))))
    (else (error "unknown expression type -- DERIV" exp))))
;; representing algebraic expressions

(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product m1 m2)
  (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (caddr p))
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(*
(* x y)
(+ x 3))
       'x)
;; With simplification

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2))))
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2))))
;: (deriv '(+ x 3) 'x)

;: (deriv '(* x y) 'x)

;: (deriv '(* (* x y) (+ x 3)) 'x)

;; 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (make-exponent b n)
  (cond
    ((=number? n 0) 1)
    ((=number? n 1) b)
    ((and (number? b) (number? n)) (expt b n))
    (else (list '** b n))))
(deriv (list '** 'x 3) 'x)
(deriv '(**
(* x y)
(+ y n))
       'x)
;; EXERCISE 2.57

;: (deriv '(* x y (+ x 3)) 'x)

;; Set Operations

(define (element-of-set? x S)
  (cond
    ((null? S) #f)
    ((equal? x (car S)) #t)
    (else (element-of-set? x (cdr S)))))
(define (adjoin-set x S)
  (if (element-of-set? x S)
    S
    (cons x S)))
(define (intersection-set S T)
  (cond
    ((or (null? S) (null? T)) '())
    ((element-of-set? (car S) T) (cons (car S) (intersection-set (cdr S) T)))
    (else (intersection-set (cdr S) T))))
(define x
  (list 1 4 6))
(define y
  (list 1 5 8))
(element-of-set? 4 x)
(element-of-set? 12 x)
(adjoin-set 4 x)
(adjoin-set 5 x)
(intersection-set x y)
;; 2.59

(define (union-set S T)
  (cond
    ((null? S) T)
    ((element-of-set? (car S) T) (union-set (cdr S) T))
    (else (cons (car S) (union-set (cdr S) T)))))
(union-set x y)
;; 2.60 Sets with duplicates

(define (adjoin-set x S)
  (cons x S))
(define (union-set S T)
  (append S T))
(define xy
  (union-set x y))
(intersection-set x xy)
;; union is O(n) instead of O(n^2), but now memory usage may be much larger than n

;; if unions were most frequent,  and the increasing size could be mitigated periodically this representation would be ok.

;; an application could be creating an on the fly set of all words entered into a text editor, perhaps for then sending them to a spell check or doing other operations on them as the user is typing.

;; you would want the agglomeration into a set to be as fast as possible, the other stuff could happen in the background.

;;; Sorted Sets

(define (element-of-set? x S)
  (cond
    ((null? S) #f)
    ((= x (car S)) #t)
    ((< x (car S)) #f)
    (else (element-of-set? x (cdr S)))))
