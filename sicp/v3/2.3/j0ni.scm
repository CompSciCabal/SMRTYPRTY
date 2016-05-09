(require-extension numbers)
(require-extension sicp)

(define true #t)
(define false #f)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; *Exercise 2.53:* What would the interpreter print in response to evaluating
;; each of the following expressions?

;; (list 'a 'b 'c)
;; #;11> (a b c)

;; (list (list 'george))
;; #;12> ((george))

;; (cdr '((x1 x2) (y1 y2)))
;; #;13> ((y1 y2))

;; (cadr '((x1 x2) (y1 y2)))
;; #;14> (y1 y2)

;; (pair? (car '(a short list)))
;; #;15> #f

;; (memq 'red '((red shoes) (blue socks)))
;; #;16> #f

;; (memq 'red '(red shoes blue socks))
;; #;18> (red shoes blue socks)

;; *Exercise 2.54:* Two lists are said to be `equal?' if they contain
;; equal elements arranged in the same order.  For example,

;; (equal? '(this is a list) '(this is a list))

;; is true, but

;; (equal? '(this is a list) '(this (is a) list))

;; is false.  To be more precise, we can define `equal?'  recursively
;; in terms of the basic `eq?' equality of symbols by saying that `a'
;; and `b' are `equal?' if they are both symbols and the symbols are
;; `eq?', or if they are both lists such that `(car a)' is `equal?'
;; to `(car b)' and `(cdr a)' is `equal?' to `(cdr b)'.  Using this
;; idea, implement `equal?' as a procedure.(5)

(define (deep-equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        (else
         (let ((a-1 (car a))
               (b-1 (car b)))
           (cond ((eq? a-1 b-1)
                  (deep-equal? (cdr a) (cdr b)))
                 ((and (pair? a-1)
                       (pair? b-1))
                  (and (deep-equal? a-1 b-1)
                       (deep-equal? (cdr a) (cdr b))))
                 (else false))))))

;; *Exercise 2.55:* Eva Lu Ator types to the interpreter the
;; expression

;; (car ''abracadabra)

;; To her surprise, the interpreter prints back `quote'.  Explain.

;; 'abracadabra expands to (quote abracadabra)
;; ''abracadabra expands to '(quote abracadabra)
;; (car '(quote abracadabra)) is the symbol "quote"

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; (define (make-sum a1 a2) (list '+ a1 a2))
;; (define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;; *Exercise 2.56:* Show how to extend the basic differentiator to handle more
;; kinds of expressions. For instance, implement the differentiation rule

;; n_1   n_2
;; --- = ---  if and only if n_1 d_2 = n_2 d_1
;; d_1   d_2

;; by adding a new clause to the `deriv' program and defining appropriate
;; procedures `exponentiation?', `base', `exponent', and `make-exponentiation'.
;; (You may use the symbol `**' to denote exponentiation.) Build in the rules
;; that anything raised to the power 0 is 1 and anything raised to the power 1
;; is the thing itself.

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? exponent) (number? base)) (expt base exponent))
        (else (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv-2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-2 (addend exp) var)
                   (deriv-2 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv-2 (multiplicand exp) var))
          (make-product
           (deriv-2 (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum
             (exponent exp)
             -1)))
          (deriv-2 (base exp) var)))
        (else (error "unknown expression
                      type: DERIV" exp))))

;; Exercise 2.57: Extend the differentiation program to handle sums and products
;; of arbitrary numbers of (two or more) terms. Then the last example above
;; could be expressed as

;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and products,
;; without changing the deriv procedure at all. For example, the addend of a sum
;; would be the first term, and the augend would be the sum of the rest of the
;; terms.

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (apply make-sum (caddr s) (cdddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (apply make-product (caddr p) (cdddr p))))

(define (make-sum a1 . as)
  (cond ((null? as) a1)
        ((=number? a1 0) (apply make-sum (car as) (cdr as)))
        ((and (number? a1) (number? (car as)))
         (apply make-sum (+ a1 (car as)) (cdr as)))
        (else (list '+ a1 (apply make-sum (car as) (cdr as))))))

(define (make-product m1 . ms)
  (cond ((null? ms) m1)
        ((=number? m1 0) 0)
        ((=number? m1 1)
         (apply make-product (car ms) (cdr ms)))
        ((and (number? m1) (number? (car ms)))
         (apply make-product (* m1 (car ms)) (cdr ms)))
        (else (list '* m1 (apply make-sum (car ms) (cdr ms))))))

;; Exercise 2.58: Suppose we want to modify the differentiation program so that
;; it works with ordinary mathematical notation, in which + and * are infix
;; rather than prefix operators. Since the differentiation program is defined in
;; terms of abstract data, we can modify it to work with different
;; representations of expressions solely by changing the predicates, selectors,
;; and constructors that define the representation of the algebraic expressions
;; on which the differentiator is to operate.

;; 1. Show how to do this in order to differentiate algebraic expressions
;;    presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify
;;    the task, assume that + and * always take two arguments and that
;;    expressions are fully parenthesized.

;; 2. The problem becomes substantially harder if we allow standard algebraic
;;    notation, such as (x + 3 * (x + y + 2)), which drops unnecessary
;;    parentheses and assumes that multiplication is done before addition. Can
;;    you design appropriate predicates, selectors, and constructors for this
;;    notation such that our derivative program still works?
