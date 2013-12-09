#lang racket

;; -------------------------------------------------------------------
;; Symbolic Data
;; -------------------------------------------------------------------

;; Exercise 2.53, p.144
(list 'a 'b 'c) ;=> '(a b c)
(list (list 'george)) ;=> '((george))
(cdr '((x1 x2) (y1 y2))) ;=> '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;=> '(y1 y2)
(pair? (car '(a short list))) ;=> #f
(memq 'red '((red shoes) (blue socks))) ;=> #f
(memq 'red '(red shoes blue socks)) ;=> '(red shoes blue socks)

;; Exercise 2.54, p.145
;; Symbol equality (same as built-in equal?)
(define (my-equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (pair? x) (pair? y))
         (and (my-equal? (car x) (car y))
              (my-equal? (cdr x) (cdr y))))
        ((and (symbol? x) (symbol? y))
         (eq? x y)) (else false)))

;; Tests
(my-equal? '(this is a list) '(this is a list))
(not (my-equal? '(this is a list) '(this (is a) list)))

;; Exercise 2.55, p.145
;; Double quotation
(eq? (car ''abracadabra)
     (car (quote (quote abracadabra))))

;; -------------------------------------------------------------------
;; Symbolic Differentiation, p.147
;; -------------------------------------------------------------------

(define (deriv sexp var)
  (cond ((number? sexp) 0)
        ((variable? sexp)
         (if (same-variable? sexp var) 1 0))
        ((sum? sexp)
         (make-sum (deriv (addend sexp) var)
                   (deriv (augend sexp) var)))
        ((product? sexp)
         (make-sum (make-product (multiplier sexp)
                                 (deriv (multiplicand sexp) var))
                   (make-product (deriv (multiplier sexp) var)
                                 (multiplicand sexp))))
        ((exponentiation? sexp)
         (let ((u (base sexp))
               (n (exponent sexp)))
           (make-product (make-product n (deriv u var))
                         (make-exponentiation u (- n 1)))))
        (else (error "unknown s-expression type -- DERIV" sexp))))

(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)
(define (augend s) (combine-rest s '+))

(define (combine-rest sexp op)
  (let ((rest (cddr sexp)))
    (if (= (length rest) 1)
        (car rest)
        (cons op rest))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define multiplier cadr)
(define (multiplicand p) (combine-rest p '*))

(define (=number? sexp num)
  (and (number? sexp) (= sexp num)))

;; Tests
(eq? 1 (deriv '(+ x 3) 'x))

(eq? 'y (deriv '(* x y) 'x))

;; Exercise 2.56, p.150
;; Derivative of polynomial
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define base cadr)
(define exponent caddr)

;; Tests
(equal? '(* 5 (** x 4))
        (deriv '(** x 5) 'x))

(equal? '(* 2 x)
        (deriv '(** x 2) 'x))

(= 1 (deriv '(** x 1) 'x))

;; Exercise 2.57, p.151
(equal? '(+ (* 2 x) 2)
        (deriv '(+ (** x 2) (* 2 x) 1) 'x))

(equal? '(+ (* x y) (* y (+ x 3)))
        (deriv '(* x y (+ x 3)) 'x))

(= 2 (deriv '(+ x y (+ x 3)) 'x))

;; Exercise 2.58, p.151
;; Algebraic infix notation
#|
;; Parenthesized version
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2)))) ; swap the addend and +

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+))) ; car -> cadr

(define addend car) ; cadr -> car
(define augend caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2)))) ; swap the multiplier and *

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*))) ; car -> cadr

(define multiplier car) ; cadr -> car
(define multiplicand caddr)

;; Tests
(= 4 (deriv '(x + (3 * (x + (y + 2)))) 'x))

;; Standard algebraic version
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (pair? a1) (pair? a2)) (append a1 '(+) a2))
        ((pair? a1) (append a1 `(+ ,a2)))
        ((pair? a2) (append `(,a1 +) a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (member '+ x)))

(define (addend x)
  (let-values (((left _) (split-by x '+)))
    (if (= (length left) 1)
        (car left)
        left)))

(define (augend x)
  (let-values (((_ right) (split-by x '+)))
    (if (= (length right) 1)
        (car right)
        right)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define multiplier car)

(define (multiplicand x)
  (let-values (((left _) (split-by x '+)))
    (let ((m (cddr left)))
      (if (= (length m) 1)
          (car m)
          m))))

(define (split-by li op)
  (let-values (((left right)
                (splitf-at li (lambda (x) (not (eq? '+ x))))))
    (if (null? right)
        (values left right)
        (values left (cdr right)))))

;; Tests
(= 4 (deriv '(x + 3 * (x + y + 2)) 'x))
(deriv '(x * y * z + x * (x + y) + 2 * x * (x + z) + y * z) 'x)
|#
;; -------------------------------------------------------------------
;; Sets as unordered lists
;; -------------------------------------------------------------------
#|
;; Membership, Θ(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; Adjoin, Θ(n)
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Intersection, Θ(n^2)
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Union, Θ(n^2)
;; Exercise 2.59, p.153
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1)
                 (adjoin-set (car set1) set2))))

;; -------------------------------------------------------------------
;; Exercise 2.60, p.153
;; Sets as unordered lists with duplicates
;; -------------------------------------------------------------------

;; Adjoin, Θ(1)
(define adjoin-set cons)

;; element-of-set? is the same, Θ(n)
;; intersection-set is the same, Θ(n^2)
;; union-set is the same, Θ(n)

;; This representation is better if the majority of
;; operations are adjoin and union.

;; -------------------------------------------------------------------
;; (Numeric) sets as ordered lists
;; -------------------------------------------------------------------

;; Membership, Θ(n)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; Adjoin, Θ(n)
;; Exercise 2.61, p.155
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; Intersection, Θ(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; Union, Θ(n)
;; Exercise 2.62, p.155
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1)
                                           set2)))
                      ((< x2 x1)
                       (cons x2 (union-set set1
                                           (cdr set2)))))))))

;; Tests
(equal? '(1 2 4 6 8) (adjoin-set 1 '(2 4 6 8)))
(equal? '(2 4 5 6 8) (adjoin-set 5 '(2 4 6 8)))
(equal? '(2 4 6 8 9) (adjoin-set 9 '(2 4 6 8)))

(equal? '(2 4 6) (union-set '() '(2 4 6)))
(equal? '(1 3 5) (union-set '(1 3 5) '()))
(equal? '(1 2 3 4 5 6) (union-set '(1 3 5) '(2 4 6)))
|#
;; -------------------------------------------------------------------
;; (Numeric) sets as binary trees
;; -------------------------------------------------------------------

;; Tree abstraction

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

;; You can do it as (define make-tree list),
;; but it does not enforce correct number of params
(define (make-tree entry left right)
  (list entry left right))

;; Exercise 2.63, p.158
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '()))

(define tree-1 '(7 (3 (1 () ())
                      (5 () ()))
                   (9 ()
                      (11 () ()))))

(define tree-2 '(3 (1 () ())
                   (7 (5 () ())
                      (9 ()
                         (11 () ())))))

(define tree-3 '(5 (3 (1 () ())
                      ())
                   (9 (7 () ())
                      (11 () ()))))

;; tree->list-1 and tree->list-2 produce
;; the same results, depth-first binary search,
;; with the same order of growth, Θ(n).
(equal? '(1 3 5 7 9 11) (tree->list-1 tree-1))
(equal? '(1 3 5 7 9 11) (tree->list-1 tree-2))
(equal? '(1 3 5 7 9 11) (tree->list-1 tree-3))

(equal? '(1 3 5 7 9 11) (tree->list-2 tree-1))
(equal? '(1 3 5 7 9 11) (tree->list-2 tree-2))
(equal? '(1 3 5 7 9 11) (tree->list-2 tree-3))

;; Exercise 2.64, p.159
;; Ordered list -> balanced BST
;; Θ(n) order of growth
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (right-size (- n (+ left-size 1)))
             (this-entry (car non-left-elts))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;; Tests
(define tree-4 '(5 (1 ()
                      (3 () ()))
                   (9 (7 () ())
                      (11 () ()))))

(equal? tree-4 (list->tree '(1 3 5 7 9 11)))

;; Set operations

;; Membership, Θ(log n) on balanced tree
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

;; Adjoin, Θ(log n) on balanced tree
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; Exercise 2.65, p.160
(define (union-set set1 set2)
  (let ((ol1 (tree->list-1 set1))
        (ol2 (tree->list-1 set2)))
    (list->tree (union-list ol1 ol2))))

(define (intersection-set set1 set2)
  (let ((ol1 (tree->list-1 set1))
        (ol2 (tree->list-1 set2)))
    (list->tree (intersection-list ol1 ol2))))

;; These two procedures are the same
;; as above for odered lists
(define (intersection-list ol1 ol2)
  (if (or (null? ol1) (null? ol2))
      '()
      (let ((x1 (car ol1)) (x2 (car ol2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list (cdr ol1) (cdr ol2))))
              ((< x1 x2)
               (intersection-list (cdr ol1) ol2))
              ((< x2 x1)
               (intersection-list ol1 (cdr ol2)))))))

(define (union-list ol1 ol2)
  (cond ((null? ol1) ol2)
        ((null? ol2) ol1)
        (else (let ((x1 (car ol1)) (x2 (car ol2)))
                (cond ((= x1 x2)
                       (cons x1 (union-list (cdr ol1) (cdr ol2))))
                      ((< x1 x2)
                       (cons x1 (union-list (cdr ol1) ol2)))
                      ((< x2 x1)
                       (cons x2 (union-list ol1 (cdr ol2)))))))))

;; Tests
(define tree-5 '(6 (2 ()
                      (4 () ()))
                   (10 (8 () ())
                       ())))

(equal? '(6 (3 (1 ()
                  (2 () ()))
               (4 ()
                  (5 () ())))
            (9 (7 ()
                  (8 () ()))
               (10 ()
                   (11 () ()))))
        (union-set tree-1 tree-5))

(equal? tree-4 (intersection-set tree-1 tree-2))
