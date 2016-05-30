(require-extension numbers)
(require-extension sicp)
(require-extension test)

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
        ((=number? (car as) 0) (apply make-sum a1 (cdr as)))
        ((and (number? a1) (number? (car as)))
         (apply make-sum (+ a1 (car as)) (cdr as)))
        (else (list '+ a1 (apply make-sum (car as) (cdr as))))))

(define (make-product m1 . ms)
  (cond ((null? ms) m1)
        ((=number? m1 0) 0)
        ((=number? (car ms) 0) 0)
        ((=number? m1 1)
         (apply make-product (car ms) (cdr ms)))
        ((=number? (car ms) 1)
         (apply make-product m1 (cdr ms)))
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

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (let ((v (car set2)))
        (if (element-of-set? v set1)
            (union-set set1 (cdr set2))
            (union-set (cons v set1) (cdr set2))))))

;; 2.60

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union-set set1 set2)
  (append set1 set2))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))

(define (adjoin-set x set)
  (define (iter acc s)
    (cond ((null? s) acc)
          ((> x (car s))
           (iter (cons (car s) acc) (cdr s)))
          ((< x (car s))
           (iter (append (reverse acc) (cons x s)) '()))
          (else set)))
  (if (> x (car (reverse set)))
      (reverse (cons x (reverse set)))
      (iter '() set)))

(define (union-set set1 set2)
  (define (iter acc set1 set2)
    (cond ((null? set1)
           (append (reverse acc) set2))
          ((null? set2)
           (append (reverse acc) set1))
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((< x1 x2) (iter (cons x1 acc) (cdr set1) set2))
                   ((> x1 x2) (iter (cons x2 acc) set1 (cdr set2)))
                   (else (iter (cons x1 acc) (cdr set1) (cdr set2))))))))
  (iter '() set1 set2))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

;; *Exercise 2.63:* Each of the following two procedures converts a binary tree
;; to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; a. Do the two procedures produce the same result for every tree? If not, how
;;    do the results differ? What lists do the two procedures produce for the
;;    trees in *Note Figure 2-16::?

(define tree1 (make-tree 7
                         (make-tree 3
                                    (make-tree 1 nil nil)
                                    (make-tree 5 nil nil))
                         (make-tree 9
                                    nil
                                    (make-tree 11 nil nil))))
(define tree2 (make-tree 3
                         (make-tree 1 nil nil)
                         (make-tree 7
                                    (make-tree 5 nil nil)
                                    (make-tree 9 nil (make-tree 11 nil nil)))))
(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 nil nil)
                                    nil)
                         (make-tree 9
                                    (make-tree 7 nil nil)
                                    (make-tree 11 nil nil))))

(define tree4 (make-tree 7 (make-tree 3 nil nil) (make-tree 9 nil nil)))

(test-group "identical result for each"
  (test-assert (deep-equal? (tree->list-1 tree1) (tree->list-2 tree1)))
  (test-assert (deep-equal? (tree->list-1 tree2) (tree->list-2 tree2)))
  (test-assert (deep-equal? (tree->list-1 tree3) (tree->list-2 tree3)))
  (test-assert (deep-equal? (tree->list-1 tree4) (tree->list-2 tree4))))

;; b. Do the two procedures have the same order of growth in the number of steps
;;    required to convert a balanced tree with n elements to a list? If not,
;;    which one grows more slowly?

;; (copy-to-list tree4 '())
;; (copy-to-list '(3 nil nil) (cons 7 (copy-to-list '(9 nil nil) '())))
;; (copy-to-list '(3 nil nil) (cons 7 (copy-to-list nil (cons 9 (copy-to-list nil '())))))
;; (copy-to-list '(3 nil nil) (cons 7 (copy-to-list nil '(9))))
;; (copy-to-list '(3 nil nil) '(7 9))
;; (copy-to-list '(3 nil nil) '(7 9))
;; (copy-to-list nil (cons 3 (copy-to-list nil '(7 9))))
;; (copy-to-list nil (cons 3 '(7 9)))
;; (copy-to-list nil '(3 7 9))
;; '(3 7 9)

;; *Exercise 2.64:* The following procedure `list->tree' converts an ordered
;; list to a balanced binary tree. The helper procedure `partial-tree' takes as
;; arguments an integer n and list of at least n elements and constructs a
;; balanced tree containing the first n elements of the list. The result
;; returned by `partial-tree' is a pair (formed with `cons') whose `car' is the
;; constructed tree and whose `cdr' is the list of elements not included in the
;; tree.

(define (list->tree elements)
  (car (partial-tree
        elements
        (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a. Write a short paragraph explaining as clearly as you can how
;;    `partial-tree' works. Draw the tree produced by `list->tree' for the list
;;    `(1 3 5 7 9 11)'.

(list->tree '(1 3 5 7 9 11))

;; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))




;; b. What is the order of growth in the number of steps required by
;;    `list->tree' to convert a list of n elements?
