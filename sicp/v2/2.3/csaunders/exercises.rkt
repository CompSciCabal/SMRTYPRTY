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
(displayln "see commented sections in earlier code")

(displayln "exercise 2.58 a.")
(deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)
(define (infix-make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (infix-sum? exp) (eq? '+ (cadr exp)))
(define infix-addend car)
(define infix-augend caddr)
(set! make-sum infix-make-sum)
(set! sum? infix-sum?)
(set! addend infix-addend)
(set! augend infix-augend)

(define (infix-make-product m1 m2)
  (cond [(or (=number? m1 0) (=number? m2 0)) 0]
        [(=number? m1 1) m2]
        [(=number? m2 1) m1]
        [(and (number? m1) (number? m2)) (* m1 m2)]
        [else (list m1 '* m2)]))
(define (infix-product? exp) (eq? '* (cadr exp)))
(define infix-multiplier car)
(define infix-multiplicand caddr)
(set! make-product infix-make-product)
(set! product? infix-product?)
(set! multiplier infix-multiplier)
(set! multiplicand infix-multiplicand)

(deriv '(x + (3 * (x + (y + 2)))) 'x)

(displayln "exercise 2.58 b.")
(define (any? pred test . rest)
  (cond [(pred test) #t]
        [(empty? rest) #f]
        [else (apply any? pred rest)]))

(define (zero? n) (=number? n 0))      

(define (alg-make-sum a1 a2)
  (let [(a1-sum? (alg-sum? a1))
        (a2-sum? (alg-sum? a2))
        (a1-num? (number? a1))
        (a2-num? (number? a2))]
  (cond [(and a1-num? (= a1 0)) a2]
        [(and a2-num? (= a2 0)) a1]
        [(and a1-sum? a2-sum?) (append a1 '(+) a2)]
        [a1-sum? (append a1 (list '+ a2))]
        [a2-sum? (append (list a1 '+) a2)]
        [else (list a1 '+ a2)])))
(define (alg-sum? exp) (and (pair? exp) (eq? '+ (cadr exp))))
(define alg-addend car)
(define alg-augend cddr)

(define (alg-make-product m1 m2)
  (let* [(m1-prod? (alg-prod? m1))
        (m2-prod? (alg-prod? m2))
        (m1-num? (number? m1))
        (m2-num? (number? m2))]
    (cond [(any? zero? m1 m2) 0]
          [(and m1-num? (= m1 1)) m2]
          [(and m2-num? (= m2 1)) m1]
          [(and m1-num? m2-num?) (* m1 m2)]
          [(and m1-prod? m2-prod?) (append m1 '(*) m2)]
          [m1-prod? (append m1 (list '* m2))]
          [m2-prod? (append (list m1 '*) m2)]
          [else (list m1 '* m2)])))
          

(define (alg-prod? exp) (and (pair? exp) (eq? '* (cadr exp))))
(define alg-multiplier car)
(define alg-multiplicand cddr)

(displayln "exercise 2.59")
(define (element-of-set? x set)
  (cond [(empty? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set set1 set2)
  (cond [(empty? set1) set2]
        [(empty? set2) set1]
        [else (union-set
               (adjoin-set (car set2) set1)
               (cdr set2))]))

(displayln "exercise 2.60")
(define (slow-element-of-set? x set)
  (cond [(empty? set) #f]
        [(equal? x (car set)) #t]
        [else (element-of-set? x (cdr set))]))

(define slow-adjoin-set cons)
(define (slow-union-set set1 set2)
  (cond [(empty? set1) set2]
        [(empty? set2) set1]
        [else (slow-union-set (slow-adjoin-set (car set2) set1)
                        (cdr set2))]))

;; Intersection set suffers when there are lots of duplicates in the set being created.
;; If the set contains a number multiple times that is in the other set, all of those
;; values will be copied over. We never get to trim down our sets size. Adding is fast though
;; because we don't need to check for membership.
(define (intersection-set set1 set2)
                          (cond [(any? empty? set1 set2) '()]
                                [(element-of-set? (car set1) set2)
                                 (cons (car set1) (intersection-set (cdr set1) set2))]
                                [else (intersection-set (cdr set1) set2)]))


(displayln "exercise 2.61")
(define (sort-element-of-set? x set)
  (cond [(null? set) #f]
        [(= x (car set)) #t]
        [(< x (car set)) #f]
        [else (sort-element-of-set? x (cdr set))]))

;; Worst Case: Need to traverse entire list to add element O(n)
;; Average Case: Make it about half-way through the list before adding the element and
;; stopping
(define (sort-adjoin-set x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) (sort-adjoin-set x (cdr set)))]))

(displayln "exercise 2.62")
(define (sort-union-set set1 set2)
  (cond [(empty? set1) set2]
        [(empty? set2) set1]
        [(= (car set1) (car set2))
         (cons (car set1)
               (sort-union-set (cdr set1) (cdr set2)))]
        [(> (car set1) (car set2))
         (cons (car set2)
               (sort-union-set set1 (cdr set2)))]
        [else (cons (car set1)
                    (sort-union-set (cdr set1) set2))]))

(displayln "exercise 2.63")
(define (reduce proc accum lst)
  (if (empty? lst)
      accum
      (reduce proc
              (proc (car lst)accum)
              (cdr lst))))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-tree-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set? x (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set)))]))

(define (build-tree-set lst)
  (reduce (lambda (x set) (adjoin-tree-set x set))
          '()
          lst))
  
(define set-a (build-tree-set (list 5 2 8 1 3 9 7 6 4)))
(define set-b (build-tree-set (list 1 2 3 4 5 6 7 8 9)))
(define set-c (build-tree-set (list 9 8 7 6 5 4 3 2 1)))
(define fig-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define fig-2 '(3 (1 () ()) (7 (5 ()()) (9 () (11 () ())))))
(define fig-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (12 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; a. The produce the same results regardless of which one is used because they
;;    are both working through the tree in order.

;; b. tree->list-1
;;    Tree Traversal takes n time (there's no way around that), but since during each step we perform
;;    an append (the set is chopped in half each time though) we are going to need to take that into account.
;;    As was stated in SICP (or the lectures), when we see a function that reduces the space by an amount, we
;;    know that the method is O(log n) complexity. But, because we are doing this computation n times, we end
;;    up with a total complexity of O(n * log n)
;;    
;;    tree->list-2 does not suffer from the additional complexity because it simply appends builds out the list
;;    using cons, which has constant time complexity. Thus the overall complexity for this function is simply
;;    the time it takes to traverse the tree.

(displayln "exercise 2.64")
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (partial-tree elts left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n (+ left-size 1))]
             [this-entry (car non-left-elts)]
             [right-result (partial-tree (cdr non-left-elts) right-size)]
             [right-tree (car right-result)]
             [remaining-elts (cdr right-result)])
             (cons (make-tree this-entry left-tree right-tree)
                   remaining-elts))))

(list->tree '(1 2 3 4 5 6 7 8 9 10))
(list->tree '(1 3 5 7 9 11))

;; a. I'm using let* simply to reduce all the nesting that would be required when using just let.
;;    This does make things a bit confusing because the explicitness of the operations gets lost
;;    a little bit.  We want to converge to 0 as quickly as possible in order to get to our base
;;    case (left node is entry with no leaves).
;;    With that in place, we know that the next item in the list of unused elements is our element
;;    We then construct the right hand side of the tree. Again, we want to converge that side to 0
;;    as quickly as possible too.
;;    In a simple case (input '(1 2 3)) we end up building:
;;    -- the LHS with elts: '(1 2 3) n: ((3 - 1) / 2)
;;       -- the LHS with elts: '(1 2 3) n: (1 / 2) -> '() 
;;       -- entry 1
;;       -- the RHS with elts: '(2 3) n: (1 - (0 + 1)) -> '()
;;    <-- returns '((1 () ()) (2 3)) which we set to left-result and extract the data
;;    -- entry 2
;;    -- the RHS with elts: '(3) n: 3 - (1 + 1)
;;       -- the LHS with elts: '(3) n: (1 - 1) / 2 -> '()
;;       -- entry 3
;;       -- the RHS with elts: '(), n: 1 - (0 + 1) -> '()
;;    <-- returns '((3 () ()) '())
;; <-- returns '(2 (1 () ()) (3 () ()))
  
;; b. Again this procedure takes our lists and breaks them into two separate pieces;
;;    the left and right sides of the tree. But since we are performing that operation
;;    twice we end up not gaining any time savings during the traversal (2 * n / 2 is still n).
;;    Thankfully our make-tree function is a constant-time function, so we don't suffer any
;;    penalties because of that. So the performance of this function is O(n)

(displayln "exercise 2.65")
(define tree->list tree->list-2)
(define (balanced-union-set tree1 tree2)
  (list->tree (sort-union-set (tree->list tree1)
                              (tree->list tree2))))

(define (balanced-intersection-set tree1 tree2)
  (list->tree (intersection-set (tree->list tree1)
                                (tree->list tree2))))

(balanced-union-set (list->tree '(1 2 3 4 5 6 7 8 9 10))
                    (list->tree '(5 6 7 8 9 10 11 12 13 14)))

(balanced-intersection-set (list->tree '(1 2 3 4 5 6 7 8 9 10))
                           (list->tree '(5 6 7 8 9 10 11 12 13 14)))