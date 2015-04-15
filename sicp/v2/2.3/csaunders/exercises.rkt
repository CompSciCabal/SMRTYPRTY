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