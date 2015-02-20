;; STILL using Chicken. What the fuck is wrong with me?
;;; 2.17
(define (last-pair a-list)
  (unless (null? a-list)
    (if (null? (cdr a-list))
        a-list
        (last-pair (cdr a-list)))))

;;; 2.18
(define (my-reverse a-list)
  (define (rec lst acc)
    (if (null? lst)
        acc
        (rec (cdr lst) (cons (car lst) acc))))
  (rec a-list '()))

;;; 2.19
;; Coin values is a list, so you can simply

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

;; or, if you want to be yappy about it, I guess ...
(define (first-denomination denoms)
  (car denoms))

(define (except-first-denominations denoms)
  (cdr denoms))

(define (no-more? denoms)
  (null? denoms))

;;; 2.20
(define (filter predicate a-list)
  (define (rec lst acc)
    (cond ((null? lst)
           (my-reverse acc))
          ((predicate (car lst))
           (rec (cdr lst) (cons (car lst) acc)))
          (else
           (rec (cdr lst) acc))))
  (rec a-list '()))

(define (same-parity . nums)
  (if (even? (car nums))
      (filter even? nums)
      (filter odd? nums)))

;;; 2.21
(define (square n) (expt n 2))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list items) (map square items))

;;; 2.22
;;   Because we're consing each element onto the accumulator, which adds it to
;; the front of the list.
;;   The second approach will cons the old list of results onto the new element.
;; That'll create a pair whose first element is the old accumulator and whose
;; second element is the new result.
;;   What you'd actually want is either to `append` the new result to the end of the
;; list (which performs very shittily for singly-linked lists)
;;   -- or --
;;   Reverse your result when returning the accumulator
;;   -- or --
;;   Hang onto a pointer to the last cons in your list and push to it each time
;; instead of consing to the front.

;;; 2.23
(define (for-each fn lst)
  (if (null? lst)
      #t
      (begin (fn (car lst))
             (for-each fn (cdr lst)))))

;;; 2.24

;; (1 (2 (3 4)))

;; [ | ] -> [ | ] -> NIL
;;  |        |
;;  1       [ | ] -> [ | ] -> NIL
;;           |        |
;;           2       [ | ] -> [ | ] -> NIL
;;                    |        |
;;                    3        4

;; (1 (2 (3 4)))
;;  /	\
;; 1        (2 (3 4))
;;           /   \
;;          2    (3 4)
;;                / \
;;               3   4

;;; 2.25

;; > (car (cdr (car (cdr (cdr '(1 2 (5 7) 9))))))
;; 7

;; > (car (car '((7))))
;; 7

;; > (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
;; 7

;;; 2.26

;; > (append x y)
;; (1 2 3 4 5 6)

;; > (cons x y)
;; ((1 2 3) 4 5 6)

;; > (list x y)
;; ((1 2 3) (4 5 6))

;;; 2.27
(define (deep-reverse a-tree)
  (define (rec tree acc)
    (cond ((null? tree) acc)
          ((pair? (car tree))
           (rec (cdr tree) (cons (rec (car tree) '()) acc)))
          (else
           (rec (cdr tree) (cons (car tree) acc)))))
  (rec a-tree '()))

;;; 2.28
(define (fringe tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (fringe (car tree)) (fringe (cdr tree))))
        (else
         (cons (car tree) (fringe (cdr tree))))))

;;; 2.29

;; a)
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b)
(define (branch-weight branch)
  (total-weight (branch-structure branch)))

(define (total-weight structure)
  (if (number? structure)
      structure
      (+ (branch-weight (left-branch structure))
         (branch-weight (right-branch structure)))))

;; c)
(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? structure)
  (let ((l (left-branch structure))
        (r (right-branch structure)))
    (if (number? structure)
        #t
        (and (= (torque l) (torque r))
             (balanced? (branch-structure l))
             (balanced? (branch-structure r))))))

;; d)
;; It depends on how honest you were about not reaching beneath the selector abstraction.
;; If you were, you'll need to change the basic selectors.
;; If you weren't, you'll need to change any function that resorted directly to representation-specific selectors.
;; If you were asking about me specifically, I was honest so I won't have to change much.

;;; 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((atom? tree) (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-too tree)
  (map (lambda (t) ;;godDAMN it feels good to do that. I think I'll do it again;
                   ;; (lambda (t)
                   ;;   (print t)
                   ;;   (print "That variable was named 't'")
                   ;;   (print "U Jelly, Common Lisp?"))
         (if (pair? t)
             (square-tree t)
             (square t)))
       tree))

;;; 2.31
(define (tree-map fn tree)
  (map (lambda (t)
         (if (pair? t)
             (tree-map fn t)
             (fn t)))
       tree))

;;; 2.32
;; The template they hand us is

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map ?? rest)))))

;; Which I have to mildly modify for Chicken

;; (define (subsets s)
;;   (if (null? s)
;;       (list '()) ;; <- that right there
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map ?? rest)))))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (elem)
                       (cons (car s) elem))
                     rest)))))

;;   You want to append all the subsets of s not containing (car s)
;; to the set of all subsets that do include (car s)