;; 2.17
(define (last-pair list1)
  (if (null? (cdr list1))
    (list (car list1))
    (last-pair (cdr list1))))

;; 2.18
(define (reverse items)
 (define (reverse-iter out rest-items)
  (if (null? rest-items)
   out
   (reverse-iter (cons (car rest-items) out) (cdr rest-items))))
 (reverse-iter nil 1))

;; 2.19
(define (first-denomination coins)
 (car coins))

(define (except-first-denomination coins)
 (cdr coins))

(define (no-more? coins)
 (null? coins))

(define (cc amount coin-values)
 (cond ((= amount 0) 1)
  ((or (< amount 0) (no-more? coin-values)) 0)
  (else
    (+ (cc amount
        (except-first-denomination coin-values))
       (cc (- amount
            (first-denomination coin-values))
           coin-values)))))

;; 2.20
(define (even? n)
 (= (remainder n 2) 0))

(define (odd? n)
 (not (even? n)))

(define (same-parity x y . z)
 (define (sp-iter rest-items out)
  (if (null? rest-items)
   out
   (sp-iter (cdr rest-items)
    (if (or
         (and (even? x) (even? (car rest-items)))
         (and (odd? x) (odd? (car rest-items))))
        (cons out (car rest-items))
        out))))
 (iter z (list x)))

;; 2.21

(define (square-list-alt items)
 (if (null? items)
  nil
  (cons (* (car items) (car items) (square-list (cdr items))))))

(define (square-list items)
 (map (lambda (x) (* x x)) items))

;; 2.22
;; Louis Reasoner's fn cons the squared value to the beginning of the collection
;; i.e. it is (cons square answer) instead of (cons answer square), that way
;; whatever the answer is will always be pushed back. The alternative also fails
;; because he is consing a list onto a value which righly produces a list of list
;; of .. you get the gist. Instead he should append instead of cons

;; 2.23
(define (for-each proc items)
 (when (not (null? items))
  (proc (car items))
  (for-each proc (cdr items)))
 true)

;; 2.24
;; '(1 '(2 '(3 4)))
;; Let's forgo the diagrams on this document :)

;; 2.25
(cadr ;; 7
  (cadr ;; (5 7)
   (cdr ;; 3 (5 7)..
           '(1 3 '(5 7) 9))))

(cadr (cadr (cadr (cadr (cadr (cadr '(1 '(2 '(3 '(4 '(5 '(6 '(7)))))))))))) ;; 6x cadr

;; 2.26
;; (append x y) => '(1 2 3 4 5 6)
;; (cons x y) => '(1 2 3 '(4 5 6))
;; (list x y) => '('(1 2 3) '(4 5 6))

;; 2.27
;; Abstraction for 2.27, 2.28, 2.30
(define (tree-recursion coll if-empty if-pair if-else)
 (cond ((null? coll) if-empty)
  ((pair? coll) (if-pair coll))
  (else (if-else coll))))

(define (deep-reverse coll)
 (tree-recursion
   coll
   nil
   (lambda (l) (append (deep-reverse (cdr l))
                       (list (deep-reverse (car l)))))
   (lambda (l) (append (deep-reverse (cdr l)) (list (car l))))))

;; 2.28
(define (fringe coll)
 (tree-recursion
   coll
   nil
   (lambda (l) (append (fringe (car coll)) (fringe (cdr coll))))
   (lambda (l) (list coll))))

;; 2.29
; a)
(define (left-branch mobile)
 (car mobile))

(define (right-branch mobile)
 (cadr mobile))

(define (branch-length branch)
 (car branch))

(define (branch-structure branch)
 (cadr branch))

 ; b)
(define (branch-weight branch)
 (if (pair? (branch-structure branch))
  (total-weight (branch-structure branch))
  (branch-structure branch)))

(define (total-weight mobile)
 (+ (branch-weight (left-branch mobile))
  (branch-weight (right-branch mobile))))

; c)
(define (branch-torque branch)
 (* (branch-length branch)
  (branch-weight branch)))

(define (branch-balanced? branch)
 (or (not (pair? (branch-structure branch)))
  (mobile-balanced? (branch-structure branch))))

(define (mobile-balanced? mobile)
 (and (= (branch-torque (left-branch mobile))
         (branch-torque (right-branch mobile)))
      (branch-balanced? (left-branch mobile))
      (branch-balanced? (right-branch mobile))))

; d)
;; only the selectors (left-branch, right-branch, branch-length, branch-structure) need to be changed

;; 2.30
(define (square-tree-direct tree)
 (tree-recursion
   tree
   nil
   (lambda (t)
    (cons (square-tree-direct (car tree))
          (square-tree-direct (cdr tree))))
   (lambda (t) (* t t))))

(define (square-tree-map tree)
 (map (lambda (t)
       (if (pair? t)
        (square-tree-map t)
        (* t t)))
      tree))

;; 2.31
;; whoohoo better abstraction than my earlier tree-recursion fn
(define (tree-map proc tree)
 (cond ((null? tree) null)
       ((pair? tree) (cons (tree-map proc (car tree))
                           (tree-map proc (cdr tree))))
       (else (proc tree)))

;; 2.32
(define (subsets s)
 (if (null s)
  (list nil) ;; if s is nil, then subsets are an empty list
  (let ((rest (subsets (cdr s))))
   (append rest (map
                 (lambda (r)
                  (cons (car s) r))
                 rest)))))
;; let's walk through an example
;; (subset (1 2 3))
;; rest => (subset (2 3))
;;          rest => (subset (3))
;;                rest => ()
;; the appending is to append the last empty rest to the mapped coll (an empty set is still a set!)
;;                    (cons 2 nil) => 2
;;                (cons 3 nil) => 3
;;         (cons 2 3) => 2 3
;;    (cons 1 nil) => 1
;;    (cons 1 3) => 1 3
;;    (cons 1 2) => 1 2
;;    (cons (1 2) 3) => 1 2 3
;; then just append the empty set and voila
