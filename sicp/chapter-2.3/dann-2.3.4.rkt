#lang racket

;;; PRELUDE

(define (make-leaf symbol weight) (list 'leaf symbol weight)) 
(define (leaf? object) (eq? (car object) 'leaf)) 
(define (symbol-leaf x) (cadr x)) 
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree)) 
(define (symbols tree)
  (if (leaf? tree) 
      (list (symbol-leaf tree)) 
      (caddr tree)))
(define (weight tree) 
  (if (leaf? tree)
      (weight-leaf tree) 
      (cadddr tree)))

(define (make-code-tree left right) 
  (list left 
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))

(define (choose-branch bit branch) 
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch)) 
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree) 
  (define (decode-1 bits current-branch)
    (if (null? bits) 
        '()
        (let ((next-branch 
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch) 
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)) 
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set) 
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs) 
  (if (null? pairs)
      '() 
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) 
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree 
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                   (make-leaf 'B 2) 
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1))))) 
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree) 
  (if (null? message)
      '() 
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;; Ex 2.67

; (decode sample-message sample-tree)
;   '(A D A B B C A)

;;; Ex 2.68

(define (encode-symbol symbol tree)
 (let ((left (left-branch tree))
       (right (right-branch tree)))
   (cond ((memq symbol (symbols left))
          (if (leaf? left)
              '(0)
              (cons 0 (encode-symbol symbol left))))
         ((memq symbol (symbols right)) 
          (if (leaf? right)
              '(1)
              (cons 1 (encode-symbol symbol right))))
         (else (error "bad symbol" '(symbol tree))))))

; (encode '(A D A B B C A) sample-tree)
;   '(0 1 1 0 0 1 0 1 0 1 1 1 0)

;;; Ex 2.69

(define (generate-huffman-tree pairs) 
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  ; (define (leafsort a b) 
  ;   (sort '(a b) (lambda (a b) (< (weight a) (weight b)))))
  (if (eq? 1 (length pairs))
      (car pairs)
      (successive-merge (adjoin-set 
                         (make-code-tree (car pairs) (cadr pairs)) 
                         (cddr pairs)))))

; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;   '((leaf A 4)
;     ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4)
;     (A B D C)
;     8)

;;; Ex 2.70

; (define rocktree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

; (define rocksong '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

; (encode rocksong rocktree)
;   '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0)

; 84 bits. 4 * 36 = 144 bits for fixed-width encoding. (58%!)

;;; Ex 2.71

; It's the most unbalanced binary tree (linear). 
; 1 bit for most frequent symbol, N-1 bits for least frequent.

;;; Ex 2.72

; most frequent: takes 1 step (i.e. O(1)), because there's only 1 symbol in the left list
; -- unless it ends up on the right, in which case it can take N steps to search the list.

; least frequent: it's O(n^2), because you take n-1 steps down the tree,
; searching through [~n .. 1] symbols each time in the worst case.
; in the best case, the least-frequent symbol is first in the list,
; and it only takes ~n steps. 
