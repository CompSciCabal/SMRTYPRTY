#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x)) ;; car cdr
(define (weight-leaf x) (caddr x)) ;; car cdr cdr

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

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

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

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
                               ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))


;; Exercise 2.67 
;; Use the decode procedure to decode the message, and give the result!
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; (decode sample-message sample-tree) => '(A D A B B C A)

;; Exercise 2.68 
;; Implement encode-symbol such that it returns an error if the symbol is not
;; in the tree.
(define m (decode sample-message sample-tree)) ;; '(A D A B B C A)

(define (encode-symbol-1 symbol tree)
  (cond
    [(leaf? tree) null]
    [(member symbol (symbols (left-branch tree)))
          (append '(0) (encode-symbol-1 symbol (left-branch tree)))]
    [else 
     (append '(1) (encode-symbol-1 symbol (right-branch tree)))]))

(define (encode-symbol s t)
  (cond
    [(not (member s (symbols t))) (error "bad bit!")]
    [else (encode-symbol-1 s t)]))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
  
;; Exercise 2.69
(define (successive-merge) 'what)