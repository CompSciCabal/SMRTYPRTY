#lang racket

;; -------------------------------------------------------------------
;; Huffman Encoding Trees
;; -------------------------------------------------------------------

;; Observation: None of the examples/exercises below
;; uses higher order functions.

;; Leaf representation

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? node)
  (eq? 'leaf (car node)))

(define symbol-leaf cadr)

(define weight-leaf caddr)

;; Tree representation

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)

(define right-branch cadr)

;; Generic procedures

(define (symbols node)
  (if (leaf? node)
      (list (symbol-leaf node))
      (caddr node)))

(define (weight node)
  (if (leaf? node)
      (weight-leaf node)
      (cadddr node)))

;; Decoding messages

(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= 0 bit) (left-branch branch))
          ((= 1 bit) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits)
                                          current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; Tests. Exercise 2.67, p.167

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(equal? '(A D A B B C A)
        (decode sample-message sample-tree))

;; Encoding messages. Exercise 2.68, p.167

;; it would be shorter with flatmap/mapcan
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (choose-branch branch)
    (let ((left (left-branch branch)))
      ; If symbol is not in the left branch
      ; it has to be in the right. See comment below.
      (if (member symbol (symbols left))
          (values left 0)
          (values (right-branch branch) 1))))

  (define (encode-bit branch)
    (if (leaf? branch)
        '()
        (let-values (((next-branch bit) (choose-branch branch)))
          (cons bit (encode-bit next-branch)))))

  ; It suffice to check the validity of the symbol only once,
  ; since the root node contains all valid symbols by construction.
  ; Due to that we don't need to search the symbol list from the
  ; right branch on every step in CHOOSE-BRANCH procedure.
  (if (member symbol (symbols tree))
      (encode-bit tree)
      (error "unknown symbol -- ENCODE-SYMBOL" symbol)))

;; Tests

(equal? sample-message (encode '(A D A B B C A) sample-tree))

;; Building Huffman trees

;; insert into list sorted by weight
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; ((A 2) (B 1)) -> sorted set of leaves
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (adjoin-set (make-leaf (caar pairs) (cadar pairs))
                  (make-leaf-set (cdr pairs)))))

;; Exercise 2.69, p.168
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (let ((left (car leaves)))
    (if (null? (cdr leaves))
        left
        (successive-merge
         (adjoin-set (make-code-tree left (cadr leaves))
                     (cddr leaves))))))

;; Tests. Exercise 2.70, p.168

(define frequencies
  '((a 2)
    (boom 1)
    (get 2)
    (job 2)
    (na 16)
    (sha 3)
    (yip 9)
    (wah 1)))

(define rock-tree
  (generate-huffman-tree frequencies))

(define song
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))

(define encoded-song
  '(1 1 1 1 1 1 1 0
    0 1 1 1 1 0 1 1
    1 0 0 0 0 0 0 0
    0 0 1 1 1 1 1 1
    1 0 0 1 1 1 1 0
    1 1 1 0 0 0 0 0
    0 0 0 0 1 1 0 1
    0 1 0 1 0 1 0 1
    0 1 0 1 0 1 0 1
    0 1 0 1 1 1 0 1
    1 0 1 1))

(equal? encoded-song
        (encode song rock-tree))

(define (log2 x)
  (/ (log x) (log 2)))

(= 84 (length encoded-song))

(= 108 (* (length song)
          (ceiling (log2 (length frequencies)))))


;; Exercise 2.71, p.168
;; Alphabet: 1, 2, ..., n. Frequencies: 1, 2, ..., 2^{n-1}.
;;
;; Q: How many bits are required to encode the moset frequent
;; symbol? the least frequent symbol?
;;
;; A: 1, n-1


;; Exercise 2.72, p.169
;; Same encoding tree as in the previous exercise.
;;
;; Q: What is the order of growth (as a function of n) in
;; the number of steps needed to encode a symbol?
;;
;; A: n+1 = Θ(n)
;; To encode symbol K, 1 ≤ K ≤ N, it requires
;; K steps - check the validity of the symbol,
;; 1 * (N - K + 1) steps - choose-branch calls.
