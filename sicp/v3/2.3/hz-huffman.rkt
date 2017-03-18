#lang racket

(require test-engine/racket-tests)

;;--- 2.67 --------------------------------------------------------------------
;;
;; (define sample-tree
;;   (make-code-tree (make-leaf 'A 4)
;;                   (make-code-tree
;;                   (make-leaf 'B 2)
;;                   (make-code-tree (make-leaf 'D 1)
;;                                   (make-leaf 'C 1)))))
;;
;; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;
;; Using the described algorithm, sample-message decodes to '(A D A B B C A)).
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

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
    (if (empty? bits)
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

(define (encode message tree)
  (if (empty? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
;;-----------------------------------------------------------------------------


;;--- 2.68 --------------------------------------------------------------------
(define (encode-symbol s tree)
  (define (encode-symbol-helper s tree)
    (cond ((empty? tree) #f)
          ((leaf? tree) (if (eq? (symbol-leaf tree) s) empty #f))
          ((not (memq s (symbols tree))) #f)
          (else
           (define result-left (encode-symbol-helper s (left-branch tree)))
           (define result-right (encode-symbol-helper s (right-branch tree)))
           (cond (result-left (cons 0 result-left))
                 (result-right (cons 1 result-right))
                 (else #f)))))
  (define result (encode-symbol-helper s tree))
  (if (not result) (error "symbol not in tree -- ENCODE-SYMBOL" s) result))
;;-----------------------------------------------------------------------------

;;--- 2.68 tests --------------------------------------------------------------
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(check-expect (encode (decode sample-message sample-tree) sample-tree) sample-message)
(check-error (encode '(A B C X) sample-tree) "symbol not in tree -- ENCODE-SYMBOL X")
;;-----------------------------------------------------------------------------


;;-----------------------------------------------------------------------------
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
;;-----------------------------------------------------------------------------


;;--- 2.69 --------------------------------------------------------------------
(define (successive-merge nodes)
    (cond ((empty? (cdr nodes)) (car nodes))
          (else
            (successive-merge
              (adjoin-set (make-code-tree (car nodes) (cadr nodes))
                          (cddr nodes))))))
;;-----------------------------------------------------------------------------

;;--- 2.69 tests --------------------------------------------------------------
(check-expect (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
              (make-code-tree (make-leaf 'A 4)
                              (make-code-tree (make-leaf 'B 2)
                                              (make-code-tree (make-leaf 'D 1)
                                                              (make-leaf 'C 1)))))
;;-----------------------------------------------------------------------------


;;--- 2.70 --------------------------------------------------------------------
(define tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3)
                                      (GET 2) (YIP 9) (JOB 2) (WAH 1))))
(define message '(GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                  SHA BOOM))

(printf "~a bits are needed to encode this message using the Huffman encoding.\n" (length (encode message tree)))
(printf "~a bits would be needed to encode this message using a fixed-length code.\n" (* 3 (length message)))
;;-----------------------------------------------------------------------------


;;--- 2.71 --------------------------------------------------------------------
;;
;; Consider the steps in the Huffman tree generation algorithm:
;;
;; 1) Find the two smallest weighted nodes and combine them into a new node.
;; 2) Remove these nodes from the list and replace them with the new node.
;; 3) Recurse with the new list.
;;
;; Note: Because all of the weights are successive powers of 2, the lowest weight
;; in the list is either 2^0 = 1, or is a sum of successive powers of 2.
;; Either way, the lowest weight in the list at any time will have the form
;; 2^k - 1 for some k > 0.
;;
;; Suppose the lowest weight in the list is Σi=0,k (2^i) = (2^k)-1. Then, the new
;; node created in the first two steps will have a weight of (2^k)-1 + 2^k, and the
;; lowest weight in the remaining list (i.e. with the lowest two elements removed)
;; will be 2^(k+1). But note that
;;   (2^k)-1 + 2^k = 2*2^k - 1 = 2^(k+1) - 1 < 2^(k+1)
;;
;; Thus, the new node added in step 2 will always be the lowest weighted node in
;; the new list. Thus, the shape of the tree will look something like the following:
;;                               [s_1,...,s_n]
;;                                /         \
;;                     [s_1,...,s_n-1]   (leaf s_n 2^(n-1))
;;                      /            \
;;                 [s_1,...,s_n-2]  (leaf s_n-1 2^(n-2))
;;                 /            \
;;            [s_1,...,s_n-3] (leaf s_n-2 2^(n-3))
;;
;; ...etc.
;;
;; I.e., a very unbalanced tree.
;; Thus, it will take one bit to encode the most frequently used symbol (s_n), and
;; n-1 bits (the height of the tree) to encode the least frequently used symbol (s_1).
;;-----------------------------------------------------------------------------


;;--- 2.72 --------------------------------------------------------------------
(define (encode-symbol-with-output s tree)
  (define (encode-symbol-helper s tree)
    (printf "(encode-symbol-helper ~a ~a)\n" s tree)
    (cond ((empty? tree) #f)
          ((leaf? tree) (if (eq? (symbol-leaf tree) s) empty
                            (begin
                              (printf "Reached a leaf but it was not ~a\n" s)
                              #f)))
          ((begin
             (printf "Evaluating (memq ~a ~a)\n" s (symbols tree))
              (not (memq s (symbols tree))))
           (printf "Stopping recursion at (encode-symbol-helper ~a ~a) because the symbol is not in this branch\n" s tree)
           #f)
          (else
           (define result-left (encode-symbol-helper s (left-branch tree)))
           (define result-right (encode-symbol-helper s (right-branch tree)))
           (cond (result-left (cons 0 result-left))
                 (result-right (cons 1 result-right))
                 (else #f)))))
  (define result (encode-symbol-helper s tree))
  (if (not result) (error "symbol not in tree -- ENCODE-SYMBOL" s) result))

;; Above is the encode-symbol function defined for exercise 2.68, with some
;; output added to help trace the recursive calls.
;;
;; In the case of the most frequent symbol, first (memq s tree) will be evaluated
;; at the root, which will take O(n) steps. There will be exactly two recursive
;; calls made, one to each branch of the tree. The left recursion will stop after
;; one step because (not (memq s tree)) will produce true, since the symbol is in
;; the right branch of the root, and therefore not in the list of symbols for the
;; left branch. The right recursion will also stop after one step, without calling
;; memq, since it will be at the leaf that contains the most frequent symbol. Thus,
;; encoding the most frequent symbol takes O(n) + O(n) + O(1) = O(n) steps.
;;
;; Encoding the least frequent symbol will make n-1 left-recursive calls, each of
;; which will make a call to memq that takes in general O(n) steps to evaluate.
;; Each call will also generate a right-recursion, which will again stop after
;; one step because it will be at a leaf that is not the intended symbol. Thus, we
;; have n-1 left recursions that each take O(n) steps, and n-1 right recursions that
;; each take O(1) steps, to get a total of (n-1) * O(n) + (n-1) * O(1) = O(n^2).
;;
;; Note: The above holds if we make no assumptions about how order is preserved in the
;; symbol list for each node. If the list is ordered from lowest to highest weight (as
;; in fact the given implementation will construct), the calls to memq will actually only
;; take O(1) time, since it will find the symbol after looking at only one element in the
;; list. Thus, __depending on how the symbol list is constructed__, encoding the least
;; frequent symbol could be as good as n * O(1) + (n-1) * O(1) = O(n).

(define T (generate-huffman-tree '((A 32) (B 16) (C 8) (D 4) (E 2) (F 1))))
(displayln "Encoding A...")
(encode-symbol-with-output 'A T)
(displayln "Encoding F...")
(encode-symbol-with-output 'F T)
;;-----------------------------------------------------------------------------

(test)
