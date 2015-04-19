#lang racket
;; From SICP
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? 'leaf (car obj)))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree l r)
  (list l
        r
        (append (symbols l) (symbols r))
        (+ (weight l) (weight r))))

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

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x) (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let [(pair (car pairs))]
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let [(next-branch
               (choose-branch (car bits) current-branch))]
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= 0 bit) (left-branch branch)]
        [(= 1 bit) (right-branch branch)]
        [else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(displayln "exercise 2.67")
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

(displayln "exercise 2.68")
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (member? symbols)
    (cond [(empty? symbols) #f]
          [(eq? symbol (car symbols)) #t]
          [else (member? (cdr symbols))]))
  
  (cond [(leaf? tree) '()]
        [(member? (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree)))]
        [(member? (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree)))]
        [else (error "bad symbol -- ENCODE-SYMBOL" symbol)]))

sample-message
(encode '(A D A B B C A) sample-tree)

(displayln "exercise 2.69")
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (empty? (cdr leaves))
      (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves)
                                        (cadr leaves))
                   (cddr leaves)))))

(displayln "exercise 2.70")
(define rock-tree
  (generate-huffman-tree
   '((a 2) (na 16) (boom 1) (Sha 3)
     (Get 2) (yip 9) (job 2) (Wah 2))))
(define rock-msg
  '(Get a job
    Sha na na na na na na na na
    Get a job
    Sha na na na na na na na na
    Wah yip yip yip yip yip yip yip yip yip
    Sha boom))
(define encoded-rock (encode rock-msg rock-tree))
encoded-rock
(length encoded-rock)
;; To encode a message consisting of 8 "words" we would need log2 8 bits (3).
;; Our message is 36 words long, so we would need to use 108 bits to encode
;; this message

(displayln "exercise 2.71")
(displayln "see huffman-trees-a.png and huffman-trees-b.png")

(displayln "exercise 2.71")
;; In the worst case our message only contains least frequent symbol
;; Thankfully our search for the encoding is in a binary tree so the
;; number of steps it takes to encode a symbol half of the number of
;; elements in the tree (T). Thus simply doing is of O(log T).
;; We need to encode each item, and that we can't get around, so that will
;; be linear growth, O(n).
;; Since we need to make N traversals into our tree the overall growth is
;; n log n
