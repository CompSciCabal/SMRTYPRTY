;;; Givens
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

;;; 2.59
(define (remove-duplicates set)
  (define (rec elems acc)
    (cond ((null? elems) acc)
          ((element-of-set? (car elems) (cdr elems))
           (rec (cdr elems) acc))
          (else
           (rec (cdr elems) (cons (car elems) acc)))))
  (rec set '()))

(define (union-set set-a set-b)
  (remove-duplicates (append set-a set-b)))

;;; 2.60
; a)
;; element-of-set? can keep the same representation, actually.
;; We still just care about the presence or absence of an element
(define (adjoin-set x set)
  (cons x set))

(define (union-set set-a set-b)
  (append set-a set-b))

(define (intersection-set set-a set-b)
  (cond ((or (null? set-a) (null? set-b)) '())
        ((element-of-set? (car set-a) set-b)
         (cons (car set1) (intersection-set (cdr set-a) set-b)))
        (else (intersection-set (cdr set-a) set-b))))
;; Ok, I typed that out line by line, but the original definition of intersection-set given in the book will still work fine with this other implementation

; b)
;; adjoin-set works in constant time instead of linear time. union-set works much better now, since my implementation of remove-duplicates is utter garbage performance-wise. The others didn't change, so they're obviously the same in performance terms.

; c)
;; Trivially, if there was something I needed to do with sets that involved lots of insertion and no lookup, the second representation would be fine. Can't really think of that one.

;;; More Givens
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; 2.61
(define (adjoin-set elem set)
  (if (element-of-set? elem set)
      set
      (sort (cons elem set))))

;; oh, wait. I'm supposed to make this more efficient, I guess?

(define (adjoin-set elem set)
  (define (rec rest)
    (cond ((null? rest)
           (list elem))
          ((< elem (car rest))
           (cons elem rest))
          (else
           (cons (car rest) (rec (cdr rest))))))
  (if (element-of-set? elem set)
      set
      (rec set)))

;;; 2.62
(define (union-set set-a set-b)
  (cond ((null? set-a)
         set-b)
        ((null? set-b)
         set-a)
        ((< (car set-a) (car set-b))
         (cons (car set-a) (union-set (cdr set-a) set-b)))
        ((> (car set-a) (car set-b))
         (cons (car set-b) (union-set set-a (cdr set-b))))
        (else
         (union-set set-a (cdr set-b)))))