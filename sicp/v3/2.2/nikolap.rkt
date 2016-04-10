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

;; 2.25

;; 2.26

;; 2.27

;; 2.28

;; 2.29

;; 2.30

;; 2.31

;; 2.32
