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