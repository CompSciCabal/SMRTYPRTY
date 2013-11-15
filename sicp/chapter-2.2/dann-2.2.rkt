#lang racket

;;; Reading Notes

; 133.6: "closure" (losing battle) 
;        [can everything in a language be closed?]
; 137.10: nil wars
; 142.11: It seems a little strange that (define g (lambda w w))
;         captures its arguments as a list...
; 144: what are some other common abstraction barriers like map? 
;      how do the different ways of decomposing abstraction effect
;      our ability to effectively reason about different problem types?



;;; PRELUDE



;;; 2.17

(define (last-pair xs)
  (let ((x (car xs)))
    (if (null? (cdr xs))
        x
        (last-pair (cdr xs)))))

;;; 2.18

(define (reverse xs)
  (define (rev-iter xs acc)
    (if (null? xs)
        acc
        (rev-iter (cdr xs) (cons (car xs) acc))))
  (rev-iter xs '()))

;;; 2.19

(define (cc amount coin-values) 
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount 
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination 
                    coin-values))
                coin-values)))))

(define (first-denomination xs) (car xs))
(define (except-first-denomination xs) (cdr xs))
(define (no-more? xs) (null? xs))

; Order is unimportant in the denomination list: at each step we 
; try making change with a denom and without it, so eventually 
; every denom is represented equally.

;;; 2.20

(define (same-parity . w)
  (define (filter-by-mod base mod xs acc)
    (if (null? xs) 
        acc
        (filter-by-mod base mod (cdr xs) 
                       (if (= (remainder (car xs) base) mod)
                           (cons (car xs) acc)
                           acc))))
  (filter-by-mod 2 (remainder (car w) 2) (cdr w) '()))

; Q: how do you do two things in an 'else' statement?

;;; 2.21

(define (square n)
  (* n n))

(define (square-list items) 
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items))))) 

(define (square-list2 items)
  (map square items))
    
;;; 2.22

; Q: evolves an iterative process?

; we're popping off items the front of things
; onto the front of answer, which reverses it

; cons'ing a list onto a single item gives a pair of '(list item)

;;; 2.23

(define (foreach fun xs)
  (if (null? xs)
      #t
      (and 
       (fun (car xs))
       (foreach fun (cdr xs)))))

;;; 2.24

; can we generalize the types of tree recursion
; (i.e. fun leaves, fun branches, fun both, ... )

; Q: why does pair? return #t for '(1 2 3) and '(1) but not '() ???

; > (list 1 (list 2 (list 3 4)))
; '(1 (2 (3 4)))
; [this margin is too small]

;;; 2.25

; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
; (cadr (caddr '(1 3 (5 7) 9)))
; (caar '((7)))
; (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; 2.26

; > (append x y)
; '(1 2 3 4 5 6)
; > (cons x y)
; '((1 2 3) 4 5 6)
; > (list x y)
; '((1 2 3) (4 5 6))

;;; 2.27

; (define (deep-reverse xs)

;;; 2.28

; (define (fringe xs)

;;; 2.29

; OO for selectors... encapsulate the data with the abstraction layer

; why doesn't the top have a rod?
; what does length+number mean? is it a weighted rod?

;;; 2.30




