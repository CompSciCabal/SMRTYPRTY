#lang racket
(require scheme/mpair)
(require compatibility/mlist)

#| 
Exercise 3.18
-------------
Write a procedure that examines a list and determines 
whether it contains a cycle, that is, whether a program that tried to 
find the end of the list by taking successive cdrs would go into an 
infinite loop. Exercise 3.13 constructed such lists.
|#
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define acons (mcons 1 2))
(define alist (mlist '1 '2 '3))
(define cycle (make-cycle (mlist '1 '2 '3)))

(define (join a-cons a-list) ; append a cons onto a list.
  (append a-list (list a-cons)))
(define (in? element list) ; boolean, element is in list
  (not (eq? (memq element list) false)))

; So annoying that all these procedures need to be type 
; specialized for mutable structures. Took way longer to 
; get this working than would otherwise. Probably should
; have rtfm'd.. 
(define (has-cycle? graph)
  (let ([visited-nodes '()])
    (define (inner node)
      (cond ((mpair? node) 
             (cond ((in? node visited-nodes) #t)
                   (else (begin 
                           (set! visited-nodes (join node visited-nodes))
                           (begin (inner (mcar node)) 
                                  (inner (mcdr node)))))))))                   
    (inner  graph)))  
(has-cycle? cycle)
