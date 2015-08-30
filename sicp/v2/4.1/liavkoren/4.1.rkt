#lang racket
#|
Holy crap, this is elegant:

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

 -- https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html

"In most Lisp implementations, dispatching on the type of an expression 
is done in a data-directed style. This allows a user to add new types of 
expressions that eval can distinguish, without modifying the definition 
of eval itself. (See exercise 4.3.)
|#

#|
Exercise 4.1
============  

Notice that we cannot tell whether the metacircular evaluator evaluates
operands from left to right or from right to left. Its evaluation order is 
inherited from the underlying Lisp: If the arguments to cons in list-of-values 
are evaluated from left to right, then list-of-values will evaluate operands 
from left to right; and if the arguments to cons are evaluated from right to 
left, then list-of-values will evaluate operands from right to left.

Write a version of list-of-values that evaluates operands from left to right 
regardless of the order of evaluation in the underlying Lisp. Also write a 
version of list-of-values that evaluates operands from right to left. 

Note:
This seems like a weird question, not sure if I 100% understand what they are
asking for.
|#


; orginal version:
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))  

(define (list-of-values-l2r exps env)
  (define (inner acc rest)
    (if (null? rest) acc
        (inner (cons acc '()) (cdr rest))))
  (if (no-operands? exps)
      '()
      (inner (eval (first-operand exps) env)
             (list-of-values (rest-operands exps) env))))

(define (reverse items)
  (define (inner acc remainder)
    (cond ((null? remainder) acc) ; finished reversing
          ((eq? (car remainder) (car items)) (inner (cons (car remainder) '()) (cdr remainder))) ; at the start of reversing
          (else (inner (cons (car remainder) acc) (cdr remainder))))) ; in the middle of reversing
  (inner '() items)) 
  
; To do a right2left version, just compose the l2r version above with reverse. 
  
