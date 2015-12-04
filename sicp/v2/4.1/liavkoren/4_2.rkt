#lang racket
#|
Exercise 4.2
============

Louis Reasoner plans to reorder the cond clauses in eval so
that the clause for procedure applications appears before the clause for
assignments. He argues that this will make the interpreter more efficient:
Since programs usually contain more applications than assignments,
definitions, and so on, his modified eval will usually check fewer clauses
than the original eval before identifying the type of an expression

b. Louis is upset that his plan didn't work. He is willing to go to any
lengths to make his evaluator recognize procedure applications before it
checks for most other kinds of expressions. Help him by changing the syntax of
the evaluated language so that procedure applications start with call. For
example, instead of (factorial 3) we will now have to write (call factorial 3)
and instead of (+ 1 2) we will have to write (call + 1 2).
|#

; original version:
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env)) ; <-- assignment
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)  ; <-- Louis proposes to move this above assignment.
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
#|
Answers
=======

a) Definitions and applications are both implemented as tagged list, so eval will 
attempt to apply the definition expression. This means that eval will attempt to 
do (eval define env). Define is a special form and this will probably raise an
"Unknown expression type" error within eval, or within LISP itself. We will also
fail to modify the environment we intended to, with the name/value binding that
we wanted. 
 

b) To make application be (call + 1 2) we need to update the application relevant
procedures. Original versions:
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
|#

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

#|
Exercise 4.4
============

Recall the definitions of the special forms and and or from chapter 1:

  - and: The expressions are evaluated from left to right. If any expression
    evaluates to false, false is returned; any remaining expressions are not
    evaluated. If all the expressions evaluate to true values, the value of
    the last expression is returned. If there are no expressions then true is
    returned.

  - or: The expressions are evaluated from left to right. If any expression
    evaluates to a true value, that value is returned; any remaining
    expressions are not evaluated. If all expressions evaluate to false, or if
    there are no expressions, then false is returned.
    

Install and and or as new special forms for the evaluator by defining
appropriate syntax procedures and evaluation procedures eval-and and eval-or.
Alternatively, show how to implement and and or as derived expressions.
|#

; probably cheating to use filter, but whatevs. 
(define (eval-and exp)
  (if (eq? '() (filter (lambda (x) (eq? x #f)) exp)) #t #f))

(define (eval-or exp)
  (if (filter (lambda (x) (eq? x #f)) exp) #f #t))

#|
Exercise 4.5
============  

Scheme allows an additional syntax for cond clauses, (<test> => <recipient>).
If <test> evaluates to a true value, then <recipient> is evaluated. Its value
must be a procedure of one argument; this procedure is then invoked on the
value of the <test>, and the result is returned as the value of the cond
expression. For example

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

returns 2. Modify the handling of cond so that it supports this extended
syntax. 
|#

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))