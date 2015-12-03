#lang racket
#|
Exercise 4.25
=============

Suppose that (in ordinary applicative-order Scheme) we define unless as shown above
and then define factorial in terms of unless as

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

What happens if we attempt to evaluate (factorial 5)? Will our definitions work in a
normal-order language? 

(define (unless condition 
                usual-value 
                exceptional-value)
  (if condition 
      exceptional-value 
      usual-value))

Answer
======

Because normal scheme is eager this never terminates, because `unless` always evals
usual-values, even if condition is true.


Exercise 4.26
=============

Ben Bitdiddle and Alyssa P. Hacker disagree over the importance of lazy evaluation
for implementing things such as unless. Ben points out that it’s possible to
implement unless in applicative order as a special form. Alyssa counters that, if
one did that, unless would be merely syntax, not a procedure that could be used in
conjunction with higher-order procedures. Fill in the details on both sides of the
argument. Show how to implement unless as a derived expression (like cond or let),
and give an example of a situation where it might be useful to have unless available
as a procedure, rather than as a special form.

Answer
======



Exercise 4.27
=============

Suppose we type in the following definitions to the lazy evaluator:

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

Give the missing values in the following sequence of interactions, and explain
your answers.

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
Response: is 0, bec. `w` hasn't been eval'd yet.


;;; L-Eval input:
w

;;; L-Eval value:
Response: 10

;;; L-Eval input:
count

;;; L-Eval value:
Response: 2

|#

