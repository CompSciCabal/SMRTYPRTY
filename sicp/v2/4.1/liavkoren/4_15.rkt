#lang racket

#|
Exercise 4.15
=============

Given a one-argument procedure p and an object a, p is said to ``halt'' 
on a if evaluating the expression (p a) returns a value (as opposed to 
terminating with an error message or running forever). Show that it is 
impossible to write a procedure halts? that correctly determines whether 
p halts on a for any procedure p and object a. Use the following 
reasoning: If you had such a procedure halts?, you could implement the 
following program:
|#
(define (run-forever) (run-forever)) ; an infinite loop

(define (try p)  
  (if (halts? p p)  ; does p halt on p? 
      (run-forever) ; if yes, don't terminate
      'halted))     ; otherwise terminate. 

#|
Now consider evaluating the expression (try try) and show that any possible 
outcome (either halting or running forever) violates the intended behavior 
of halts?
|#

(try try) -->

((lambda (p) 
  (if (halts? p p)
      (run-forever)
      'halted) try)) -->

(eval (eval-if (halts? try try) (run-forever) 'halted) <env_0>)

if (halts? try try) --> #t, then (try try) doesn't terminate, and vice versa.
