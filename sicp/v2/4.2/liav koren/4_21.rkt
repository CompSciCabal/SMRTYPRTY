#lang racket

#|
Exercise 4.21
=============

Amazingly, Louis’s intuition in Exercise 4.20 is correct. It is indeed
possible to specify recursive procedures without using letrec (or even
define), although the method for accomplishing this is much more subtle
than Louis imagined. The following expression computes 10 factorial by
applying a recursive factorial procedure:231

((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

Check (by evaluating the expression) that this really does compute
factorials. Devise an analogous expression for computing Fibonacci numbers.
Consider the following procedure, which includes mutually recursive internal
definitions: 


1) Check (by evaluating the expression) that this really does compute factorials.
Devise an analogous expression for computing Fibonacci numbers.

|#

(
 (lambda (n) ( ; return a proc of n     
    (lambda (fact) (fact fact n)) ; return proc of fact
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1))))) ; call proc of fact with proc of ft, k
    ))
 10)


((lambda (n)
   (
    (lambda (fibo) (fibo fibo n))
    (lambda (fb k)
      (cond ((= k 0) 1)
            ((= k 1) 1)
            (else (+ (fb fb (- k 1)) (fb fb (- k 2))))))

   ))
 5)

#|
Fill in the missing expressions to complete an alternative definition of f,
which uses neither internal definitions nor letrec:
|#

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

