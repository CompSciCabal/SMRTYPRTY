#lang racket
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (when (not (memq new-constraint constraints))
        (set! constraints 
              (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

#|
Exercise 3.33  
-------------

Using primitive multiplier, adder, and constant constraints, 
define a procedure averager that takes three connectors a, b, 
and c as inputs and establishes the constraint that the value 
of c is the average of the values of a and b.
|#
(displayln "Exercise 3.33, make a constraint based averager:")
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (averager a b output)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier u v output)
    (constant 0.5 v)
    'ok))

(probe "average" c)

(averager a b c)

(set-value! a -10 'user)
(set-value! b 10 'user)


#| 
Exercise 3.34.  Louis Reasoner wants to build a squarer, a constraint device 
with two terminals such that the value of connector b on the second terminal 
will always be the square of the value a on the first terminal. He proposes 
the following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

There is a serious flaw in this idea. Explain.
|#
(displayln "Exercise 3.34")
(define (squarer d output)
  (multiplier d d output))

(define d (make-connector))
;(define e (make-connector))
(define output (make-connector))
(squarer d output)
(displayln "This only works one way: ")
(set-value! output 20 'user)
(probe "squarer" output)


#|
Exercise 3.35.  Ben Bitdiddle tells Louis that one way to avoid 
the trouble in exercise 3.34 is to define a squarer as a new 
primitive constraint. Fill in the missing portions in Ben's outline 
for a procedure to implement such a constraint...

|#
(displayln "Exercise 3.35")
(displayln "I'm being lazy, don't want to dig up the sqrt code from ch1, using times/divide by two as a proxy.")

(define (squarer2 a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (/ (get-value b) 2) me))
        (set-value! b (* (get-value a) 2) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define f (make-connector))
(define g (make-connector))
(squarer2 f g)
(probe "squarer f" f)
(probe "squarer g" g)

(set-value! g 20 'user)

(displayln "3.37")
#|
Exercise 3.37
-------------
The celsius-fahrenheit-converter procedure is cumbersome when 
compared with a more expression-oriented style of definition, 
such as

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

Here c+, c*, etc. are the ``constraint'' versions of the arithmetic 
operations. For example, c+ takes two connectors as arguments and 
returns a connector that is related to these by an adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

Define analogous procedures c-, c*, c/, and cv (constant value) that 
enable us to define compound constraints as in the converter example above.
|#

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(probe "C" C)
(probe "F" F)
(set-value! F 80 'user)
