#lang racket
(require (only-in racket 
                  (apply apply-in-underlying-scheme)
                  (eval eval-in-underlying-scheme)))
(require (only-in r5rs set-car! set-cdr!))
(require (only-in compatibility/mlist mlist))

(define (map proc items)
  (if (null? items) '()
      (mcons (proc (mcar items)) (map proc (mcdr items)))))

(define (length items)
  (if (null? items) 0
      (+ 1 (length (mcdr items)))))

(define (filter predicate items)
  (cond ((null? items) '())
        ((predicate (mcar items)) (mcons (mcar items) (filter predicate (mcdr items))))
        (else (filter predicate (mcdr items)))))

(define (in? object items)
  (not (eq? (filter (lambda (x) (eq? x object)) items) '())))

#|
Exercise 4.11
=============
Instead of representing a frame as a pair of lists, we can represent a 
frame as a list of bindings, where each binding is a name-value pair. 
Rewrite the environment operations to use this alternative representation. 

Each frame of an environment is represented as a pair of lists: a list of the
variables bound in that frame and a list of the associated values.14

Answer
======

make-frame, frame-variables, frame-values and add-binding-to-frame! all need to be
touched to change the representation from list of variables/values to a list of zipped
variables and values.
|#

(define (make-frame vars vals) (mcons vars vals))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))


(define (new-make-frame variables values) 
  ; Given two lists of the same length, zips them together. Eg:
  ; (make-frame (mlist 1 2 3) (mlist 'a 'b 'c)) -> (mlist (1 . a) (2 . b) (3 . c))
  (if (null? variables) '()
      (mcons (mcons (mcar variables) (mcar values)) (make-frame (mcdr variables) (mcdr values)))))  

(define (new-frame-variables frame)
  (map mcar frame))
(define (new-frame-values frame)
  (map mcdr frame))
(define (new-add-binding-to-frame! var val frame)
  (mcons (mcons var val) frame))


; used for testing:
(define test-frame (make-frame (mlist '1 '2 '3 '4) (mlist 'a 'b 'c 'd)))

; =================

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (enclosing-environment env) (mcdr env))
(define the-empty-environment '())
(define (first-frame env) (mcar env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

#|
Exercise 4.12
=============

The procedures set-variable-value!, define-variable!, and lookup-variable-value can be 
expressed in terms of more abstract procedures for traversing the environment structure. 
Define abstractions that capture the common patterns and redefine the three procedures 
in terms of these abstractions. 

Note
----
Remember that the environment is the collection of frames. Each frame is a mapping
of keys/vals, with a pointer to the 'enclosing environment'. Frames are linear, but 
environments may not be. See the diagram in:
https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-21.html#%_sec_3.2

Also: these functions *won't work* with the new-frame procedures defined above, because
those produce new lists via map!
|#

(define test-env (extend-environment (mlist 1 2 3) (mlist 'a 'b 'c) the-empty-environment))
(define extended-env (extend-environment (mlist 10 11 12) (mlist 'x 'y 'z) test-env))


; factoring out the logic that searches through a frame makes the other procedures much
; clearer, and clears the way for further refactoring.
(define (find-in-frame target frame)
  ; Given a target, list of variables and values, find-in-frame will search for
  ; the target. If it finds target in the frame, it returns the vals list
  ; truncated from the matching val. Otherwise returns the empty list.
  ;
  ; eg: (find-in-frame 3 test-frame) --> (mcons 'c (mcons 'd '()))
  (define (scan vars vals)
    (cond ((null? vars) '())
          ((eq? target (mcar vars)) vals)
          (else (scan (mcdr vars) (mcdr vals)))))
  (scan (frame-variables frame) (frame-values frame)))


(define (lookup-variable-value-new var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ((frame (first-frame env))
               (frame-result (find-in-frame var frame)))
          (cond ((null? frame-result) (env-loop (enclosing-environment env)))
                (else (mcar frame-result))))))
  (env-loop env))

(define (set-variable-value!-new var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ((frame (first-frame env))
               (frame-result (find-in-frame var frame)))
          (cond ((null? frame-result) (env-loop (enclosing-environment env)))
                (else (set-car! frame-result val))))))
  (env-loop env))

(define (define-variable!-new var val env)
  (let* ((frame (first-frame env))
               (frame-result (find-in-frame var frame)))
    (if (null? frame-result) (add-binding-to-frame! var val frame)
        (set-mcar! frame-result val))))

; These could be refactored further, but at this point the pattern is pretty clear,
; so I'm going to move on.


#|
Exercise 4.13
=============
Scheme allows us to create new bindings for variables by means of define, but provides 
no way to get rid of bindings. Implement for the evaluator a special form make-unbound! 
that removes the binding of a given symbol from the environment in which the 
make-unbound! expression is evaluated. This problem is not completely specified. For 
example, should we remove only the binding in the first frame of the environment? 
Complete the specification and justify any choices you make. 

Answer
======

We're only going to unbind in the first frame of the environment, because the variable
we're looking at could be shadowing other values in other frames. If we want this more
powerful version of unbinding, it should get a different name, such as globally-unbind!
|#

; unfortunately, our find-in-frame procedure won't work here, so we step back to the 
; previous pattern:
(define (make-unbound! var env)
  (let ((frame (first-frame env))         
    (define (search variables values)
      (cond 
        ((if (null? variables)) (make-frame variables values))
        ((eq? (mcar variables) var) (search (mcdr variables) (mcdr values))
        (else ))))))


(variables (frame-variables frame))
         (values (frame-values frame))
  
#|
Exercise 4.15
=============
Given a one-argument procedure p and an object a, p is said to ``halt'' on a 
if evaluating the expression (p a) returns a value (as opposed to terminating 
with an error message or running forever). Show that it is impossible to write 
a procedure halts? that correctly determines whether p halts on a for any 
procedure p and object a. Use the following reasoning: If you had such a procedure 
halts?, you could implement the following program:

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

Now consider evaluating the expression (try try) and show that any possible outcome 
(either halting or running forever) violates the intended behavior of halts?.23 

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

Now consider evaluating the expression (try try)

--> eval ((lambda (x)  
            (if (halts? p p)
                (run-forever)
                'halted)) try)

eval ((if (halts? try try) (run-forever) 'halted)
|#