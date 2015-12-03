#|
Exercise 4.7
============

Let* is similar to let, except that the bindings of the let* variables are
performed sequentially from left to right, and each binding is made in an
environment in which all of the preceding bindings are visible. For example

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

returns 39. Explain how a let* expression can be rewritten as a set of nested
let expressions, and write a procedure let*->nested-lets that performs this
transformation. If we have already implemented let (Exercise 4.6) and we want
to extend the evaluator to handle let*, is it sufficient to add a clause to
eval whose action is

(eval (let*->nested-lets exp) env)

or must we explicitly expand let* in terms of non-derived expressions?

|#

(define (bar)
  (let ((x 1))
    (let ((y (+ x 1)))
      (+ x y))))

; bar becomes:
(define (bar)
  ((lambda (x)
     ((lambda (y)
        (+ x y))
      (+ x 1)))
   1))
(define test '(let* ((x 1) (y (+ x 1)) (z (* x y))) (+ x y z)))


(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))


(define (let*->nested-lets exp)
  (define (let*-pairs exp)
    ; Given the expression return the ((var1 exp1)...(varn expn)) pairs
    (cadr exp))
  (define (last-pair? pairs) (null? (cdr pairs)))
  ; if this is the last pair of var/exp, then the body is let-body exp,
  ; otherwise the body is a cons of recursive call with the next pair,
  ; and the exp  
  (define (loop pairs)
    ; the signature for make-lambda is parameter, body.
    (if (last-pair? pairs)         
        (cons
         (make-lambda (list (caar pairs)) (let-body exp))
         (cdar pairs))
        (cons
         (make-lambda (list (caar pairs)) (list (loop (cdr pairs)) ))
         (cdar pairs)))
    )
  (loop (let*-pairs exp)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(let*->nested-lets test)

#|
Exercise 4.8
============
“Named let” is a variant of let that has the form

(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)

The ⟨bindings⟩ and ⟨body⟩ are just as in ordinary let, except that ⟨var⟩
is bound within ⟨body⟩ to a procedure whose body is ⟨body⟩ and whose
parameters are the variables in the ⟨bindings⟩. Thus, one can repeatedly
execute the ⟨body⟩ by invoking the procedure named ⟨var⟩. For example,
the iterative Fibonacci procedure (1.2.2) can be rewritten using named
let as follows:

(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))

Modify let->combination of Exercise 4.6 to also support named let.
|#

(define (fib n)
  (let fib-iter ((a 1) (b 0) (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1)))))

(define named '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))
; equivalent to:
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) 
                  a 
                  (- count 1))))
  (fib-iter 1 0 n))


(define (named-let->define exp)
  ; transforms a named-let to an inner named function.
  (let* ((bindings (named-let-bindings exp))
         (params (map car bindings))
         (vars (map cadr bindings))
         (body (named-let-body exp)))    
    (list
     (cons 'define
           (cons
            (cons (named-let-name named) params)
            (named-let-body exp)))
     
     
     vars))) ; function invocation
                                      
(define (named-let? exp) (not (pair? (cadr exp))))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-name exp) (cadr exp))
(define (named-let-body exp) (cdddr exp))
