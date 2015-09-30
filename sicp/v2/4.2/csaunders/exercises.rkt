#lang racket

;; Pre-requisites
(require compatibility/mlist)

;; Capture the underlying system eval and apply
(require (only-in racket/base
                  [eval eval-in-underlying-scheme]
                  [apply apply-in-underlying-scheme]))


;; Missing definitions
(define (make-procedure) '())
(define (primitive-procedure?) '())
(define (apply-primitive-procedure) '())
(define (compound-procedure?) '())
(define (procedure-body) '())
(define (procedure-parameters) '())
(define (procedure-environment) '())
(define (true?) '())


(define (eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(quoted? exp) (text-of-quotation exp)]
        [(assignment? exp) (eval-assignment exp env)]
        [(definition? exp) (eval-definition exp env)]
        [(if? exp) (eval-if exp env)]
        [(lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)]
        [(begin? exp)
         (eval-sequence (begin-actions exp) env)]
        [(cond? exp) (eval (cond->if exp) env)]
        [(application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env)]
        [else (error "Unknown expression type -- EVAL" exp)]))

(define (apply procedure arguments env)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure)))]
        [else (error "Unknown procedure type -- APPLY" procedure)]))

#|
| Procedure Arguments
|#

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

#|
| Conditionals
|#

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

#|
| Sequences
|#

(define (eval-sequence exps env)
  (cond [(last-exp? exps) (eval (first-exp exps) env)]
        [else (eval (first-exp exps) env)
               (eval-sequence (rest-exps exps) env)]))

#|
| Assignments and Definitions
|#

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                   (eval (definition-value exp) env)
                   env)
  'ok)

#|
| Special Forms
|#

(define (self-evaluating? exp)
  (cond [(number? exp) true]
        [(string? exp) true]
        [else false]))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (value-for-key 'quote exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (value-for-key tagged-list key)
  (cond [(empty? tagged-list) (error "key does not exist -- VALUE-FOR-KEY" key)]
        [(eq? key (car tagged-list)) (cadr tagged-list)]
        [else (value-for-key (cddr tagged-list) key)]))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;; formal params
                   (cddr exp)))) ;; procedure body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? exp) (null? (cdr exp)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let [(first (car clauses))
            (rest (cdr clauses))]
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(displayln "exercise 4.11")
(define (make-frame variables values)
  (if (null? variables)
      '()
      (mcons (mcons (car variables) (car values))
             (make-frame (cdr variables)
                         (cdr values)))))

(define (add-binding-to-frame! var val frame)
  (mcons (mcons var val)
         frame))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcaar bindings))
             (mcdar bindings))
            (else (scan (mcdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcaar bindings))
             (set-mcdr! (mcar bindings) val))
            (else (scan (mcdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame! var val bindings))
            ((eq? var (mcaar bindings))
             (set-mcdr! (mcar bindings)))
            (else (scan (mcdr bindings)))))
    (scan frame)))

#|
| Environment
|#
(define (mcaar x) (mcar (mcar x)))
(define (mcdar x) (mcdr (mcar x)))

(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

(define (primitive-procedure-names) (list '+ '* '- '/))
(define (primitive-procedure-objects) (list + * - /))

(define (setup-environment)
  (let [(initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment))]
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; Chapter 4.2.2 Actual Definitions
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)))))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (user-print obj)
  (if (compound-procedure? obj)
      (displayln (list 'compound-procedure
                     (procedure-parameters obj)
                     (procedure-body obj)
                     '<proc-env>))
      (displayln obj)))

(define (driver-loop)
  (displayln input-prompt)
  (let* [(input (read))
         (output (actual-value input the-global-environment))]
    (displayln output-prompt)
    (user-print output))
  (driver-loop))

(define (force-it obj) obj)
(define (delay-it exp env) exp)

(displayln "exercise 4.27")
#|
I'm not entirely sure about what would be happening here, though
what I think would happen is, as follows:

count => 0
id => <proc>

w => (id (id 10))
           ^ Needs to be forced, therefore count = 1
w => (thunk (id 10))

Calling w would result in the thunk being forced, because it is a value type
w => 10

As a result count gets incremented again

count => 2
|#

(displayln "exercise 4.28")
;; The eval cannot be used because if we didn't force our thunks before
;; calling our procedures, there is the chance we would send our data structure
;; to an host procedure, which would be invalid.
;; By always forcing when we go to evaluate, we know that we will be working
;; with primitives.

(displayln "exercise 4.29")
;; TODO
;; Fibonacci probably. dunnolol

;; I think the count would be 1 at the end because we are required to force
;; (id 10) only once. If we don't have memoization though, it would be 2 because
;; the procedure would've had to be evaluated both times.

(displayln "exercise 4.30")
;; a. Display and Eval are primitive functions so they will always be forced before
;;    being passed down to the host.

;; b. (p1 1) would be (1 2). (p2 1) would be 1 because the set! evaluation would be delayed

;; c. actual-value calls force-it

;; d. skip

(displayln "exercise 4.31")
;; I'm going to skip this one as it's a bit involved

(displayln "exercise 4.32")
#|
| Q. How are these streams lazier?
|
| A. Values from list entries aren't calculated
|    until we actually need to use them. This
|    includes _car_ which wasn't the case in our
|    lazy list implementation in chapter 3.
|
|    Some examples of where this laziness is super
|    helpful is the questions in chapter 3 where we
|    were modelling electrical circuits. With this
|    implementation, we wouldn't need to worry about
|    the methods not being defined.
|#

(displayln "exercise 4.33")
;.. inside (eval exp env)
; ((quoted? exp) (eval-quoted exp env))
;...
(define (eval-quoted exp env)
  (let [(text (text-of-quotation exp))]
    (if (pair? text)
        (eval (cons 'list
                    (list 'quote (car exp))
                    (list 'quote (cdr exp)))
              env)
        text)))

(displayln "exercise 4.34")
;; Most of this is just copy-pasta from
;; https://wqzhang.wordpress.com/2010/04/21/sicp-exercise-4-34/
;; There are a few parts that are confusing to me and I'm kinda
;; lost.
;; I know what I want to do... but don't know how to express it.
;;
;;... inside eval
;; ((list-lambda? exp)
;;    (make-list-procedure (lambda-parameters p)
;;                         (lambda-body exp)
;;                         env))
;;...
(define (normal-procedure? p)
  (tagged-list? p 'procedure))
(define (list-procedure? p)
  (tagged-list? p 'list-procedure))

(define (make-list-procedure params body env)
  (list 'list-procedure params body env))

(set! compound-procedure? (lambda (p)
      (or (normal-procedure? p)
          (list-procedure? p))))

(define max-elts 5)

(define (list-proc->list proc count)
  (define (apply-proc-to-list proc lst env)
    ;; ????
    (eval-sequence (procedure-body proc)
                   (extend-environment
                    (procedure-parameters proc)
                    lst
                    (procedure-environment proc))))
  (define (list-element option)
    (force-it (apply-proc-to-list (actual-value option the-global-environment) 
                                  (list proc)
                                  the-global-environment)))
  (define (list-peek x n)
    (if (list-procedure? x)
        (if (= n 0)
            '(......)
            (list-proc->list x n))
        x))
  (cons (list-peek (list-element 'car) max-elts)
        (list-peek (list-element 'cdr (- count 1)))))

(define (improved-user-print obj)
  (define (display-normal obj)
    (displayln (list 'compound-procedure
                     (procedure-parameters obj)
                     (procedure-body obj)
                     '<proc-env>)))
  
  (define (display-list obj)
    (displayln (list-proc->list obj max-elts)))
  
  (cond
    [(normal-procedure? obj) (display-normal obj)]
    [(list-procedure? obj) (display-list obj)]
    [else (displayln obj)]))

(displayln "exercise 4.35")
(define (amb . args) '())
(define (require p) (if (not p) (amb) 'ok))
(define (an-int-starting-from i)
  (amb i (an-int-starting-from (+ i 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(displayln "exercise 4.36")
#|
| replacing an-integer-between with an-int-starting-from
| isn't sufficient because it wouldn't create limits
| that we would need in order to ensure that there is
| an upper limit being applied to k.
|#

(define (a-pythagorean-triple-from low)
  (let* [(i (an-int-starting-from low))
         (isq (* i i))
         (j (an-integer-between i (/ (- isq 1) 2)))
         (jsq (* j j))
         (k (sqrt (+ isq jsq)))]
    (require (integer? k))
    (list i j k)))

(displayln "exercise 4.37")
;; Ben is correct because he ensures that the upper
;; limit is clamped. Therefore, he reduces the total
;; search space. Exercise 4.36 has an infinite search space.
  