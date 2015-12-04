#lang racket
(require (only-in racket 
                  (apply apply-in-underlying-scheme)
                  (eval eval-in-underlying-scheme)
                  (cons underlying-cons)
                  (list underlying-list)
                  (car underlying-car)
                  (cdr underlying-cdr)
                  (pair? underlying-pair?)))
(require (only-in r5rs set-car! set-cdr!))
(require (only-in compatibility/mlist mlist))

;Fudge pairs into mpairs:
(define cons mcons)
(define car mcar)
(define cdr mcdr)
(define list mlist)
(define cadr (lambda (items) (car (cdr items))))
(define cdar (lambda (items) (cdr (car items))))
(define caddr (lambda (items) (car (cdr (cdr items)))))
(define caadr (lambda (items) (car (car (cdr items)))))
(define cdadr (lambda (items) (cdr (car (cdr items)))))
(define cddr (lambda (items) (cdr (cdr items))))
(define cdddr (lambda (items) (cdr (cdr (cdr items)))))
(define cadddr (lambda (items) (car (cdr (cdr (cdr items))))))
(define pair? mpair?)
(define (map proc items)
  (if (null? items) '()
      (cons (proc (car items)) (map proc (cdr items)))))
(define (length items)
  (if (null? items) 0
      (+ 1 (length (cdr items)))))



(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
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
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body
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
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (let? exp) (tagged-list? exp 'let))
(define (let-assignment exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-exp assignment)
  (if (null? assignment)
      '()
      (cons (cadr (car assignment))
            (let-exp (cdr assignment)))))
(define (let-var assignment)
  (if (null? assignment)
      '()
      (cons (car (car assignment))
            (let-var (cdr assignment)))))
  
(define (let->combination exp)
  (transform-let (let-assignment exp) (let-body exp)))
(define (transform-let assignment body)
  (cons (make-lambda (let-var assignment) body)
        (let-exp assignment)))

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
        ((let? exp) (eval (let->combination exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
(define (apply-primitive-procedure proc args)
  (define (mlist->list args) ; converts mpairs to normal pairs for the underlying scheme.
    (if (null? args) '()
        (underlying-cons (car args) (mlist->list (cdr args)))))
   (if (pair? args) (apply-in-underlying-scheme (primitive-implementation proc) (mlist->list args))
       (apply-in-underlying-scheme (primitive-implementation proc) args)))
(define (primitive-implementation proc) (cadr proc))
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) "*unassigned*") (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
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
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'mcons mcons)
        (list '* *)       
        ))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (list->mlist args)
  (if (underlying-pair? args)
      (if (null? args) '()
          (cons (underlying-car args) (list->mlist (underlying-cdr args))))
      args))
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (list->mlist (read))))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; (driver-loop)

; Exercise EVAL:
(eval (list 'define 'foo '10) the-global-environment)
(eval (list 'define (list 'append 'x 'y) (list 'if (list 'null? 'x) 'y (list 'cons (list 'car 'x) (list 'append (list 'cdr 'x) 'y)))) the-global-environment)
(define test (list 1 2 3))
(eval (list 'append (list 1 2 3) (list 4 5 6)) the-global-environment) ;<-- doesn't work because the evaluator steps down to the list of (1 2 3) and
; thinks that 1 is a proc to call. 

(eval (list 'set! 'foo 100) the-global-environment)
(eval 'foo the-global-environment)
(eval (list 'if 'true 2000 "if with truthy cond") the-global-environment)
(eval (list 'if 'false 2000 "if with false.") the-global-environment)
(eval (list 'lambda (list 'x) (list * 'x 'x)) the-global-environment)
(eval (list 'begin (list 'define 'bar 555)) the-global-environment)
(eval (list 'define (list 'square 'x) (list '* 'x 'x)) the-global-environment)
(eval (list 'square 2) the-global-environment)
; (eval (list 'cond (list (list 'false "ack") (list 'false "foo") (list 'true "cond output"))) the-global-environment)

(apply (list 'primitive +) (list 1 2 3))

#|
Exercise 4.6
============

Let expressions are derived expressions, because

(let ((⟨var1⟩ ⟨exp1⟩) ... (⟨varn⟩ ⟨expn⟩))
  ⟨body⟩)

is equivalent to

((lambda (⟨var₁⟩ … ⟨varₙ⟩)
   ⟨body⟩)
 ⟨exp₁⟩
 …
 ⟨expₙ⟩)

Implement a syntactic transformation let->combination that reduces evaluating let
expressions to evaluating combinations of the type shown above, and add the
appropriate clause to eval to handle let expressions.

Notes
-----

The call will be (eval (list 'let '((foo 10) (bar 20)) ) the-global-environment)
|#

#|
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-vars exp)
  (cadr exp))
(define (let-body exp)
  (caddr))
;(define (eval-let exp env)
  
;  )
|#



(define test2 (list 'let (list (list (list 'foo 10) (list 'bar 20))) (list 'lambda '(x) '(* x x))))
(eval (list 'define 'test3 "*unassigned*") the-global-environment)
; (eval 'test3 the-global-environment) ; <-- raises expected error.

#|

Before evaluating the body of a lambda expression, we “scan out” and eliminate all the
internal definitions in the body. The internally defined variables will be created with
a let and then set to their values by assignment. For example, the procedure

(lambda ⟨vars⟩
  (define u ⟨e1⟩)
  (define v ⟨e2⟩)
  ⟨e3⟩)

would be transformed into

; (cons 'lambda (cons <let clauses> (cons <set! clauses> (cons body))))
; where: <let clauses> is (cons 'let (cons (definition-variable def) "*unassigned*")) for def in procedure
; <set! clauses> is (cons 'set! (cons (definition-variable def) (definition-value def))) for def in procedure
(lambda ⟨vars⟩ 

  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u ⟨e1⟩)
    (set! v ⟨e2⟩)
    ⟨e3⟩))

Exercise 4.16
=============
In this exercise we implement the method just described for interpreting internal
definitions.

- Change lookup-variable-value (4.1.3) to signal an error if the value it finds
  is the symbol *unassigned*.
- Write a procedure scan-out-defines that takes a procedure body and returns an
  equivalent one that has no internal definitions, by making the transformation
  described above.
- Install scan-out-defines in the interpreter, either in make-procedure or in
  procedure-body (see 4.1.3). Which place is better? Why? 
|#


; (cons 'lambda (cons <let clauses> (cons <set! clauses> (cons body))))
; where: <let clauses> is (cons 'let (cons (definition-variable def) "*unassigned*")) for def in procedure
; <set! clauses> is (cons 'set! (cons (definition-variable def) (definition-value def))) for def in procedure

(define (scan-out-define proc)
  (let ((body (lambda-body proc)))
    (define (collect-defs body)
      ; loop over the body of the proc, and collect all the internal definitions.
      (if (null? body) '()
          (if (definition? (car body))
              (cons (car body) (collect-defs (cdr body)))
              (collect-defs (cdr body)))))
    (let ((definitions (collect-defs body)))
      (cons ))
    
    (collect-defs body)))



