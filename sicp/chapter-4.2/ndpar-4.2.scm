#lang planet neil/sicp

;; -------------------------------------------------------
;; Lazy Metacircular Evaluator
;; -------------------------------------------------------

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
        ((let? exp) (eval (let->combination exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply1 (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

; If we call it 'apply', Racket overrides standard 'apply',
; so 'apply-in-underlying-scheme' wouldn't work.
(define (apply1 procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

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

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

;; -------------------------------------------------------
;; Representing Expressions
;; -------------------------------------------------------

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
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
      (make-lambda (cdadr exp) (cddr exp))))

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
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp))
        (let-vals exp)))

;; -------------------------------------------------------
;; Evaluator Data Structures
;; -------------------------------------------------------

(define (false? x) (eq? x false))
(define (true? x) (not (false? x)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define (frame-bindings frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val)
                        (frame-bindings frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define ((set-val! val) var) (set-cdr! var val))

(define (do-in-frame var frame then-proc else-proc)
  (let ((binding (assoc var (frame-bindings frame))))
    (if binding
        (then-proc binding)
        (else-proc binding))))

(define (env-loop var env action)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env))
            (try-next-frame
             (lambda (_)
               (env-loop var (enclosing-environment env) action))))
        (do-in-frame var frame action try-next-frame))))

(define (lookup-variable-value var env)
  (env-loop var env cdr))

(define (set-variable-value! var val env)
  (env-loop var env (set-val! val)))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (bind (lambda (_)
                 (add-binding-to-frame! var val frame))))
    (do-in-frame var frame (set-val! val) bind)))

;; -------------------------------------------------------
;; Running Evaluator
;; -------------------------------------------------------

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'runtime runtime)
        (list 'newline newline)
        (list 'display display)
        (list '= =)
        (list '< <)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '* *)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (actual-value input the-global-environment)))
    (announce-output output-prompt)
    (user-print output))
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

;; -------------------------------------------------------
;; Representing thunks
;; -------------------------------------------------------

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ; oops. abstraction leak
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; -------------------------------------------------------
;; Normal order and Applicative order
;; -------------------------------------------------------

;; Exercise 4.26, p.401
;; Implementing 'unless' as a special form,
;; namely, deriving it from 'if'.

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-usual exp) (caddr exp))
(define (unless-exceptional exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-exceptional exp)
           (unless-usual exp)))

;; Add this clause to 'eval' function:
;; ((unless? exp) (eval (unless->if exp) env))

;; -------------------------------------------------------
;; Exercises
;; -------------------------------------------------------

(driver-loop)

;> (define x 5)
;> (set! x 6)
;> (define (append x y) (if (null? x) y (cons (car x) (append (cdr x) y))))
;> (append '(a b c) '(d e f))
;> (define (try a b) (if (= a 0) 1 b))
;> (try 0 (/ 1 0))

;; Exercise 4.25, p.400

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; Runs infinitely in applicative-order language.
;; Works as expected in normal-order language,
;; or with 'unless' as a special form.
; (factorial 5)

;; Exercise 4.27, p.406
;; Lazy evaluation and side effects

(define (id x)
  (set! count (+ count 1))
  x)

(define count 0)

(define w (id (id 10))) ; 1st 'id' is evaluated, 2nd not

;; Exercise 4.29, p.407
;; Memoization

(define (square x) (* x x))
(square (id 10)) ;= 100
;; with memo: count = 1
;; without memo: count = 2

(define (times n x)
  (if (= n 0)
      0
      (+ x (times (- n 1) x))))

(define (fib n)
  (if (< n 3)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

;; Profiling lazy evaluator is tricky:
;; You need to force the evaluation of runtime
(define (test)
  (define start (runtime))
  (times 20 (fib 20))
  (/ (- (runtime) start) 1e6))

(test) ;= ~0.2 sec

;; Without forcing the result is negative!
(let ((start (runtime)))
  (times 20 (fib 20))
  (/ (- (runtime) start) 1e6))

;; Exercise 4.28, p.407
;; Forcing evaluation of operator

(define (fold op acc ls)
  (if (null? ls)
      acc
      (fold op (op acc (car ls)) (cdr ls))))
;              ^
;; Without forcing there would be an error:
;; Unknown procedure type (thunk op ...)
(fold + 0 '(1 2 3))

;; Exercise 4.30, p.407
;; Side-effects and lazy evaluation of sequences

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          '(57 321 88))

;; a. eval-sequence is called few times:

;; - Evaluating single-exp body of for-each.

;; - Evaluating (begin (proc (car items)) ...)
;; where proc is thunked lambda.
;; proc is forced when applied, and (car items) is thunked.

;; - Evaluating body of lambda ((newline)(display x)) with
;; thunked x. While applying display, x is forced because
;; display is primitive.

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(p1 1) ;= (1 2)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
;    ^
;; b. This sexp is passed as a thunk to procedure p.
;; Within the body of p, e as a variable
;; is evaluated to thunk (and discarded) in Ben's case,
;; is forced (and changes x) in Cy's case.

(p2 1) ;= Ben: 1; Cy: (1 2)

;; c. The final result obtained by eval will be the same
;; as obtained by actual-value. The converse may not be true.

;; -------------------------------------------------------
;; Lazy Lists, p.409
;; -------------------------------------------------------

;> (define (cons x y) (lambda (m) (m x y)))
;> (define (car z) (z (lambda (p q) p)))
;> (define (cdr z) (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;(define (map proc items)
;  (if (null? items)
;      '()
;      (cons (proc (car items))
;            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(list-ref integers 17) ;= 18

; compare with 3.5

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (add-lists (scale-list integrand dt)
                     int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map f y))
  y)

(list-ref (solve (lambda (x) x) 1 1e-4) 1e4) ;= 2.718
