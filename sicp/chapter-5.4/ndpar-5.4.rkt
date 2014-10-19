#lang racket

;; -------------------------------------------------------
;; Explicit-Control Evaluator
;;
;; - Run ndpar-5.4-sim.rkt
;; - Evaluate following procedures in DrRacket REPL
;; -------------------------------------------------------

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'cond? cond?)
        (list 'lambda? lambda?)
        (list 'let? let?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)
        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)
        (list 'last-operand? last-operand?)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-body procedure-body)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'begin-actions begin-actions)
        (list 'no-more-exps? no-more-exps?)
        (list 'first-exp first-exp)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)
        (list 'true? true?)
        (list 'cond-clauses cond-clauses)
        (list 'no-conds? no-conds?)
        (list 'first-cond first-cond)
        (list 'rest-conds rest-conds)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'let->combination let->combination)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'announce-output announce-output)
        (list 'user-print user-print)
        (list 'get-global-environment get-global-environment)))

(define eceval
  (make-machine
   eceval-operations
   '(read-eval-print-loop
     (perform (op initialize-stack)) ; defined in simulator
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     print-result
     (perform (op print-stack-statistics)) ; defined in simulator
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     eval-dispatch
     (if ((op self-evaluating?) (reg exp)) (label ev-self-eval))
     (if ((op variable?) (reg exp)) (label ev-variable))
     (if ((op quoted?) (reg exp)) (label ev-quoted))
     (if ((op assignment?) (reg exp)) (label ev-assignment))
     (if ((op definition?) (reg exp)) (label ev-definition))
     (if ((op if?) (reg exp)) (label ev-if))
     (if ((op cond?) (reg exp)) (label ev-cond))
     (if ((op lambda?) (reg exp)) (label ev-lambda))
     (if ((op let?) (reg exp)) (label ev-let))
     (if ((op begin?) (reg exp)) (label ev-begin))
     (if ((op application?) (reg exp)) (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (if ((op no-operands?) (reg unev)) (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (if ((op last-operand?) (reg unev)) (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (if ((op primitive-procedure?) (reg proc)) (label primitive-apply))
     (if ((op compound-procedure?) (reg proc)) (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (if ((op no-more-exps?) (reg unev)) (label ev-sequence-end))
     (assign exp (op first-exp) (reg unev))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-end
     (restore continue)
     (goto (reg continue))

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (if ((op true?) (reg val)) (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-cond
     (assign unev (op cond-clauses) (reg exp))
     (save unev)

     ev-cond-next
     (if ((op no-conds?) (reg unev)) (label ev-cond-done))
     (assign exp (op first-cond) (reg unev))
     (if ((op cond-else-clause?) (reg exp)) (label ev-cond-actions))
     (save exp)
     (save continue)
     (assign exp (op cond-predicate) (reg exp))
     (assign continue (label ev-cond-decide))
     (goto (label eval-dispatch))

     ev-cond-decide
     (restore continue)
     (restore exp)
     (if ((op true?) (reg val)) (label ev-cond-actions))
     (restore unev)
     (assign unev (op rest-conds) (reg unev))
     (save unev)
     (goto (label ev-cond-next))

     ev-cond-actions
     (assign unev (op cond-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-cond-done
     (restore unev)
     (goto (reg continue))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))

     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop)))))

(start eceval)

;; -------------------------------------------------------
;; Tests and Exercises
;;
;; - Evaluate following procedures in eceval REPL
;; -------------------------------------------------------

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))
; ((total-stack-pushes . 3) (maximum-stack-depth . 3))

(append '(a b c) '(d e f))
; ((total-stack-pushes . 118) (maximum-stack-depth . 17))


;; Exercise 5.23, p.560
;; Syntax transformers

(define (ex-5-23 x)
  (let ((a 5)) (+ a x)))

(ex-5-23 3)

;; Exercise 5.24, p.560
;; cond as a special form

(cond (else 1))
(cond ((< 1 3) 2) ((/ 5 0) 3) (else (/ 4 0)))
(cond ((< 4 3) 2) ((< 5 7) 3) (else (/ 4 0)))
(cond ((< 4 3) 2) ((< 8 7) 3) (else 4))
(cond ((< 4 3) 2) ((< 8 7) 0)) ; #f (non-determined)
(cond) ; non-determined


;; Exercise 5.26, p.564

(define (factorial-iter n)
  (define (iter prod count)
    (if (< n count)
        prod
        (iter (* count prod)
              (+ count 1))))
  (iter 1 1))

; 1 10  64
; 2 10  99
; 3 10 134
; 4 10 169
; 5 10 204

;; Exercise 5.27, p.564

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

; 1  8  16
; 2 13  48
; 3 18  80
; 4 23 112
; 5 28 144

; Factorials
; ┌─────────┬─────────┬──────────┐
; │         │max depth│ # pushes │
; │         │ (space) │  (time)  │
; ├─────────┼─────────┼──────────┤
; │recursive│  3 + 5n │ 32n - 16 │
; │iterative│      10 │ 35n + 29 │
; └─────────┴─────────┴──────────┘


;; Exercise 5.29, p.565

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; 2 13   72  1
; 3 18  128  2
; 4 23  240  3
; 5 28  408  5
; 6 33  688  8
; 7 38 1136 13

; M = 5n + 3 - same as recursive factorial
; S(n) = S(n-1) + S(n-2) + 40
; S(n) = 56 Fib(n+1) - 40 = O(φ^n)

; S=. 3 3 $ 128 72 1 240 128 1 408 240 1x
; (%. S) (+/ .*) 240 408 688 NB. => 1 1 40
; (%. 2 2 $ 2 1 3 1) (+/ .*) 72 128x NB. => 56 _40
; 2 }. (_40 + 56&*) 1 1 2 3 5 8 13 21
