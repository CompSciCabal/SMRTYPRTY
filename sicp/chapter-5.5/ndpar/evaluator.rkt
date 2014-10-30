#lang racket

(require r5rs)
(require (file "compiler.rkt"))
(require (file "primitive.rkt"))
(require (file "simulator.rkt"))

(provide (all-defined-out))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

;; -------------------------------------------------------
;; Explicit-Control Evaluator
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
        (list 'last-exp? last-exp?)
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
        (list 'eq? eq?)
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
     (if ((op eq?) (reg val) (const _*unbound-variable*_)) (label unbound-variable))
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
     (assign unev (op operands) (reg exp))
     (assign exp (op operator) (reg exp))
     (save continue)
     (assign continue (label ev-appl-did-sym-operator))
     (if ((op variable?) (reg exp)) (label ev-variable))
     (save env)
     (save unev)
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)
     (restore env)

     ev-appl-did-sym-operator
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
     (if ((op eq?) (reg val) (const _*illegal-argument*_)) (label illegal-argument))
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
     (assign exp (op first-exp) (reg unev))
     (if ((op last-exp?) (reg unev)) (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

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

     illegal-argument
     (perform (op user-print) (reg argl))
     (assign val (const illegal-argument))
     (goto (label signal-error))

     unbound-variable
     (perform (op user-print) (reg exp))
     (assign val (const unbound-variable))
     (goto (label signal-error))

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
