#lang planet neil/sicp

;; -------------------------------------------------------
;; The AMB Evaluator
;; -------------------------------------------------------

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((variable? exp) (analyze-variable exp))
        ((quoted? exp) (analyze-quoted exp))
        ((assignment? exp) (analyze-assignment exp))
        ((perm-assignment? exp) (analyze-perm-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((if? exp) (analyze-if exp))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (perm-assignment? exp) (tagged-list? exp 'permanent-set!))

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

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ramb? exp) (tagged-list? exp 'ramb))

(define (if-fail? exp) (tagged-list? exp 'if-fail))

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

(define (and? exp) (tagged-list? exp 'and))
(define (and->if exp) (expand-and-operands (operands exp)))

(define (expand-and-operands ops)
  (if (no-operands? ops)
      'true
      (make-if (first-operand ops)
               (expand-and-operands (rest-operands ops))
               'false)))

(define (or? exp) (tagged-list? exp 'or))
(define (or->if exp) (expand-or-operands (operands exp)))

(define (expand-or-operands ops)
  (if (no-operands? ops)
      'false
      (make-if (first-operand ops)
               'true
               (expand-or-operands (rest-operands ops)))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp))
        (let-vals exp)))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (let ((let-bindings (cadr exp))
        (let-body (caddr exp)))
    (define (expand-let-bindings bindings)
      (if (null? bindings)
          let-body
          (list 'let
                (list (car bindings))
                (expand-let-bindings (cdr bindings)))))
    (expand-let-bindings let-bindings)))

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
;; Syntactic Analysis
;; -------------------------------------------------------

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-quoted exp)
  (let ((qval (cadr exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-val (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-val env)
                            (fail2)))))
             fail))))

(define (analyze-perm-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if-fail exp)
  (let ((sproc (analyze (cadr exp)))
        (fproc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (sproc env
             (lambda (value fail2)
               (succeed value fail2))
             (lambda ()
               (fproc env succeed fail))))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             (lambda (value1 fail2)
               (proc2 env succeed fail2))
             fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed nil fail)
      ((car aprocs) env
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed fail))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-ramb expr)
  (analyze-amb (cons 'amb (shuffle (amb-choices expr)))))

(define (shuffle items)
  (if (null? items)
      '()
      (let ((ls (take-random items)))
        (cons (car ls) (shuffle (cdr ls))))))

(define (take-random items)
  (take-at (random (length items)) items))

(define (take-at index items)
  (define (prepend list1 list2)
    (if (null? list2)
        list1
        (prepend (cons (car list2) list1)
                 (cdr list2))))
  (define (drop i items discarded)
    (if (= i 0)
        (cons (car items)
              (prepend (cdr items) discarded))
        (drop (- i 1)
              (cdr items)
              (cons (car items) discarded))))
  (drop index items '()))

;; -------------------------------------------------------
;; Global Environment
;; -------------------------------------------------------

(define primitive-procedures
  (list (list 'null? null?)
        (list 'list list)
        (list 'cons cons)
        (list 'car car)
        (list 'cdr cdr)
        (list 'shuffle shuffle)
        (list 'equal? equal?)
        (list 'eq? eq?)
        (list 'not not)
        (list 'even? even?)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

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
    (define-variable! 'nil nil initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; -------------------------------------------------------
;; "Built-in" Compound Procedures
;; -------------------------------------------------------

(define (eval exp) ; p.429
  (ambeval exp
           the-global-environment
           (lambda (value fail) value)
           (lambda () 'failed)))

(eval
 '(define (amb-list items)
    (if (null? items)
        (amb)
        (amb (car items) (amb-list (cdr items))))))

(eval
 '(define (ramb-list items)
    (amb-list (shuffle items))))

(eval
 '(define (require p)
    (if (not p) (amb))))

(eval
 '(define (!= a b) (not (= a b))))

(eval
 '(define (<= a b)
    (or (< a b) (= a b))))

(eval
 '(define (abs x)
    (if (< x 0) (- x) x)))

(eval
 '(define (in x items f)
    (cond ((null? items) false)
          ((f x (car items)) true)
          (else (member x (cdr items))))))

(eval
 '(define (memq x items)
    (in x items eq?)))

(eval
 '(define (member x items)
    (in x items equal?)))

(eval
 '(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items))))))

(eval
 '(define (map f items)
    (if (null? items)
        '()
        (cons (f (car items)) (map f (cdr items))))))

(eval
 '(define (foldr f init seq)
    (if (null? seq)
        init
        (f (car seq) (foldr f init (cdr seq))))))

(eval
 '(define (append seq1 seq2)
    (foldr cons seq2 seq1)))

(eval
 '(define (flatmap f items)
    (foldr append '() (map f items))))

;; -------------------------------------------------------
;; REPL
;; -------------------------------------------------------

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

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

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(driver-loop)
