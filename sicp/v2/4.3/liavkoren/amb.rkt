; Note: run in R5RS
(#%require (only racket/base random))
(define false #f)
(define true #t)

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))        
        (else
         (error "Unknown expression type: ANALYZE" exp))))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type EXECUTE-APPLICATION" proc))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval. (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             ;; the predicate to obtain pred-value
             ; (what?)
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for
             ;; evaluating the predicate
             fail))))


(define (analyze-sequence exp)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; succ cont for a:
         (lambda (a-val fail2)
           (b env succeed fail2))
         ;; fail cont for a:
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially
               first-proc
               (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exp)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ;*2*
                            (set-variable-value! var old-value env)
                            (fail2)))))
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

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success cont for this aprocs
       (lambda (arg fail2)
         (get-args (cdr aprocs) env
                   ;; success cont for recursive call to get-args
                   (lambda (args fail3)
                     (succeed (cons arg args) fail3))
                   fail2))
       fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env succeed (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

(define (shuffle items)
  (define (get-index items)
    ; returns a random index for the list of items
    (random (length items)))
  (define (slice items index)
    ; given a list and an index, slices the item at index out of the list
    (if (= index 0) (cdr items)
        (cons (car items) (slice (cdr items) (- index 1)))))  
  (define (shuffler)
    (if (null? items) '()
        (let* ((index (get-index items))
               (choice (list-ref items index)))
          (set! items (slice items index))
          (cons choice (shuffler)))))
  (shuffler))

(define (analyze-ramb exp)
  (analyze-amb
   (cons 'ramb
         (shuffle (amb-choices exp)))))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Language syntax
; ===============
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

; definitions are either: (define ⟨var⟩ ⟨value⟩) or (define (⟨var⟩ ⟨param1⟩ ... ⟨paramn⟩) ⟨body⟩)
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda 
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
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

(define (begin? exp) 
  (tagged-list? exp 'begin))
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
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp 
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp 
                      (cond-actions first))
                     (expand-clauses 
                      rest))))))

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

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

; syntax fin


(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))


; the environment
; ===============

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
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

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) 
                        (cdr vals)))))
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
             (env-loop 
              (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! 
              var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) 
                        (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Amb
; ===

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb
   (car items)
   (an-element-of (cdr items))))

(define (xor x y)
  (not (equal? x y)))

; REPL
; ====
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'not not)
        (list 'xor xor)
        (list '<= <=)
        (list '>= >=)
        (list '< <)
        (list '> >)
        (list 'list list)
        (list 'member member)
        (list 'abs abs)
        (list 'memq memq)
        (list 'random random)
        (list 'map map)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) 
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))
(define input-prompt  "<<< M-Eval input:")
(define output-prompt ">>> M-Eval value:")


(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ">>> Starting a new problem-o")
            (newline)
            (ambeval input the-global-environment
                     ;; success cont:
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; failure cont:
                     (lambda ()
                       (announce-output ">>> There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop (lambda ()
                   (newline)
                   (display ">>> There is no current problemo")
                   (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
       (list 'compound-procedure
             (procedure-parameters object)
             (procedure-body object)
             '<procedure-env>))
      (display object)))

(define the-global-environment 
  (setup-environment))
(driver-loop)

#|
Exercise 4.35
=============

Write a procedure an-integer-between that returns an integer between two
given bounds. This can be used to implement a procedure that finds
Pythagorean triples, i.e., triples of integers ( i , j , k ) between the
given bounds such that i ≤ j and i 2 + j 2 = k 2 , as follows:

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))
|#

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (interpret exp)
  (ambeval exp the-global-environment
                     ;; success cont:
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; failure cont:
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop))))
(interpret (list '(define (require p)
  (if (not p) (amb)))))
#|
(interpret (list '(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))))

(interpret (list '(define (a-pythagorean-triple-between low high)
      (let ((i (an-integer-between low high)))
        (let ((j (an-integer-between i high)))
          (let ((k (an-integer-between j high)))
            (require (= (+ (* i i) (* j j)) 
                        (* k k)))
            (list i j k)))))))
(interpret (list '(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) 
       (an-element-of (cdr items))))))
(interpret (list '(define (require p)
  (if (not p) (amb)))))
(interpret (list '(a-pythagorean-triple-between 2 8)))
|#

#|
Exercise 4.40
=============

In the multiple dwelling problem, how many sets of assignments
are there of people to floors, both before and after the requirement
that floor assignments be distinct?

Answer: not distinct, it's 5^5, with distinct it's 5!. 

It is very inefficient to generate all possible assignments of people
to floors and then leave it to backtracking to eliminate them. For example,
most of the restrictions depend on only one or two of the person-floor
variables, and can thus be imposed before floors have been selected for
all the people. Write and demonstrate a much more efficient nondeterministic
procedure that solves this problem based upon generating only those possibilities
that are not already ruled out by previous restrictions. (Hint: This will require
a nest of let expressions.) 
|#



(define (an-integer-between low hi)
  (require (<= low hi))
  (amb low (an-integer-between (+ low 1) hi)))

(define (require p) 
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher 
                      miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require
     (not (= (abs (- smith fletcher)) 1)))
    (require 
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (multiple-dwelling-faster)
  (let ((baker (an-integer-between 1 4)))
    (let ((cooper (an-integer-between 2 5)))
      (let ((miller (an-integer-between (+ cooper 1) 5)))
        (let ((fletcher (car (amb (list (an-integer-between 1 (- cooper 2))) (list (an-integer-between (+ cooper 2) 5))))))
          (let ((smith (car (amb (list (an-integer-between 1 (- fletcher 2))) (list (an-integer-between (+ fletcher 2) 5))))))
            (require (not (= fletcher 5)))
            (require (not (= fletcher 1)))
            (require (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

#|
Exercise 4.41
=============

Write an ordinary Scheme program to solve the multiple dwelling puzzle.
|#


#|
Exercise 4.42
=============

Solve the following “Liars” puzzle (from Phillips 1934):

Five schoolgirls sat for an examination. Their parents—so they thought—showed
an undue degree of interest in the result. They therefore agreed that, in
writing home about the examination, each girl should make one true statement
and one untrue one. The following are the relevant passages from their letters:

    Betty: “Kitty was second in the examination. I was only third.”
    Ethel: “You’ll be glad to hear that I was on top. Joan was second.”
    Joan: “I was third, and poor old Ethel was bottom.”
    Kitty: “I came out second. Mary was only fourth.”
    Mary: “I was fourth. Top place was taken by Betty.” 

What in fact was the order in which the five girls were placed? 
|#


(define (liars)
  (let      
      ((kitty (an-integer-between 1 5))
       (betty (an-integer-between 1 5))
       (ethel (an-integer-between 1 5))
       (joan (an-integer-between 1 5))
       (mary (an-integer-between 1 5)))
       (require (xor (= kitty 2) (= betty 3))) ; betty's
       (require (xor (= ethel 1) (= joan 2)))  ; ethel's
       (require (xor (= joan 3) (= ethel 5)))  ; joan's
       (require (xor (= kitty 2) (= mary 4)))  ; kitty's 
       (require (xor (= mary 4) (= betty 1)))  ; mary's 
       (require (distinct? (list kitty betty ethel joan mary)))
       (list (list 'kitty kitty)
             (list 'betty betty)
             (list 'ethel ethel)
             (list 'joan joan)
             (list 'mary mary))))

; ((kitty 1) (betty 3) (ethel 5) (joan 2) (mary 4))



#|
Exercise 4.50
=============

Implement a new special form ramb that is like amb except that it searches alternatives
in a random order, rather than from left to right. Show how this can help with Alyssa’s
problem in Exercise 4.49. 
|#