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
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env))]
        [else (error "Unknown expression type -- EVAL" exp)]))

(define (apply procedure arguments)
  (cond [(primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments)]
        [(compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
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
  (if (true? (eval (if-predicate exp) env))
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

; ------------------------------------

(displayln "exercise 4.1")
(define (left-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let [(left (eval (first-operand exps) env))]
        (let [(right (left-list-of-values (rest-operands exps) env))]
          (cons left right)))))

(define (right-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let* [(right (right-list-of-values (rest-operands exps) env))
             (left  (eval (first-operand exps) env))]
        (cons left right))))


(displayln "exercise 4.2")
#|
a.
The problem with Louis's approach is that everything would be considered as an
application due to the definition of `application?` (all applications are simply
pairs)
|#

;; b.
(define (louis-application exp) (tagged-list? exp 'call))
(define (louis-operator exp) (cadr exp))
(define (louis-operands exp) (cddr exp))

(displayln "exercise 4.3")

(define lookup (make-hash))
(define (get key) (hash-ref lookup key))
(define (add key proc) (hash-set! lookup key proc))


(define (text-of-quotation-with-env exp env)
  (text-of-quotation exp))
(add 'quote text-of-quotation-with-env) ;; with-env is just to ensure a consistent API
(add 'set! eval-assignment)
(add 'define eval-definition)
(add 'if eval-if)

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
(add 'lambda eval-lambda)
(add 'begin (lambda (exp env)
              (eval-sequence (begin-actions exp) env)))

(define (eval-cond exp env)
  (eval (cond->if exp) env))
(add 'cond eval-cond)

(define (eval-application exp env)
  (apply (eval (operator exp) env)
         (list-of-values (operands exp) env)))

(define (is-defined? tag)
  (hash-has-key? lookup tag))

(define (data-directed-eval exp env)
  (cond [(self-evaluating? exp) exp]
        [(variable? exp) (lookup-variable-value exp env)]
        [(is-defined? (car exp)) ((get (car exp)) exp env)]
        [(application? exp) (eval-application exp env)]
        [else (error "Unknown expression type -- EVAL" exp)]))

(displayln "exercise 4.4")
#|
These would get mounted by doing the following to eval:
[(and? exp) (eval-and (cdr exp) env)]
[(or? exp) (eval-or (cdr exp) env)]
|#
(define (and? exp) (tagged-list? 'and exp))
(define (or? exp) (tagged-list? 'or exp))

(define (eval-and exp env)
  (cond [(null? exp) true]
        [(true? (apply (car exp) env)) (eval-and (cdr exp) env)]
        [else false]))

(define (eval-or exp env)
  (cond [(null? exp) false]
        [(true? (apply (car exp) env)) true]
        [else (eval-or (cdr exp) env)]))

#|
For the derived expressions we'd do something like this:
[(and? exp) (eval (and->if (cdr exp) env))]
[(or? exp) (eval (or->if (cdr exp) env))]
|#
(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))

(define (and->if exps env) (expand-and-clauses (and-clauses exp)))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (let [(first (car clauses))
            (rest (cdr clauses))]
        (make-if first
                 (expand-and-clauses rest)
                 'false))))

(define (or->if exps env) (expand-or-clauses (or-clauses exp)))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (let [(first (car clauses))
            (rest (cdr clauses))]
        (make-if first
                 'true
                 (expand-or-clauses rest)))))

(displayln "exercise 4.5")

(define (stabby-clause? clause) (eq? '=> (cadr clause)))
(define (stabby-clause-actions clause) (caddr clause))

(define (improved-cond-actions clause)
  (if (stabby-clause? clause)
      (list (stabby-clause-actions clause) (list (cond-predicate clause)))
      (sequence->exp (cond-actions clause))))

(define (extended-expand-clauses clauses)
  (if (null? clauses)
      'false
      (let [(first (car clauses))
            (rest (cdr clauses))]
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF" clauses))
            (make-if (cond-predicate first)
                     (improved-cond-actions first)
                     (expand-clauses rest))))))

(displayln "exercise 4.6")
(define (let? exp) (tagged-list? exp 'let))
(define (let-defs exp) (cadr exp))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
  (expand-let (let-defs exp) (let-body exp) '() '()))

#|
This solution is iterative but suffers from the problem
that it builds up the arguments list in reverse order.

The variables will have the same names and values, but
instead of showing up in order '((lambda (a b c) body) (1 2 3))
we'll end up with something that looks like this:
'((lambda (c b a) body) (3 2 1))

The results of the computation will be the same
|#
(define (expand-let defs body arguments expressions)
  ;; ('let [(a (+ 1 2)) (b (+ 2 3)) ...] body)
  (define (variable-name) (caar defs))
  (define (variable-expr) (cadar defs))
  (if (null? defs)
      (cons (make-lambda arguments body) expressions)
      (expand-let (cdr defs)
                  body
                  (cons variable-name arguments)
                  (cons variable-expr expressions))))

(displayln "exercise 4.7")
#|
Let* can be implemented by simply creating a bunch
of lets that only define a single value and continuously
recur until there aren't any lets remaining.
|#
(define (let*->nested-lets exp)
  (transform-let* (cadr exp) (cddr exp)))

(define (transform-let* assignment body)
  (if (null? assignment)
      (cons 'let (cons assignment body))
      (list 'let
            (list (car assignment))
            (transform-let* (cdr assignment) body))))

(displayln "exercise 4.8")
(define (improved-let->combination exp)
  (expand-let (let-defs exp) (let-body exp) '() '()))

(define (expand-named-let name bindings body)
  (define (var-names bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings)
              (var-names (cdr bindings)))))
  (define (var-values bindings)
    (if (null? bindings)
        '()
        (cons (cdar bindings)
              (var-values (cdr bindings)))))
  
  (sequence->exp
   (list (cons 'define
               (cons (cons name (var-names bindings))
                     body))
         (cons name (var-values bindings)))))

(displayln "exercise 4.9")

#|
('while condition body)
(while
  (< i 10)
  (set! i (+ i 1)))

(define (while cond body)
  (define (while-loop)
    (if (cond)
        (begin
          (body)
          (while-loop))
         #f))
  (while-loop))
|#

(define (while? exp) (tagged-list? exp 'while))
(define (while->combination exp)
  (define (while-condition) (cadr exp))
  (define (while-body) (caddr exp))
  (list
   (list 'define
         (list 'while-loop)
         (make-if (while-condition)
                  (sequence->exp
                   (list (while-body))
                   (list 'while-loop))
                  'done))
   (list 'while-loop)))

(displayln "exercise 4.10")
;; I honestly don't have anything here for this. This Scheme language that
;; we have been implementing has a lot of the features that I'd want that
;; aren't in racket (i.e. always need else in an if clause, etc.)
;;
;; Maybe something that would be useful would be making it possible to set!
;; multiple things from a single call:
;; (set! a 1 b 2 c 3)
;;
(define (extended-eval-assignment exp env)
  (define (done?) (null? exp))
  (define (assignment-variable exp) (cadr exp))
  (define (assignment-value exp) (caddr exp))
  (if (done?) 'ok
      ;; Which environment is getting modified by set-variable-value?
      ;; Will those values get saved in `env` or do I need to capture
      ;; that environment and pass it around?
      (begin (set-variable-value! assignment-variable
                                  (eval (assignment-value) env))
             (extended-eval-assignment (cdr exp) env))))

#| Environment Prerequisites |#
(define (mcaar x) (mcar (mcar x)))
(define (mcdar x) (mcdr (mcar x)))

(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

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

(displayln "exercise 4.12")
(define (environment-action var env on-exists on-missing)
  (define (env-loop env)
    (define (on-empty-bindings frame)
      (if (null? on-missing)
          (env-loop (enclosing-environment env))
          (on-missing frame)))
    (define (scan bindings)
      (cond [(null? bindings)
             (on-empty-bindings bindings)]
            [(eq? var (mcaar bindings))
             (on-exists bindings)]
            [else (scan (mcdr bindings))]))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- ENVIRONMENT-ACTION" var)
        (scan (first-frame env))))
  (env-loop env))

(define (simple-lookup-variable-value var env)
  (environment-action var env
                      (lambda (bindings) ;; on-exists
                        (mcdar bindings))
                      '())) ;; on-missing

(define (simple-set-variable-value! var val env)
  (environment-action var env
                      (lambda (bindings) ;; on-exists
                        (set-mcdr! (mcar bindings) val))
                      '())) ;; on-missing

(define (simple-define-variable! var val env)
  (environment-action var env
                      (lambda (bindings) ;; on-exists
                        (set-mcdr! (mcar bindings) val))
                      ;; From what I'm seeing. Frame and Bindings are the same thing
                      ;; I probably made a mistake in naming things or something
                      (lambda (frame) ;; on-missing
                        (add-binding-to-frame! var val frame))))

(displayln "exercise 4.13")
;; If we design make-unbound! to only remove the first occurance of the variable definition, it's actually
;; pretty easy to do using the previously defined `environment-action` procedure.
;; If we want to completely blow away all bindings to a variable it would be a bit more involved and I think
;; it would require duplicating a lot of the code in `environment-action`.

(define (make-unbound! var env)
  (environment-action var env
                      (lambda (bindings) ;; on-exists
                        (let [(bind-rest (mcdr bindings))]
                          (set-mcdr! bindings (mcdr bind-rest))
                          (set-mcar! bindings (mcar bind-rest))))
                      '()))
(define some-env (mlist (mlist (mcons 'a 1) (mcons 'b 2) (mcons 'c 3))))
;; (displayln some-env)
(make-unbound! 'b some-env)
;; (displayln some-env)

(displayln "exercise 4.14")
#|
The system version of map isn't expecting the data
to be in the format that our metacircular evaluator
is storing the data. As a result, we end up mapping
over values incorrectly and don't apply things in the
right way. (i.e. passing in symbol + instead of the <proc:+>
to map)

Eva Lu Ator on the other hand was able to push her
map function through the internal eval/apply operations
which properly destructure the data from it's representation.
|#

(displayln "exercise 4.15")

#|
If (halts? try try) were to return true, we'd run forever.
Meanwhile, if (halts? try try) were to return false we'd get
back 'halted. But this puts us in a paradox

Turing proved that it's impossible to know for sure if a
program will ever finish computation.
|#

(displayln "exercise 4.16")
(define (lookup-variable-value-with-unassigned var env)
  (define (raise-or-return-value var val)
    (if (eq? '*unassigned* val)
        (error "Unbound variable" var)
        val))
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcaar bindings))
             (raise-or-return-value var (mcdar bindings)))
            (else (scan (mcdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (scan-out-defines body)
  ;; I found this one after trying out my own.
  ;; While mine worked, it was pretty gross and this
  ;; solution is far more readable and elegant.
  ;; https://github.com/qiao/sicp-solutions/blob/a2fe069ba6909710a0867bdb705b2e58b2a281af/chapter4/4.16.scm
  (let* [(definitions (filter definition? body))
         (rest-expressions
          (filter
           (lambda (expr) (not (definition? body)))
           body))
         (vars (map definition-variable definitions))
         (vals (map definition-value definitions))]
    (define (make-unassigned-binding var)
      (list var '*unassigned*))
    (define (make-assignment var val)
      (list 'set! var val))
    (append (list 'let (map make-unassigned-binding vars))
            (map make-assignment vars vals)
            rest-expressions)))

;; 4.16 c) make-procedure because that protects us from accidentally
;; calling scan-out-defines

(displayln "exercise 4.17")
;; Nope not drawing anything
;; Lets are transformed into lambdas which explains why
;; the extra frame is created. This protects us from the
;; outer environment.
;; Simultaneous scope rule... no.

(displayln "exercise 4.18")
#| This will not work because when a and b are evaluated
| the values they are trying to access will not be correct.
| Instead they will be '*undefined* when actually trying to
| use the values.
| In the other implementation everything is fine because the
| a lambda is created with [i.e. (lambda (y dy) ...)] and
| evaluated with initial values of '*undefined*. The values
| are then evaluated and set as expected.
|#

(displayln "exercise 4.19")
#|
| I support Alyssas view since b is trying to use a
| value that is yet to be defined. We could reach out
| to the outer scope, but that would be somewhat misleading
| and it would be preferable to simply let the user know
| that they have written weird code.
|
| One way to implement Evas solution would be to determine
| if the definition is a constant one or an actual evaluation.
| In the case of constants, we could ensure those are always
| defined first. While this may help, it doesn't necessarily resolve
| issues where some evaluations rely on others.
|
| Another alternative to that would basically be dependency
| resolution, which opens a whole other can of worms.
|#

(displayln "exercise 4.20")
;; a.
(define (letrec-definitions exp) (cadr exp))
(define (letrec-body exp) (caddr exp))

(define (letrec exps env) (expand-letrec (letrec-definitions exps) (letrec-body exps)))
(define (expand-letrec lets body)
  (define vars (map car lets))
  (define vals (map cdr lets))
  (define initial-definitions (map (lambda (x) (list x '*undefined*)) vars))
  (define set-definitions (map (lambda (var val) (list 'set! var val)) vars vals))
  (list 'let
        initial-definitions
        (make-begin
         set-definitions
         body)))
  
;; b. The problem with Louis' logic is that odd? / even? cannot be declared in the same
;; scope. If they were defined in nested lets, one wouldn't know about the other.
;; Finally, if they were passed into a function as arguments, they would be missing the binding
;; in order to both call eachother.
;; The end result is that his approach doesn't work in practice and requires a function like letrec