#lang racket

;; Pre-requisites

;; Missing definitions
(define (lookup-variable-value) '())
(define (make-procedure) '())
(define (primitive-procedure?) '())
(define (apply-primitive-procedure) '())
(define (compound-procedure?) '())
(define (procedure-body) '())
(define (procedure-parameters) '())
(define (procedure-environment) '())
(define (extend-environment) '())
(define (set-variable-value!) '())
(define (define-variable) '())
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
  (define-variable (definition-variable exp)
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
      (list (stabby-clause-actions clause) (cond-predicate clause))
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