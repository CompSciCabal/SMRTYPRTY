#lang racket

(require r5rs)

(provide (all-defined-out))

(define (false? x) (eq? x false))
(define (true? x) (not (false? x)))

;; -------------------------------------------------------
;; Primary forms
;; -------------------------------------------------------

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

;; Numbers and Strings

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

;; Variables

(define (variable? exp) (symbol? exp))

;; Quotation

(define (quoted? exp) (tagged-list? exp 'quote))
(define text-of-quotation cadr)

;; Assignments

(define (assignment? exp) (tagged-list? exp 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)

;; Definitions

(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

;; If

(define (if? exp) (tagged-list? exp 'if))
(define if-predicate cadr)
(define if-consequent caddr)

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; Lambdas

(define (lambda? exp) (tagged-list? exp 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; Sequences

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))

;; Applications

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; -------------------------------------------------------
;; Derived forms
;; -------------------------------------------------------

;; Let

(define (let? exp) (tagged-list? exp 'let))

(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp))
        (let-vals exp)))

;; Cond

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

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

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

;; -------------------------------------------------------
;; Procedures
;; -------------------------------------------------------

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(define compiled-procedure-entry cadr)
(define compiled-procedure-env caddr)

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
