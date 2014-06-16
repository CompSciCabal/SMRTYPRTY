#lang racket

(require "put-get.scm")
(require "streams.scm")
(require (only-in r5rs scheme-report-environment))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define user-initial-environment (scheme-report-environment 5))

;; -------------------------------------------------------
;; Helper Functions
;; -------------------------------------------------------

(eval '(define (slist->string slist)
         (apply string-append (map symbol->string slist)))
      user-initial-environment)

(eval '(define (slist<? a b)
         (string<? (slist->string a) (slist->string b)))
      user-initial-environment)

;; -------------------------------------------------------
;; Driver Loop and Instantiation
;; -------------------------------------------------------

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (stream-display
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; -------------------------------------------------------
;; Evaluator
;; -------------------------------------------------------

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc ; special forms
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; -------------------------------------------------------
;; Compound Queries
;; -------------------------------------------------------

(define (conjoin conjuncts frame-stream)
  (if (null? conjuncts)
      frame-stream
      (conjoin (cdr conjuncts)
               (qeval (car conjuncts) frame-stream))))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (null? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (car disjuncts) frame-stream)
       (delay (disjoin (cdr disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

;; -------------------------------------------------------
;; Filters
;; -------------------------------------------------------

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (car operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pattern var: LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

;; -------------------------------------------------------
;; Pattern Matching
;; -------------------------------------------------------

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;; -------------------------------------------------------
;; Rules and Unification
;; -------------------------------------------------------

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let* ((clean-rule (rename-variables-in rule))
         (unify-result
          (unify-match query-pattern
                       (conclusion clean-rule)
                       query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;; -------------------------------------------------------
;; Maintaining Database
;; -------------------------------------------------------

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      THE-ASSERTIONS))

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      THE-RULES))

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (stream-cons assertion old-assertions))
    'ok))

(define (store-assertion-in-index assertion)
  (cond ((indexable? assertion)
         (let* ((key (index-key-of assertion))
                (current-assertion-stream
                 (get-stream key 'assertion-stream)))
           (put key
                'assertion-stream
                (stream-cons assertion
                             current-assertion-stream))))))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (stream-cons rule old-rules))
    'ok))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (cond ((indexable? pattern)
           (let* ((key (index-key-of pattern))
                  (current-rule-stream
                   (get-stream key 'rule-stream)))
             (put key
                  'rule-stream
                  (stream-cons rule
                               current-rule-stream)))))))

(define (indexable? pat)
  (or (symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat) (symbol? (car pat)))

;; -------------------------------------------------------
;; Query Syntax Procedures and Frames
;; -------------------------------------------------------

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule)
  (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;; -------------------------------------------------------
;; Installed Relations and Rules
;; -------------------------------------------------------

(add-rule-or-assertion! '(rule (same (? x) (? x))))


(query-driver-loop)

;; -------------------------------------------------------
;; Tests
;; -------------------------------------------------------

(eval '(slist->string '(a b c))
      user-initial-environment)

(eval '(slist<? '(a b c) '(a c c))
      user-initial-environment)
