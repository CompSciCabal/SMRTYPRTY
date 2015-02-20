;;  Notes

; Footnote 6 on pg 499. Half-joke about contemplation of the meaning of 'true?'

; Pg 506. "Implementing cond in this way simplifies the evaluator because it reduces the number of special forms for which the evaluation process must be explicitly specified".

; Can we imagine an evaluator with no special forms other than lambda?
; How about one with no special forms?
; What would we gain or lose by removing the assignment portions of this evaluator?

;; Exercises

;; 4.1

;; The given is

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

;; If we don't want to care about the underlying cons' order of evaluation, we need to be explicit. Left-to-right would look like

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((head (eval (first-operand exps) env)))
	(cons head (list-of-values (rest-operands exps) env)))))

;; while right-to-left would predictably be

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((tail (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env) tail))))

;; 4.2
;; a) Nope, Louis. Because of our definition of application?, that would cause conflicts with `if?`, `definition?` and some others.
;; b) Fuck you, Louis, I don't want to write `call` before every function application. Hypothetically, if you could find programmers that would, you could then re-define application? as (tagged-list? exp 'call). Good luck with that one.

;; 4.4
;; Mechanical definition. In our view of the world, both would have to be special forms because of the short-circuiting.

;; 4.5
;; This shit is bananas.

;; 4.7
;; Every name/value pair would have to be its own lambda. For instance

(let* ((a 1)
       (b 2)
       (c 3))
  (list a b c))

;; can also be expressed as

((lambda (a)
   ((lambda (b)
      ((lambda (c)
	 (list a b c)) 
       3))
    2)) 
 1)


;; 4.9

;; You'd probably want a minimal map, fold and repeat.
(define (fold-left fn memo list)
  (if (null? list)
      memo
      (fold-left fn (fn (car list) memo) (cdr list))))

(define (map fn list) ;; I'm having fun
  (reverse (fold-left
	    (lambda (a memo)
	      (cons (fn a) memo))
	    nil list)))

(define (repeat fn count)
  (if (zero? count)
      #t
      (begin (fn count)
	     (repeat fn count))))
