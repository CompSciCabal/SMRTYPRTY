#lang racket

(require compatibility/defmacro)

(define (send! to pin message)
  (thread-send to (cons pin message)))

(define-macro (make-part body)
  (let ((fn (gensym))
	(connections (gensym))
	(setup (gensym)))
    `(letrec ((,connections '())
	      (out! (lambda (pin msg)
		      (map (connection-lookup pin ,connections)
			   (lambda (target pin)
			     (send! target pin msg)))))
	      (,fn (lambda ()
		     (match (thread-receive)
		       [(cons pin msg) ,@body]
		       [junk (out! 'error "Malformed message")])
		     (,fn)))
	      (,setup (lambda ()
			(match (thread-receive)
			  [(list 'internal 'reset-connections conns)
			   (set! ,connections conns)
			   (,fn)]
			  [junk (,setup)]))))
       (thread ,fn))))

(define connection-lookup (k table)
  (let ((targets (assoc k table)))
    (if targets
	(cdr targets)
	'())))

(define filter-for (part table)
  (map (lambda (conn) (cons (cadar conn) (cdr conn)))
       (filter (lambda (conn) (eq? (caar conn) part))
	       table)))

(define (send-connection-table! to full-table)
  (thread-send to (list 'internal 'reset-connections (filter-for to full-table))))

(define-macro (make-proxy parent parts . system-map)
  (define (convert-item itm)
    `(list ,(car itm) ',(cadr itm)))
  (define (convert-line conn-line)
    `(cons ,(convert-item (car conn-line)) (list ,@(map convert-item (cddr conn-line)))))
  (define (part->f-binding part-pair)
    `(,(car part-pair) #f))
  (define (part->set-form part-pair)
    `(set! ,(car part-pair) ,(cadr part-pair)))
  (let ((pt (gensym))
	(launch (gensym))
	(fn (gensym))
	(initial (gensym))
	(setup (gensym)))
    `(let ((,pt ,parent)
	   (self #f)
	   (connections #f))
       (let ((,launch
	      (lambda (msg)
		(lambda (part/pin)
		  (cond ((and ,pt (eq? (first part/pin) self))
			 (thread-send ,pt (list (current-thread) (second part/pin) msg)))
			((not (eq? (first part/pin) self))
			 (thread-send (first part/pin) (cons (second part/pin) msg))))))))
	 (let ,(map part->f-binding parts)
	   (letrec ((,fn (lambda ()
			   (match (thread-receive)
			     [(list src pin msg)
			      (map (,launch msg) (connection-lookup (list src pin) connections))]
			     [(cons pin msg)
			      (map (,launch msg) (connection-lookup (list self pin) connections))])
			   (,fn)))
		    (,initial (lambda ()
				(set! self (current-thread))
				,@(map part->set-form parts)
				(,setup)))
		    (,setup (lambda ()
			      (match (thread-receive)
				[(list 'internal 'reset-connections conns)
				 (set! ,connections conns)
				 ,@(map 
				    (lambda (part-pair)
				      `(send-connection-table! ,(car part-pair) connections))
				    parts)
				 (,fn)]
				[junk (,setup)]))))
	     (thread ,setup)))))))

(define (make-counter parent)
  (let ((count 0))
    (make-part parent
	       (set! count (+ 1 count))
	       (out! 'out count))))

(define (make-greeter parent template)
  (make-part parent (out! 'out (format template msg))))

(define test 
  (make-proxy #f
   ((pr (make-part self (displayln (format "Printing: ~a" msg))))
    (ct (make-counter self))
    (greet (make-greeter self "Hello there, ~a!")))
   ((self in) -> (ct in) (pr in) (greet in))
   ((ct out) -> (pr in))
   ((greet out) -> (pr in))))

(define (load-test num)
  (unless (zero? num)
    (send! test 'in (format "Name-~a" num))
    (load-test (- num 1))))

;; (enter! "fbp.rkt")
(send! test 'in "Inaimathi")
(send! test 'in "Garamond")
(send! test 'in "Corsiva")
