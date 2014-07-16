#lang racket

(require compatibility/defmacro)

(define-macro (send! to pin message)
  `(thread-send ,to (cons ,pin ,message)))

(define-macro (make-part parent . body)
  (let ((fn (gensym))
	(pr (gensym)))
    `(letrec ((,pr ,parent)
	      (out! (lambda (pin msg)
		      (let ((m (list (current-thread) pin msg)))
			(if ,pr
			    (thread-send ,pr m)
			    (displayln m)))))
	      (,fn (lambda ()
		     (displayln (format "Reactor ~a waiting for input with parent ~a..." (current-thread) ,pr))
		     (match (thread-receive)
		       [(cons pin msg) ,@body]
		       [junk (out! 'error "Malformed message")])
		     (,fn))))
       (thread ,fn))))

(define-macro (make-proxy parent parts . system-map)
  (define (convert-item itm)
    `(list ,(car itm) ',(cadr itm)))
  (define (convert-line conn-line)
    `(cons ,(convert-item (car conn-line)) (list ,@(map convert-item (cddr conn-line)))))
  (let ((pt (gensym))
	(launch (gensym))
	(fn (gensym)))
    `(thread
      (letrec ((,fn (lambda ()      
		      (let ((,pt ,parent)
			    (self (current-thread))) ;; that's the problem, I think. We need self set inside of the function for this thread, not outside
			(displayln (format "Started container: ~s" self))
			(let ,parts
			  (let ((connections
				 (list ,@(map convert-line system-map)))
				(,launch
				 (lambda (msg)
				   (lambda (part/pin)
				     (displayln (format "Launching '~a' at ~s" msg part/pin))
				     (cond ((and ,pt (eq? (first part/pin) self))
					    (displayln "Launching at parent...")
					    (thread-send ,pt (list (current-thread) (second part/pin) msg)))
					   ((not (eq? (first part/pin) self))
					    (displayln (format "Launching at ~s..." (first part/pin)))
					    (thread-send (first part/pin) (cons (second part/pin) msg))))))))
			    (match (thread-receive)
			      [(cons pin msg)
			       (displayln (format "Got message ~a::~s..." pin msg))
			       (let ((targets (or (assoc (list self pin) connections) '())))
				 
				 (map (,launch msg) targets))]
			      [(list src pin msg)
			       (displayln "Got internal message...")
			       (let ((targets (or (assoc (list src pin) connections) '())))
				 (map (,launch msg) targets))])
			    (,fn)))))))
	,fn))))

(define test 
  (make-proxy #f
	      ((pr (make-part self (displayln (format "Printing: ~a" msg))))
	       (ct (let ((count 0))
		     (make-part self 
				(displayln "Counting...")
				(set! count (+ 1 count))
				(out! 'out count))))
	       (greet (make-part self 
				 (displayln "Greeting...")
				 (out! 'out (format "Hello there, ~a!" msg)))))
	      ((self in) -> (ct in) (pr in) (greet in))
	      ((ct out) -> (pr in))
	      ((greet out) -> (pr in))))

;; (enter! "fbp.rkt")
;; (send! test 'in "Leo")

;; (make-proxy parent
;;  ((pr (new-printer))
;;   (ct (new-counter))
;;   (greet (new-greeter)))
;;  ((self in) -> (ct in) (pr in) (greet in))
;;  ((greet out) -> (pr in)))

;; (thread
;;  (let ((self (current-thread))
;;        (pr (new-printer))
;;        (ct (new-counter))
;;        (greet (new-greeter)))
;;    (let ((connections
;; 	  (list (cons (list self 'in) (list (list ct 'in) (list pr 'in) (list greet 'in)))
;; 		(cons (list greet 'out) (list (list pr 'in)))))
;; 	 (launch
;; 	  (lambda (msg)
;; 	    (lambda (part/pin)
;; 	      (cond ((and parent (eq? (first part/pin) self))
;; 		     (thread-send parent (list (current-thread) (second part/pin) msg)))
;; 		    ((not (eq? (first part/pin) self))
;; 		     (thread-send (first part/pin) (cons (second part/pin) msg))))))))
;;      (lambda ()
;;        (match (thread-receive)
;; 	 [(cons pin msg)
;; 	  (let ((targets (assoc (list self pin) connections)))
;; 	    (map (launch msg) targets))]
;; 	 [(list src pin msg) 
;; 	  (let ((targets (assoc (list src pin) connections)))
;; 	    (map (launch msg) targets))])))))
