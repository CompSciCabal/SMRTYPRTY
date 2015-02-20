;; -------------------------------------------------------
;; A Register-Machine Simulator
;; -------------------------------------------------------

;; Exercise 5.7, p.515

; a. Recursive exponentiation
(define expt-1-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label done))
     expt-loop
     (if ((op =) (reg n) (const 0)) (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     done)))

(expt-1-machine 'trace-on)
(set-register-contents! expt-1-machine 'n 5)
(set-register-contents! expt-1-machine 'b 3)
(start expt-1-machine)
(get-register-contents expt-1-machine 'val)
(print-statistics expt-1-machine)
(print (get-info expt-1-machine))

; b. Iterative exponentiation
(define expt-2-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '((assign c (reg n))
     (assign p (const 1))
     expt-iter
     (if ((op =) (reg c) (const 0)) (label done))
     (assign c (op -) (reg c) (const 1))
     (assign p (op *) (reg b) (reg p))
     (goto (label expt-iter))
     done)))

(set-register-contents! expt-2-machine 'n 5)
(set-register-contents! expt-2-machine 'b 3)
(start expt-2-machine)
(get-register-contents expt-2-machine 'p)
(print-statistics expt-2-machine)
(print (get-info expt-2-machine))

;; Exercise 5.8, p.523
;; Ambiguous labels

(define ex-5-8-machine
  (make-machine
    (list)
    '(start
      (goto (label here))
      here
      (assign a (const 3))
      (goto (label there))
      here
      (assign a (const 4))
      (goto (label there))
      there)))

;(start ex-5-8-machine)
;(get-register-contents ex-5-8-machine 'a) ;=> 3

;; Exercise 5.9, p.529
;; Labels cannot be used as operands

(make-machine
 (list (list '+ +))
 '((assign a (op +) (const 1) (label done))
   done))

;; Exercise 5.11, p.529
;; Restoring wrong register

(define ex-5-11-machine
  (make-machine
    (list)
    '((assign x (const 5))
      (assign y (const 6))
      (save y)
      (save x)
      (restore y))))

(start ex-5-11-machine)

;; Exercise 5.14, p.532
;; Monitoring stack of recursive factorial machine
;; in interactive mode

(define (fact-init-machine)
  (init-machine fact-machine))

(define (fact-print-info)
  (print (get-info fact-machine)))

(define (fact-init-stack)
  (init-stack fact-machine))

(define (fact-print-stats)
  (print-statistics fact-machine))

(define fact-machine
  (make-machine
    (list (list '= =) (list '* *) (list '- -)
          (list 'read read) (list 'print print)
          (list 'init-machine fact-init-machine)
          (list 'print-info fact-print-info)
          (list 'fact-stats fact-print-stats)
          (list 'fact-init fact-init-stack))
    '(fact-start
      (perform (op init-machine))
      (perform (op fact-init))
      (assign n (op read))
      (assign continue (label fact-done))
      fact-loop
      (if ((op =) (reg n) (const 1)) (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))
      (goto (reg continue))
      base-case
      (assign val (const 1))
      (goto (reg continue))
      fact-done
      (perform (op print) (reg val))
      (perform (op fact-stats))
      (perform (op print-info))
      (goto (label fact-start)))))

(start fact-machine)

