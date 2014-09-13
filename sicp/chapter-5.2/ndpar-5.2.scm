;; Exercise 5.7, p.515

; a. Recursive exponentiation
(define expt-1-machine
  (make-machine
   '(n b val)
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
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

(set-register-contents! expt-1-machine 'n 5)
(set-register-contents! expt-1-machine 'b 3)
(start expt-1-machine)
(get-register-contents expt-1-machine 'val)

; b. Iterative exponentiation
(define expt-2-machine
  (make-machine
   '(n b c p)
   (list (list '= =) (list '- -) (list '* *))
   '((assign c (reg n))
     (assign p (const 1))
     expt-iter
     (test (op =) (reg c) (const 0))
     (branch (label done))
     (assign c (op -) (reg c) (const 1))
     (assign p (op *) (reg b) (reg p))
     (goto (label expt-iter))
     done)))

(set-register-contents! expt-2-machine 'n 5)
(set-register-contents! expt-2-machine 'b 3)
(start expt-2-machine)
(get-register-contents expt-2-machine 'p)
