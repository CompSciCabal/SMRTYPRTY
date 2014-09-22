;; -------------------------------------------------------
;; Designing Register Machines
;; -------------------------------------------------------

;; Exercise 5.2, p.498

(controller
   (assign n (op read))
   (assign c (const 1))
   (assign p (const 1))
 test-c
   (test (op >) (reg c) (reg n))
   (branch (label done))
   (assign c (op +) (reg c) (const 1))
   (assign p (op *) (reg p) (reg c))
   (goto (label test-c))
 done
   (perform (op print) (reg p)))

;; Exercise 5.3, p.502

; With primitive good-enough? and improve
(controller
   (assign x (op read))
   (assign g (const 1.0))
 sqrt-iter
   (test (op good-enough?) (reg g) (reg x))
   (branch (label done))
   (assign g (op improve) (reg g) (reg x))
   (goto (label sqrt-iter))
 done
   (perform (op print) (reg g)))

; With implemented good-enough? and improve
(controller
   (assign x (op read))
   (assign g (const 1.0))
 sqrt-iter
   (assign a (op *) (reg g) (reg g))
   ; input for operation can be only reg or const
   ; see Chapter 5.1.5
   (assign a (op -) (reg a) (reg x))
   (assign a (op abs) (reg a))
   (test (op <) (reg a) (const 0.001))
   (branch (label done))
   (assign a (op /) (reg x) (reg g))
   (assign b (op +) (reg g) (reg a))
   (assign g (op /) (reg b) (const 2))
   (goto (label sqrt-iter))
 done
   (perform (op print) (reg g)))

; Implementation in assembly language
(define newton-machine
  (make-machine
    (list (list '< <)
          (list '- -)
          (list '* *)
          (list '+ +)
          (list '/ /)
          (list 'abs abs))
    '(sqrt-iter
      (assign a (op *) (reg g) (reg g))
      (assign a (op -) (reg a) (reg x))
      (assign a (op abs) (reg a))
      (test (op <) (reg a) (const 0.001))
      (branch (label done))
      (assign a (op /) (reg x) (reg g))
      (assign b (op +) (reg g) (reg a))
      (assign g (op /) (reg b) (const 2))
      (goto (label sqrt-iter))
      done)))

(set-register-contents! newton-machine 'x 4)
(set-register-contents! newton-machine 'g 1.0)
(start newton-machine)
(get-register-contents newton-machine 'g) ;=> 2.0000000929222947

;; Exercise 5.4, p.510

; a. Recursive exponentiation
(controller
   (assign continue (label done))
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
 done)

; b. Iterative exponentiation
(controller
   (assign b (op read))
   (assign n (op read))
   (assign c (reg n))
   (assign p (const 1))
 expt-iter
   (test (op =) (reg c) (const 0))
   (branch (label done))
   (assign c (op -) (reg c) (const 1))
   (assign p (op *) (reg b) (reg p))
   (goto (label expt-iter))
 done
   (perform (op print) (reg p)))

;; Exercise 5.6, p.512
;; Run in simulator

(define fib-machine
  (make-machine
    (list (list '< <) (list '- -) (list '+ +))
    '((assign continue (label fib-done))
      fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
      afterfib-n-1
      (restore n)
      (assign n (op -) (reg n) (const 2))
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
      afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
      immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 6)
(start fib-machine)
(get-register-contents fib-machine 'val) ;=> 8

;; Exercise 5.12, p.530

(display (get-info fib-machine))

((instructions (assign val (reg n))
               (assign continue (label fib-done))
               (assign val (op +) (reg val) (reg n))
               (assign continue (label afterfib-n-1))
               (assign n (reg val))
               (assign n (op -) (reg n) (const 1))
               (assign continue (label afterfib-n-2))
               (assign n (op -) (reg n) (const 2))
               (branch (label immediate-answer))
               (goto (reg continue))
               (goto (label fib-loop))
               (restore continue)
               (restore val)
               (restore n)
               (save continue)
               (save val)
               (save n)
               (test (op <) (reg n) (const 2)))
 (entry-points continue)
 (stack-regs n val continue)
 (sources (n ((op -) (reg n) (const 2))
             ((op -) (reg n) (const 1))
             ((reg val)))
          (continue ((label afterfib-n-2))
                    ((label afterfib-n-1))
                    ((label fib-done)))
          (val ((op +) (reg val) (reg n))
               ((reg n)))))
