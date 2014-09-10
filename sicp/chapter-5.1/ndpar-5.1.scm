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
