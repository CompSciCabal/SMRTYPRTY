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
