((env)
 (val)
 ;; Assuming n = 5
 ;; ---------------------------
 ;; Total stack pushes: # 26
 ;; save argl: 4
 ;; save continue: 9
 ;; save env: 5
 ;; save proc: 8
 ;; restore argl: 4
 ;; restore continue: 9
 ;; restore env: 5
 ;; restore proc: 8
 ;; ---------------------------
 ;; Max stack depth: ^ 14
 ;; On every recursive call (false-branch4),
 ;; stack grows by 3 entries:
 ;; save continue: 4
 ;; save proc: 4
 ;; save argl: 4

 (;; construct the procedure and skip over code for the procedure body
  (assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))

  entry1 ; calls to factorial will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) ; n => 5, 4, 3, 2, 1
  ;; begin actual procedure body
  (save continue) ; # 5 ^ 13
  (save env) ; # 5 ^ 14
  ;; p-code: compute (= n 1)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch6))

  compiled-branch7 ; never called
  (assign continue (label after-call8))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => (= 5 1),(= 4 1),(= 3 1),(= 2 1),(= 1 1)

  after-call8 ; p-code: val now contains result of (= n 1)
  (restore env) ; # 5 ^ 13
  (restore continue) ; # 5 ^ 12
  (if ((op false?) (reg val)) (label false-branch4))

  true-branch3 ; return 1
  (assign val (const 1)) ; c-code begin
  (goto (reg continue)) ; c-code end

  false-branch4
  ;; a-code: compute and return (* (factorial (- n 1)) n)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue) ; # 4 ^ 10
  (save proc) ; # 4 ^ 11 - save * procedure
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val)) ; argl => (5),(4),(3),(2)
  (save argl) ; # 4 ^ 12 - save partial argument list for *
  ;; compute (factorial (- n 1)), which is the other argument for *
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (save proc) ; # 4 ^ 13 - save factorial procedure
  ;; compute (- n 1), which is the argument for factorial
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl)) ; argl => (n 1)
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch9))

  compiled-branch10 ; never called
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch9
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => 4, 3, 2, 1

  after-call11 ; val now contains result of (- n 1)
  (assign argl (op list) (reg val)) ; argl => (4),(3),(2),(1)
  (restore proc) ; # 4 ^ 12 - restore factorial
  ;; apply factorial
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch12))

  compiled-branch13
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc)) ; val => entry1
  (goto (reg val))

  primitive-branch12 ; never called
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call14 ; val now contains result of (factorial (- n 1))
  (restore argl) ; # 4 ^ 2 - restore partial argument list for *
  (assign argl (op cons) (reg val) (reg argl)); argl => (1 2),(2 3),(6 4),(24 5)
  (restore proc) ; # 4 ^ 1 - restore *
  (restore continue) ; # 4 ^ 0
  ;; apply * and return its value
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch15))

  compiled-branch16 ; never called
  ;; note that a compound procedure here is called tail-recursively
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch15
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => 2, 6, 24, 120
  (goto (reg continue))

  after-call17 ; a-code ends
  after-if5
  after-lambda2 ; calls to factorial will end here
  ;; assign the procedure to the variable factorial
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))
