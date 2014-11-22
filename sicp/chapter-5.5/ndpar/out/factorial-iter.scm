((env)
 (val)
 ;; Assuming n = 5
 ;; ---------------------------
 ;; Total stack pushes: # 32
 ;; save argl: 5
 ;; save continue: 11
 ;; save env: 11
 ;; save proc: 5
 ;; restore argl: 5
 ;; restore continue: 11
 ;; restore env: 11
 ;; restore proc: 5
 ;; ---------------------------
 ;; Max stack depth: ^ 3
 ;; At the end of each iteration (compiled-branch18),
 ;; stack depth is 0

 ((assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))

  entry1 ; factorial - entry
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; factorial - body
  (assign val (op make-compiled-procedure) (label entry3) (reg env))
  (goto (label after-lambda4))

  entry3 ; iter - entry
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (prod count)) (reg argl) (reg env)) ; (prod count) => (1 1),(1 2),(2 3),(6 4),(24 5),(120 6)
  ;; iter - body
  (save continue) ; # 6 ^ 1
  (save env) ; # 6 ^ 2
  ;; (< n count) - body
  (assign proc (op lookup-variable-value) (const <) (reg env)) ; proc => <
  (assign val (op lookup-variable-value) (const count) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl)) ; argl => (n count)
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch8))

  compiled-branch9 ; never called
  (assign continue (label after-call10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => (< n count): (< 5 1),(< 5 2),(< 5 3),(< 5 4),(< 5 5),(< 5 6)

  after-call10
  (restore env) ; # 6 ^ 1
  (restore continue) ; # 6 ^ 0
  (if ((op false?) (reg val)) (label false-branch6))

  true-branch5 ; return prod
  (assign val (op lookup-variable-value) (const prod) (reg env)) ; val => prod: 120
  (goto (reg continue))

  false-branch6
  (assign proc (op lookup-variable-value) (const iter) (reg env)) ; proc => iter
  (save continue) ; # 5 ^ 1
  (save proc) ; # 5 ^ 2
  (save env) ; # 5 ^ 3
  ;; (+ count 1) - body
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const count) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch14))

  compiled-branch15 ; never called
  (assign continue (label after-call16))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => (+ count 1): 2, 3, 4, 5, 6

  after-call16
  (assign argl (op list) (reg val)) ; argl => (2),(3),(4),(5),(6)
  (restore env) ; # 5 ^ 2
  (save argl) ; # 5 ^ 3
  ;; (* count prod) - body
  (assign proc (op lookup-variable-value) (const *) (reg env)) ; proc => *
  (assign val (op lookup-variable-value) (const prod) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const count) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch11))

  compiled-branch12 ; never called
  (assign continue (label after-call13))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ; val => (* count prod): 1, 2, 6, 24, 120

  after-call13
  (restore argl) ; # 5 ^ 2
  (assign argl (op cons) (reg val) (reg argl)) ; argl => new (prod count): (1 2),(2 3),(6 4),(24 5),(120 6)
  (restore proc) ; # 5 ^ 1: iter
  (restore continue) ; # 5 ^ 0
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch17))

  compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc)) ; val => entry3
  (goto (reg val))

  primitive-branch17 ; never called
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call19
  after-if7
  after-lambda4
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  ;; call iter proc within factorial procedure body
  (assign proc (op lookup-variable-value) (const iter) (reg env)) ; proc => iter
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl)) ; argl => (1 1)
  (if ((op primitive-procedure?) (reg proc)) (label primitive-branch20))

  compiled-branch21
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val)) ; val => entry3

  primitive-branch20
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call22
  after-lambda2
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))
