(assign val (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))

entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
(assign proc (op lookup-variable-value) (const +) (reg env))

; right-to-left evaluation
(assign val (op lookup-variable-value) (const y) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const x) (reg env))
(assign argl (op cons) (reg val) (reg argl))

(if ((op primitive-procedure?) (reg proc)) (label primitive-branch3))

compiled-branch4
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))

after-call5
after-lambda2
(perform (op define-variable!) (const f) (reg val) (reg env))
(assign val (const ok))
