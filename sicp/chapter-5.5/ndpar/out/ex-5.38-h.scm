(assign val (op make-compiled-procedure) (label entry1) (reg env))
(goto (label after-lambda2))

entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (a b c d)) (reg argl) (reg env))

(assign arg1 (op lookup-variable-value) (const a) (reg env))
(assign arg2 (op lookup-variable-value) (const b) (reg env))
(assign arg1 (op +) (reg arg1) (reg arg2))
(assign arg2 (op lookup-variable-value) (const c) (reg env))
(assign arg1 (op +) (reg arg1) (reg arg2))
(assign arg2 (op lookup-variable-value) (const d) (reg env))
(assign val (op +) (reg arg1) (reg arg2))
(goto (reg continue))

after-lambda2
(perform (op define-variable!) (const h) (reg val) (reg env))
(assign val (const ok))
