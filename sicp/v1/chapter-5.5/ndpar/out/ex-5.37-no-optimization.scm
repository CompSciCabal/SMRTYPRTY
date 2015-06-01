(save continue)
(save env)
(save continue)
(assign val (op make-compiled-procedure) (label entry1) (reg env))
(restore continue)
(goto (label after-lambda2))

entry1
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
(save continue)
(save env)
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const =) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save env)
(save continue)
(assign val (const 1))
(restore continue)
(assign argl (op list) (reg val))
(restore env)
(save argl)
(save continue)
(assign val (op lookup-variable-value) (const n) (reg env))
(restore continue)
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(if ((op primitive-procedure?) (reg proc)) (label primitive-branch6))

compiled-branch7
(assign continue (label after-call8))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch6
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)

after-call8
(restore env)
(restore continue)
(if ((op false?) (reg val)) (label false-branch4))

true-branch3
(save continue)
(assign val (const 1))
(restore continue)
(goto (reg continue))

false-branch4
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const *) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save env)
(save continue)
(assign val (op lookup-variable-value) (const n) (reg env))
(restore continue)
(assign argl (op list) (reg val))
(restore env)
(save argl)
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const factorial) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save continue)
(save env)
(save continue)
(assign proc (op lookup-variable-value) (const -) (reg env))
(restore continue)
(restore env)
(restore continue)
(save continue)
(save proc)
(save env)
(save continue)
(assign val (const 1))
(restore continue)
(assign argl (op list) (reg val))
(restore env)
(save argl)
(save continue)
(assign val (op lookup-variable-value) (const n) (reg env))
(restore continue)
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(if ((op primitive-procedure?) (reg proc)) (label primitive-branch9))

compiled-branch10
(assign continue (label after-call11))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch9
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)

after-call11
(assign argl (op list) (reg val))
(restore proc)
(restore continue)
(if ((op primitive-procedure?) (reg proc)) (label primitive-branch12))

compiled-branch13
(assign continue (label after-call14))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch12
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)

after-call14
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
(if ((op primitive-procedure?) (reg proc)) (label primitive-branch15))

compiled-branch16
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch15
(save continue)
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(restore continue)
(goto (reg continue))

after-call17
after-if5
after-lambda2
(restore env)
(perform (op define-variable!) (const factorial) (reg val) (reg env))
(assign val (const ok))
(restore continue)
