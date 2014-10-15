#lang racket

(require r5rs)

;; -------------------------------------------------------
;; Utility functions
;; -------------------------------------------------------

(define (print . x)
  (apply display x)
  (newline))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

;; -------------------------------------------------------
;; A Register-Machine Simulator
;; -------------------------------------------------------

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Registers

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace false))
    (define (set-contents val)
      (if trace (print (list "REG:" name ":" contents "->" val)))
      (set! contents val))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) set-contents)
            ((eq? message 'trace) (λ (val) (set! trace val)))
            (else (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (reg-get-contents register)
  (register 'get))

(define (reg-set-contents! register val)
  ((register 'set) val))

(define (reg-trace-on register)
  ((register 'trace) true))

(define (reg-trace-off register)
  ((register 'trace) false))

;; Stack

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (statistics)
      (list (cons 'total-stack-pushes number-pushes)
            (cons 'maximum-stack-depth max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'statistics) (statistics))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack val)
  ((stack 'push) val))

(define (stack-init stack)
  (stack 'initialize))

(define (stack-stats stack)
  (stack 'statistics))

;; Machine

(define (make-new-machine)
  (let* ((pc (make-register 'pc))
         (stack (make-stack))
         (instruction-sequence '())
         (trace false)
         (instructions-executed 0)
         (labels '())
         (the-ops (list (list 'initialize-stack
                              (λ () (stack-init stack)))
                        (list 'print-stack-statistics
                              (λ () (stack-stats stack)))))
         (register-table (list (list 'pc pc))))
    (define (set-breakpoint label n value)
      (define (iter insts k)
        (if (= k 1)
            (if (car insts)
                (((car insts) 'set-breakpoint) value)
                (error "Invalid place for breakpoint" label n))
            (iter (cdr insts) (- k 1))))
      (iter (cdr (assoc label labels)) n))
    (define (cancel-all-breakpoints)
      (for-each (λ (label)
                  (for-each (λ (inst) (inst 'unset-breakpoint))
                            (cdr label)))
                labels))
    (define (initialize)
      (set! instructions-executed 0))
    (define (print-stats)
      (print (cons (cons 'instructions-executed instructions-executed)
                   (stack-stats stack))))
    (define (allocate-register name)
      (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (let ((register (list name (make-register name))))
            (set! register-table (cons register register-table))
            register)))
    (define (lookup-register name)
      (let ((val (assoc name register-table)))
        (if val
            (cadr val)
            (cadr (allocate-register name)))))
    (define (execute stop-at-breakpoint)
      (let ((insts (reg-get-contents pc)))
        (if (null? insts)
            'done
            (let ((inst (car insts)))
              (if (and (inst 'breakpoint) stop-at-breakpoint)
                  (print (list "BREAKPOINT:" (inst 'label) (inst 'breakpoint)))
                  (begin
                    (if trace (print (list (inst 'label) ":" (inst 'text))))
                    ((inst 'procedure))
                    (set! instructions-executed (+ 1 instructions-executed))
                    (execute true)))))))
    (define (dispatch message)
      (cond ((eq? message 'start)
             (reg-set-contents! pc instruction-sequence)
             (execute true))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'trace-on) (set! trace true))
            ((eq? message 'trace-off) (set! trace false))
            ((eq? message 'trace-reg-on)
             (λ (reg-name) (reg-trace-on (lookup-register reg-name))))
            ((eq? message 'trace-reg-off)
             (λ (reg-name) ((lookup-register reg-name) 'trace-off)))
            ((eq? message 'install-instruction-sequence)
             (λ (seq) (set! instruction-sequence seq)))
            ((eq? message 'set-labels)
             (λ (ls) (set! labels ls)))
            ((eq? message 'set-breakpoint) set-breakpoint)
            ((eq? message 'cancel-breakpoint)
             (λ (label n) (set-breakpoint label n false)))
            ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
            ((eq? message 'proceed) (execute false))
            ((eq? message 'print-stats) (print-stats))
            ((eq? message 'allocate-register) allocate-register)
            ((eq? message 'get-register) lookup-register)
            ((eq? message 'install-operations)
             (λ (ops) (set! the-ops (append the-ops ops))))
            ((eq? message 'stack) stack)
            ((eq? message 'operations) the-ops)
            (else (error "Unknown request -- MACHINE" message))))
    dispatch))

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

;; Assembler

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (λ (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (λ (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "Duplicate label -- ASSEMBLE" next-inst)
                   (begin
                     (for-each (λ (inst)
                                 (if (inst 'no-label?)
                                     ((inst 'set-label)  next-inst)))
                               insts)
                     (receive insts
                              (cons (make-label-entry next-inst insts)
                                    labels))))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ((machine 'set-labels) labels)
    (for-each
     (λ (inst)
       ((inst 'set-procedure)
        (make-execution-procedure
         (inst 'text)
         labels machine pc stack ops)))
     insts)))

(define (make-instruction instruction-text)
  (let ((text instruction-text)
        (label '())
        (breakpoint false)
        (procedure '()))
    (define (dispatch message)
      (cond ((eq? message 'text) text)
            ((eq? message 'set-label)
             (λ (new-label) (set! label new-label)))
            ((eq? message 'label) label)
            ((eq? message 'no-label?) (empty? label))
            ((eq? message 'set-breakpoint)
             (λ (value) (set! breakpoint value)))
            ((eq? message 'unset-breakpoint) (set! breakpoint false))
            ((eq? message 'breakpoint) breakpoint)
            ((eq? message 'set-procedure)
             (λ (proc) (set! procedure proc)))
            ((eq? message 'procedure) procedure)
            (else
             (error "Unknown request -- INSTRUCTION" message))))
    dispatch))

;; Instructions

(define (make-execution-procedure inst labels machine pc stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'if)
         (make-test inst machine labels ops pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let* ((target (get-register machine (assign-reg-name inst)))
         (value-exp (assign-value-exp inst))
         (value-proc
          (if (operation-exp? value-exp)
              (make-operation-exp value-exp machine labels operations)
              (make-primitive-exp (car value-exp) machine labels))))
    (λ ()
      (reg-set-contents! target (value-proc))
      (advance-pc pc))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (reg-set-contents! pc (cdr (reg-get-contents pc))))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-test inst machine labels operations pc)
  (let ((condition (test-condition inst))
        (dest (branch-dest inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp condition machine labels operations)))
          (if (label-exp? dest)
              (let ((insts (lookup-label labels (label-exp-label dest))))
                (λ ()
                  (if (condition-proc)
                      (reg-set-contents! pc insts)
                      (advance-pc pc))))
              (error "Bad BRANCH instruction -- ASSEMBLE" inst)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cadr test-instruction))

(define (branch-dest branch-instruction)
  (caddr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (λ () (reg-set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (λ () (reg-set-contents! pc (reg-get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (λ ()
      (push stack (cons (stack-inst-reg-name inst) (reg-get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (λ ()
      (let ((head (pop stack)))
        (if (eq? reg-name (car head))
            (begin
              (reg-set-contents! reg (cdr head))
              (advance-pc pc))
            (error "Restoring wrong register" reg-name (car head)))))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc (make-operation-exp action machine labels operations)))
          (λ ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (const c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (const insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (λ () (reg-get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (λ (e)
                (make-operand-exp e machine labels))
              (operation-exp-operands exp))))
    (λ ()
      (apply op (map (λ (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (make-operand-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (const c)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (λ () (reg-get-contents r))))
        (else
         (error "Invalid operand expression -- ASSEMBLE" exp))))

;; -------------------------------------------------------
;; Client API
;; -------------------------------------------------------

(define (init-machine machine)
  (machine 'initialize))

(define (start machine)
  (machine 'start))

(define (set-register-contents! machine register-name val)
  (reg-set-contents! (get-register machine register-name) val))

(define (get-register-contents machine register-name)
  (reg-get-contents (get-register machine register-name)))

(define (trace-register machine reg-name)
  ((machine 'trace-reg-on) reg-name))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (proceed-machine machine)
  (machine 'proceed))

(define (print-statistics machine)
  (machine 'print-stats))

(define (init-stack machine)
  ((machine 'stack) 'initialize))
