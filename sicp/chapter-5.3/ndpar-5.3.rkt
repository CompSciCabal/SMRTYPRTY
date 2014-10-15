#lang racket

;; -------------------------------------------------------
;; Memory as Vectors
;; -------------------------------------------------------

;; Exercise 5.20, p.539
;; Assuming list procedure is defined as

(define (list . x)
  (if (empty? x)
      null
      (cons (car x)
            (apply list (cdr x)))))

(define x (cons 1 2))
(define y (list x x))

;; Registers:
;;
;;           0 1  2  3  4
;;          ┌─┬──┬──┬──┬──┐
;; the-cars │ │n1│p1│p1│  
;;          ├─┼──┼──┼──┼──┤
;; the-cdrs │ │n2│e0│p2│  
;;          └─┴──┴──┴──┴──┘
;;    free  p4
;;       x  p1
;;       y  p3

;; -------------------------------------------------------
;; Run the following exercises with
;; the simulator from ndpar-5.2.scm
;; -------------------------------------------------------

;; Exercise 5.21 p.539
;; count-leaves machines

;; Recursive
;; Analogous to Fibonacci machine
(define count-leaves-1-machine
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list 'not not)
         (list '+ +))
   '((assign continue (label done))

     loop
     (if ((op null?) (reg t)) (label zero))
     (assign z (op pair?) (reg t))
     (if ((op not) (reg z)) (label one))
     (save continue)
     (assign continue (label after-car))
     (save t)
     (assign t (op car) (reg t))
     (goto (label loop))

     after-car
     (restore t)
     (assign t (op cdr) (reg t))
     (assign continue (label after-cdr))
     (save val)
     (goto (label loop))

     after-cdr
     (assign t (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg t) (reg val))
     (goto (reg continue))

     one
     (assign val (const 1))
     (goto (reg continue))

     zero
     (assign val (const 0))
     (goto (reg continue))

     done)))

;(set-register-contents! count-leaves-1-machine 't null)
;(set-register-contents! count-leaves-1-machine 't 5)
;(set-register-contents! count-leaves-1-machine 't (cons 1 2))
;(set-register-contents! count-leaves-1-machine 't (cons (cons 1 2) (cons 3 4)))
(set-register-contents! count-leaves-1-machine 't (cons (cons (cons 1 2) 3) (cons 4 5)))
(start count-leaves-1-machine)
(get-register-contents count-leaves-1-machine 'val)
(print-statistics count-leaves-1-machine)
;=> ((instructions-executed . 98) (total-stack-pushes . 12) (maximum-stack-depth . 6))

;; Recursive with explicit counter
(define count-leaves-2-machine
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'car car)
         (list 'cdr cdr)
         (list 'not not)
         (list '+ +))
   '((assign n (const 0))
     (assign continue (label done))

     iter
     (if ((op null?) (reg t)) (label empty))
     (assign z (op pair?) (reg t))
     (if ((op not) (reg z)) (label one))
     (save continue)
     (assign continue (label after-car))
     (save t)
     (assign t (op car) (reg t))
     (goto (label iter))

     after-car
     (restore t)
     (restore continue)
     (assign t (op cdr) (reg t))
     (assign n (reg val))
     (goto (label iter))

     one
     (assign val (op +) (reg n) (const 1))
     (goto (reg continue))

     empty
     (assign val (reg n))
     (goto (reg continue))

     done)))

;(set-register-contents! count-leaves-2-machine 't null)
;(set-register-contents! count-leaves-2-machine 't 5)
;(set-register-contents! count-leaves-2-machine 't (cons 1 2))
;(set-register-contents! count-leaves-2-machine 't (cons (cons 1 2) (cons 3 4)))
(set-register-contents! count-leaves-2-machine 't (cons (cons (cons 1 2) 3) (cons 4 5)))
(start count-leaves-2-machine)
(get-register-contents count-leaves-2-machine 'val)
(print-statistics count-leaves-2-machine)
;=> ((instructions-executed . 79) (total-stack-pushes . 8) (maximum-stack-depth . 6))


;; Exercise 5.22, p.539

;; Append lists
(define append-machine
  (make-machine
   (list (list 'null? null?)
         (list 'cons cons)
         (list 'car car)
         (list 'cdr cdr))
   '((assign continue (label done))

     loop
     (if ((op null?) (reg x)) (label base))
     (save continue)
     (assign continue (label after-cdr))
     (save x)
     (assign x (op cdr) (reg x))
     (goto (label loop))

     after-cdr
     (restore x)
     (restore continue)
     (assign z (op car) (reg x))
     (assign val (op cons) (reg z) (reg val))
     (goto (reg continue))

     base
     (assign val (reg y))
     (goto (reg continue))

     done)))

(set-register-contents! append-machine 'x (list 1 2 3))
(set-register-contents! append-machine 'y (list 4 5 6))
(start append-machine)
(get-register-contents append-machine 'val)
(print-statistics append-machine)

;; Splice lists
(define append!-machine
  (make-machine
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'set-cdr! set-cdr!))
   '((assign u (reg x))

     iter
     (assign v (op cdr) (reg u))
     (if ((op null?) (reg v)) (label last-pair))
     (assign u (reg v))
     (goto (label iter))

     last-pair
     (perform (op set-cdr!) (reg u) (reg y)))))

(set-register-contents! append!-machine 'x (list 1 2 3))
(set-register-contents! append!-machine 'y (list 4 5 6))
(start append!-machine)
(get-register-contents append!-machine 'x)
(print-statistics append!-machine)
