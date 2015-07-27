(defpackage :dsl-gen (:use :cl :optima))
(in-package :dsl-gen)

;;;;;;;;;; Power, as in MetaOcaml (pg 3)
;; Transliteration
(defmacro pow (n x)
  (assert (numberp n))
  (if (zerop n)
      '1
      `(* ,x (pow ,(- n 1) ,x))))

(pow 5 6)
(let ((n 37))
  (pow 7 n))

;; A more idiomatic power macro
(defmacro loopow (n x)
  (assert (numberp n))
  `(* ,@(loop repeat n collect x)))

(loopow 5 6)
(let ((n 37))
  (loopow 7 n))

;;;;;;;;;; QBF (starts on pg 4)
;; Unstaged
(defun env0 (sym)
  (error "Nope, can't find ~a" sym))

(defun extend (env name val)
  (lambda (sym)
    (if (eq sym name) val (funcall env sym))))

(defun qbf-eval (exp env)
  (match exp
    ('-true t)
    ('-false nil)
    ((list '-and a b)
     (and (qbf-eval a env) (qbf-eval b env)))
    ((list '-or a b)
     (or (qbf-eval a env) (qbf-eval b env)))
    ((list '-not a)
     (not (qbf-eval a env)))
    ((list '=> a b)
     (qbf-eval `(-or ,b (-and (-not ,b) (-not ,a))) env))
    ((list 'forall a b)
     (flet ((try-with (bool) (qbf-eval b (extend env a bool))))
       (and (try-with '-true) (try-with '-false))))
    ((list '-var x)
     (funcall env x))))

(qbf-eval '-true #'env0)
(qbf-eval '-false #'env0)
(qbf-eval '(-not -false) #'env0)

;; Staged
(defmacro qbf-evalm (exp env)
  (match exp
    ('-true 't)
    ('-false 'nil)
    ((list '-and a b)
     `(and (qbf-evalm ,a ,env) (qbf-evalm ,b ,env)))
    ((list '-or a b)
     `(or (qbf-evalm ,a ,env) (qbf-evalm ,b ,env)))
    ((list '-not a)
     `(not (qbf-evalm ,a ,env)))
    ((list '=> a b)
     `(qbf-evalm (-or ,b (-and (-not ,b) (-not ,a))) ,env))
    ((list 'forall a b)
     `(flet ((try-with (bool) (qbf-evalm ,b (extend env ,a bool))))
	(and (try-with '-true) (try-with '-false))))
    ((list '-var x)
     (funcall env x))))

(qbf-evalm -true #'env0)
(qbf-evalm -false #'env0)
(qbf-evalm (-not -false) #'env0)

;; Simply "staged"
(defmacro qbf-evalmi (exp env)
  (qbf-eval exp env)) 

(qbf-evalmi -true #'env0)
(qbf-evalmi -false #'env0)
(qbf-evalmi (-not -false) #'env0)
