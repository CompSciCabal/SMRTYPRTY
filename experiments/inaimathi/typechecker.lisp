(ql:quickload :esrap)

(defpackage :fun 
  (:use :cl :esrap))
(in-package :fun)

;;;;; Expressions
(defrule expression (or if-then-else fun let-in parenthesized call atom))
(defrule call (and expression "(" expression ")")
  (:destructure (a _1 b _2) (declare (ignore _1 _2)) (list a b)))
(defrule parenthesized (and "(" expression ")")
  (:destructure (_1 a _2) (declare (ignore _1 _2)) a))
(defrule fun (and "fun(" identifier ")" expression)
  (:destructure (_1 a _2 b) (declare (ignore _1 _2)) `(lambda (,a) ,b)))
(defrule if-then-else (and "if" expression "then" expression "else" expression)
  (:destructure (_1 a _2 b _3 c) (declare (ignore _1 _2 _3)) (list 'if a b c)))
(defrule let-in (and "let" declaration "in" expression)
  (:destructure (_1 a _2 b) (declare (ignore _1 _2)) (list 'let a b)))

;;;;; Declarations
(defrule declaration (or recursive compound parenthesized-declaration basic-declaration))
(defrule recursive (and "rec" declaration))
(defrule compound (and declaration "then" declaration)
  (:destructure (a _ b) (declare (ignore _)) (list a b)))
(defrule parenthesized-declaration (and "(" declaration ")"))
(defrule basic-declaration (and identifier "=" expression)
  (:destructure (a _ b) (declare (ignore _)) (list a b)))

(defrule atom (and (* whitespace) (+ alphanumeric) (* whitespace))
  (:lambda (list)
    (let ((txt (text list)))
      (if (every #'numericp txt)
	  (parse-integer txt)
	  (intern (string-upcase txt))))))

(defrule identifier (and (* whitespace) (not-integer (+ alphanumeric)) (* whitespace))
  (:lambda (list) (intern (string-upcase (text list)))))

(defrule alphanumeric (alphanumericp character))

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string) t))

(defun numericp (char)
  (>= 57 (char-code char) 48))
