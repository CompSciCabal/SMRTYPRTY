(ql:quickload :esrap)

(defpackage :fun 
  (:use :cl :esrap))
(in-package :fun)

;;;;; Expressions
(defrule exp (or if-then-else fun let-in parenthesized call atom))
(defrule call (and exp "(" exp ")"))
(defrule parenthesized (and "(" exp ")"))
(defrule fun (and "fun(" identifier ")" exp))
(defrule if-then-else (and "if" exp "then" exp "else" exp))
(defrule let-in (and "let" (+ decl) "in" exp))

;;;;; Declarations
(defrule decl (or recursive compound parenthesized-declaration basic-declaration))
(defrule recursive (and "rec" decl))
(defrule compound (and decl "then" decl))
(defrule parenthesized-declaration (and "(" decl ")"))
(defrule basic-declaration (and identifier "=" exp))

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
