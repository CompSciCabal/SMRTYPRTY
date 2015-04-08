(ql:quickload :esrap)

(defpackage :fun 
  (:use :cl :esrap))
(in-package :fun)

(defrule exp (or (and "if" exp "then" exp "else" exp)
		 (and "fun(" identifier ")" exp)
		 (and "let" (+ decl) "in" exp)
		 (and "(" exp ")")
		 (and exp "(" exp ")")
		 atom))

(defrule decl (or (and "rec" decl)
		  (and "(" decl ")")
		  (and identifier "=" exp)
		  (and decl "then" decl)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string) t))

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:constant nil))

(defun numericp (char)
  (>= 57 (char-code char) 48))

(defrule alphanumeric (alphanumericp character))

(defrule atom (and (* whitespace) (+ alphanumeric) (* whitespace))
  (:lambda (list)
    (let ((txt (text list)))
      (if (every #'numericp txt)
	  (parse-integer txt)
	  (intern (string-upcase txt))))))

(defrule identifier (and (* whitespace) (not-integer (+ alphanumeric)) (* whitespace))
  (:lambda (list) (intern (string-upcase (text list)))))
