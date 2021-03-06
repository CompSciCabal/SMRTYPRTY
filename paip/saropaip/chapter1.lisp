;;;; chapter1.lisp

(defpackage :saropaip/chapter1
  (:use #:cl))

(in-package #:saropaip/chapter1)

(setf names '((John Q Public) (Malcolm X)
              (Admiral Grace Murray Hopper) (Spot) 
              (Aristotle) (A A Milne) (Z Z Top)
              (Sir Larry Olivier) (Miss Scarlet)
              (Madam Penelope Quixote Sr)))


(defparameter *titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name.")

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defparameter *post-nominal-letters*
  '(MD Jr Sr)
  "A list of post-nominal letters that can appear at the end of a name.")

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (if (member (first (last name)) *post-nominal-letters*)
      (last-name (reverse (rest (reverse name))))
      (first (last name))))

(defun power (x n)
  (if (= n 0)
      1
      (* x  (power x (- n 1)) )))

(defun last-name-greg (name)
   (let ((rname (reverse name)))
     (if (not (member (car rname) *titles*))
         (car rname)
         (last-name-greg (cdr name)))))

;; Exercise 1.4
;; Copied From Answers
(defun count-anywhere (item tree)
  "Count number of times item appears in tree."
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (first tree))
              (count-anywhere item (rest tree))))))

(assert (eql (count-anywhere 'a '(a (a b) b a)) 3))

