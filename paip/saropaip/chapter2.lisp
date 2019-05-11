
;; chapter2

(defpackage :saropaip/chapter2
  (:use #:cl))

(in-package #:saropaip/chapter2)

;; Code from 1.7
(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

;; Code from 2.2
(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

;; Code from 2.3
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate (random-elt choices))))))

;; Exercise 2.1
(defun generate-2.1 (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate-2.1 phrase))
        (t (let ((choices (rewrites phrase)))
             (cond ((not choices) (list phrase))
                 (t (generate (random-elt choices))))))))

;; Exercise 2.2
(defun generate-2.2 (phrase)
  "Generate a random sentence or phrase"
  '())
