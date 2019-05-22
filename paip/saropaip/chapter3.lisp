(defpackage :saropaip/chapter3
  (:use #:cl))

(in-package #:saropaip/chapter3)

;; Exercise 3.1
(let* ((x 6)
       (y (* x x)))
  (+ x y))
;; &#9635; **Exercise 3.1 [m]** Show a `lambda` expression that is equivalent to the above `let*` expression.
;; You may need more than one `lambda.`

((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

;; Exercise 3.2

;; &#9635; **Exercise 3.2 [s]** The function cons can be seen as a special case of one of the other functions listed previously.
;; Which one?
;; (cons x y) <=> (append '(x) y)

;; Exercise 3.3
;; &#9635; **Exercise 3.3 [m]** Write a function that will print an expression in dotted pair notation.
;; Use the built-in function `princ` to print each component of the expression.

(defun expression-to-dotted-pairs (e)
  (cond ((consp e)
         (let ((sl (expression-to-dotted-pairs (first e)))
               (sr (expression-to-dotted-pairs (rest e))))
           (concatenate 'string "(" sl " . " sr ")"))
         )
        (t (princ-to-string e))))

(defun print-dotted-pairs (e)
  (princ (expression-to-dotted-pairs e)))
           
;; &#9635; **Exercise 3.4 [m]** Write a function that, like the regular `print` function, will print an expression in dotted pair notation when necessary but will use normal list notation when possible.

;; Noticed that listp doesn't check if a cons cell is nil terminated so is more like consp except true for nil
;; SAROPAIP/CHAPTER3> (listp (cons 'x 'y))
;; T

(defun expression-to-print-ready-string (e)
  (labels
      ((list-to-str (e &optional (prefix "("))
         (if (consp e)
             (let ((el (first e))
                   (er (rest e)))
               (cond  ((consp er)
                       (concatenate 'string prefix (list-to-str el) (list-to-str er " ")))
                      ((null er)
                       (concatenate 'string prefix (list-to-str el) ")"))
                      (t
                       (concatenate 'string prefix (list-to-str el) " . " (list-to-str er) ")"))
                  ))
             (princ-to-string e))))
    (list-to-str e)
    )
 )

;; SAROPAIP/CHAPTER3> (expression-to-print-ready-string '(0 ((a . b) . (c . d)) 2 (3 . (4 . (8 . 9)))))
;; "(0 ((A . B) C . D) 2 (3 4 8 . 9))"

