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


;; &#9635; **Exercise 3.6 [s]** Given the following initialization for the lexical variable `a` and the special variable `*b*`, what will be the value of the `let` form?

(setf a 'global-a)
(defvar *b* 'global-b)

(defun fn () *b*)

(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))
;; Only the special vars defvar can be shadowed like this
;; I think it must be that the lexical vars don't end up in the symbol table, not sure
;; '(local-a local-b local-b global-a local-b)

;; From section 3.19
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(setf nums '(1 2 3 2 1))

(find-all 1 nums :test #'= :key #'abs)
;;  = (remove 1 nums :test (complement #'=) :test #'= :key #'abs)


;; &#9635; **Exercise 3.7 [s]** Why do you think the leftmost of two keys is the one that counts, rather than the rightmost?

;; because it's implemented as a find on a list?


;; &#9635; **Exercise 3.8 [m]** Some versions of Kyoto Common Lisp (KCL) have a bug wherein they use the rightmost value when more than one keyword/value pair is specified for the same keyword.
;; Change the definition of `find-all` so that it works in KCL.

(defun printing-kwargs (&rest keyword-args
                        &key (a 'default-a) b &allow-other-keys)
  (progn
    (print keyword-args)
    (print a)
    (print b)
    (print (remove ':a (remove a keyword-args)))
    (print (remove (list ':a a) keyword-args))
    (print (set-difference keyword-args (list ':a a)))
    (let ((start (position ':a keyword-args)))
      (if (not start)
          (print keyword-args)
          (print (append (subseq keyword-args 0 start) (subseq keyword-args (+ start 2))))))))

(defun remove-keyword-arg (keyword-arg keyword-args)
  (let ((start (position keyword-arg keyword-args)))
    (if (not start)
        keyword-args
        (append (subseq keyword-args 0 start) (subseq keyword-args (+ start 2))))))

(defun find-all-kyoto (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) (remove-keyword-arg ':test-not keyword-args))
      (apply #'remove item sequence
             :test (complement test) (remove-keyword-arg ':test keyword-args))))

;; &#9635; **Exercise 3.9 [m]** Write a version of `length` using the function `reduce`.
(defun length-reduce (list)
  (reduce #'+ list :key (lambda (x) (declare (ignore x)) 1)))


;; &#9635; **Exercise 3.10 [m]** Use a reference manual or `describe` to figure out what the functions `lcm` and `nreconc` do.
(describe #'lcm) 
;; least common multiple
(lcm 2 5 8) 
;; = 40
;; hah, don't do this
;; (setf x '(1 2 3))
;; (nconc x x)

;; nreconc seems a bit odd
;; It will return the first argument reversed with all the other values appeneded
;; but it in place modifies the first argument to be the last element only + all the other values appended
(setf x '(1 2 3))
(nreverse x)
;; returns '(3 2 1)
;; x = '(1)
(setf x '(1 2 3))
(nreconc x '(a b c))
;; returns '(3 2 1 a b c)
;; x = '(1 a b c)


;; &#9635; **Exercise 3.11** [m] There is a built-in Common Lisp function that, given a key, a value, and an association list, returns a new association list that is extended to include the key/value pair.
;; What is the name of this function?
(acons 'c 2 '((a . 0) (b . 1)))


;; &#9635; **Exercise 3.12 [m]** Write a single expression using format that will take a list of words and print them as a sentence, with the first word capitalized and a period after the last word.
;; You will have to consult a reference to learn new `format` directives.
(format t "~@(~{~s~^ ~}~)." '(hello bob smith))

