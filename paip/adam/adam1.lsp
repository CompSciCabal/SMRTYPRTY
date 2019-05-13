(defun demo (topic mycode)
    (format t "~%~a: ~a ==> ~a " topic mycode (eval mycode)))

;; exercise 1.1
(defparameter *titles* '("MD" "Jr" "PMP" "MP"))


(defun last-name (name)
    ;(defvar rname (reverse name)) 
    (let ((rname (reverse name)))
    (if (not (member (car rname) *titles*)) 
        (car rname)
        (last-name (cdr name))))
    
    )
;; exercise 1.2
(defun power (b n)
    "Returns b to the power n. 
    N can be negative or positive but N = 0 will always give the result 1"
    (cond   ((= n 0) 1)
            ((< n 0) (/ 1 (power b (abs n))))
            (t (* b (power b (1- n))))))
;; exercise 1.3
(defun count-atoms (exp)
    "Returns the total number of non-nil atoms in the expression"
    (cond 
        ((null exp) 0)
        ((atom exp) 1)
        (t (+   (count-atoms (first exp))
                (count-atoms (rest exp))))))


;; exercise 1.4

(defun count-anywhere (item tree)
    
    (defun count-anywhere-iter (item tree counter)
        (cond
            ((null tree) counter)
            
            ((listp (first tree)) 
                (count-anywhere-iter item (first tree) (count-anywhere-iter item (rest tree) counter)))
                
            ((eq item (first tree)) 
                    (count-anywhere-iter item (rest tree) (+ 1 counter)))
                    
            (t (count-anywhere-iter item (rest tree) counter))))

        (count-anywhere-iter item tree 0)
    )

;; exercise 1.5
(defun dot-product (l1 l2) 
    "exercise 1.5: dot-product returns total of 
    the product of each pair from list 1 and list 2"
    (cond 
        ((or (null l1) (null l2)) 'undefined)
        (T (apply #'+ (mapcar #'* l1 l2)))
    ))

;-- test --
(demo "PAIP Ex 1.1" '(last-name '(john doe MP)))
(demo "PAIP Ex 1.2 case 1" '(power 2 0))
(demo "PAIP Ex 1.2 case 2" '(power 2 5))
(demo "PAIP Ex 1.2 case 3" '(power 2 -5))
(demo "PAIP Ex 1.3" '(count-atoms '(a (b) c)))

(demo "PAIP Ex 1.4 case 1" '(count-anywhere 'a '(a a)))
(demo "PAIP Ex 1.4 case 2" '(count-anywhere 'a '(a (b) a)))
(demo "PAIP Ex 1.4 case 3" '(count-anywhere 'a '(a (b) a a (b a) a)))

(demo "PAIP Ex 1.5 case 1" '(dot-product (list 1 2) (list 5 6)))
(demo "PAIP Ex 1.5 case 2" '(dot-product (list 5 10) (list 6 11)))
