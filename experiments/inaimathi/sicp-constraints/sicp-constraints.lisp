;;;; sicp-constraints.lisp
(in-package #:sicp-constraints)

(defclass connector ()
  ((value :accessor value :initform nil)
   (informant :accessor informant :initform nil)
   (constraints :accessor constraints :initform nil)))

(defclass constraint () ())
(defmethod inform-about-value ((c constraint))
  (error "Tried to inform ~s about changing" c))
(defmethod inform-about-no-value ((c constraint))
  (error "Tried to inform ~s about forgetting" c))

;;;;;;;;;; General utility
(defun for-each-except (exception fn list)
  (loop for elem in list 
     unless (eq exception elem)
     do (funcall fn elem)))

;;;;;;;;;; Constraints
(defclass adder (constraint)
  ((a1 :accessor a1 :initarg :a1)
   (a2 :accessor a2 :initarg :a2)
   (sum :accessor sum :initarg :sum)))
(defmethod inform-about-no-value ((self adder))
  (with-slots (a1 a2 sum) self
    (forget! a1 self)
    (forget! a2 self)
    (forget! sum self)))
(defmethod inform-about-value ((self adder))
  (with-slots (a1 a2 sum) self
    (cond ((and (has-value? a1) (has-value? a2))
	   (set! sum (+ (value a1) (value a2)) self))
	  ((and (has-value? a1) (has-value? sum))
	   (set! a2 (- (value sum) (value a1)) self))
	  ((and (has-value? a2) (has-value? sum))
	   (set! a1 (- (value sum) (value a2)) self)))))
(defmethod make-adder (&key a1 a2 sum)
  (let ((self (make-instance 'adder :a1 a1 :a2 a2 :sum sum)))
    (connect! a1 self)
    (connect! a2 self)
    (connect! sum self)
    self))

(defclass multiplier (constraint)
  ((m1 :accessor m1 :initarg :m1)
   (m2 :accessor m2 :initarg :m2)
   (product :accessor product :initarg :product)))
(defmethod inform-about-no-value ((self multiplier))
  (with-slots (m1 m2 product) self
    (forget! m1 self)
    (forget! m2 self)
    (forget! product self)))
(defmethod inform-about-value ((self multiplier))
  (with-slots (m1 m2 product) self
    (cond ((and (has-value? m1) (has-value? m2))
	   (set! product (* (value m1) (value m2)) self))
	  ((and (has-value? m1) (has-value? product))
	   (set! m2 (/ (value product) (value m1)) self))
	  ((and (has-value? m2) (has-value? product))
	   (set! m1 (/ (value product) (value m2)) self)))))
(defmethod make-multiplier (&key m1 m2 product)
  (let ((self (make-instance 'multiplier :m1 m1 :m2 m2 :product product)))
    (connect! m1 self)
    (connect! m2 self)
    (connect! product self)
    self))

(defclass constant (constraint) ())
(defmethod make-constant (value (conn connector))
  (let ((self (make-instance 'constant)))
    (connect! conn self)
    (set! conn value self)
    self))

(defclass probe (constraint) 
  ((name :reader name :initarg :name)
   (connector :reader connector :initarg :connector)))
(defmethod inform-about-value ((self probe))
  (format t "Probe: ~a = ~s~%" (name self) (value (connector self))))
(defmethod inform-about-no-value ((self probe))
  (format t "Probe: ~a = ~s~%" (name self) '?))
(defmethod make-probe (name (conn connector))
  (let ((self (make-instance 'probe :name name :connector conn)))
    (connect! conn self)
    self))

;;;;;;;;;; Connectors
(defun make-connector () (make-instance 'connector))
(defmethod has-value? ((conn connector))
  (not (not (informant conn))))

(defmethod set! ((conn connector) new-value setter)
  (with-slots (value informant constraints) conn
    (cond ((not (has-value? conn))
	   (setf value new-value
		 informant setter)
	   (for-each-except setter #'inform-about-value constraints))
	  ((not (equal value new-value))
	   (error "Contradiction! ~s =/= ~s" value new-value)))))

(defmethod forget! ((conn connector) retractor)
  (with-slots (informant constraints) conn
    (if (eq retractor informant)
	(progn (setf informant nil)
	       (for-each-except retractor #'inform-about-no-value constraints))
	'ignored)))

(defmethod connect! ((conn connector) (new constraint))
  (pushnew new (constraints conn))
  (when (has-value? conn)
    (inform-about-value new))
  'done)

;;;;;;;;;; Testing
;; (defvar *c* (make-connector))
;; (defvar *f* (make-connector))

;; (defun celsius-faranheit (c f)
;;   (let ((u (make-connector))
;; 	(v (make-connector))
;; 	(w (make-connector))
;; 	(x (make-connector))
;; 	(y (make-connector)))
;;     (make-multiplier :m1 c :m2 w :product u)
;;     (make-multiplier :m1 v :m2 x :product u)
;;     (make-adder :a1 v :a2 y :sum f)
;;     (make-constant 9 w)
;;     (make-constant 5 x)
;;     (make-constant 32 y)
;;     'ok))

;; (make-probe "Celsius temp" *c*)
;; (make-probe "Faranheit temp" *f*)
;; (celsius-faranheit *c* *f*)

