(in-package :sicp-constraints)

;; (compile-diagram #p"/home/inaimathi/.2dmacs/diagrams/92f85d041b7429ea99af929674afdb8ac8b7a1152f26ee421aaee113b2434033")

(defmethod compile-diagram (target (file pathname))
  (with-open-file (s file) 
    (compile-diagram target (read s))))

(defmethod compile-diagram (target (base list))
  (generate-code target (synthesize base)))

;;;;;;;;;; Synthesizing new facts
(defmethod synthesize ((base list))
  (label-line-cluster
   (label-connection-endpoints
    (label-top-inputs
     (label-arguments-and-name
      (label-labels
       (label-constants
	(label-line-connections
	 (label-constraints base)))))))))

(defmethod label-constraints ((base list))
  "Any rectangle is a constraint."
  (for-all (?id :rectangle nil) :in base :do (push (list ?id :sicp-constraint nil) base))
  base)

(defmethod label-constants ((base list))
  "A rounded rectangle with a perfectly overlapping text box is a constatn."
  (for-all (and (?id :rounded-rectangle nil)
		(?id :start ?start) (?id :end ?end)
		(?text :text nil)
		(?text :start ?start) (?text :end ?end)
		(?text :contents ?value))
	   :in base :do (progn (push (list ?text :value (parse-integer ?value :junk-allowed t)) base)
			       (push (list ?text :sicp-constant nil) base)))
  base)

(defun between? (a n b)
  (>= (max a b) n (min a b)))

(defun point-inside? (x y ax ay bx by)
  (and (between? ax x bx)
       (between? ay y by)))

(defmethod label-labels ((base list))
  "Any text box inside a constraint is a label pertaining to it."
  (for-all (and (?id :sicp-constraint nil)
		(?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
		(?text :text nil)
		(?text :start (?x ?y)) (?text :end (?x2 ?y2))
		(lisp (and (point-inside? ?x ?y ?cx ?cy ?cx2 ?cy2)
			   (point-inside? ?x2 ?y2 ?cx ?cy ?cx2 ?cy2))))
	   :in base :do (push (list ?id :constraint-of ?text) base))
  base)

(defun on-edge? (x y ax ay bx by)
  (or (= x ax) (= x bx)
      (= y ay) (= y by)))

(defmethod label-arguments-and-name ((base list))
  "Text boxes contained by constraints, and on their edges are arguments.
Text boxes contained by constraints but not on their edges are constraint names."
  (for-all (and (?id :constraint-of ?text)
		(?id :start (?cx ?cy)) (?id :end (?cx2 ?cy2))
		(?text :start (?tx ?ty)) (?text :end (?tx2 ?ty2))
		(lisp (or (on-edge? ?tx ?ty ?cx ?cy ?cx2 ?cy2)
			  (on-edge? ?tx2 ?ty2 ?cx ?cy ?cx2 ?cy2))))
	   :in base :do (push (list ?text :sicp-argument nil) base))
  (for-all (and (?id :constraint-of ?text)
		(not (?text :sicp-argument nil)))
	   :in base :do (push (list ?text :sicp-constraint-name nil) base))
  base)

(defmethod label-top-inputs ((base list))
  "Any text boxes that haven't yet been annotated as constraint-names, arguments or constants are top level inputs for the system."
  (for-all (and (?id :text nil)
		(not (?id :sicp-constraint-name nil)
		     (?id :sicp-argument nil)
		     (?id :sicp-constant nil)))
	   :in base :do (push (list ?id :sicp-top-connection nil) base))
  base)

(defmethod label-line-connections ((base list))
  "Some lines connect to other lines. Connecting lines will be a single connection rather than multiple connections."
  (for-all (and (?id :line-segment nil)
		(?id :start ?start) (?id :end ?end)
		(?id2 :line-segment nil)
		(lisp (not (equal ?id ?id2)))
		(or (?id2 :start ?end) (?id2 :end ?start)
		    (?id2 :start ?start) (?id2 :end ?end)))
	   :in base :do (push (list ?id :line-connects-to-line ?id2) base))
  base)

(defmethod label-connection-endpoints ((base list))
  "Some line-segments connect to arguments, constants or top-connections, and this needs to be stated explicitly."
  (for-all (and (?id :line-segment nil)
		(?id :start (?x ?y)) (?id :end (?x2 ?y2))
		(or (?id2 :sicp-argument nil)
		    (?id2 :sicp-constant nil)
		    (?id2 :sicp-top-connection nil))
		(?id2 :start (?ax ?ay)) (?id2 :end (?ax2 ?ay2))
		(lisp (or (and (point-inside? ?x ?y ?ax ?ay ?ax2 ?ay2) 
			       (on-edge? ?x ?y ?ax ?ay ?ax2 ?ay2))
			  (and (point-inside? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2) 
			       (on-edge? ?x2 ?y2 ?ax ?ay ?ax2 ?ay2)))))
	   :in base :collect (push (list ?id :connects-to ?id2) base))
  base)

(defun walk-segment-graph (base src &key (explored (list src)))
  (for-all `(and (or (,src :line-connects-to-line ?id)
		     (?id :line-connects-to-line ,src))
		 (lisp (not (member ?id (list ,@explored)))))
	   :in base
	   :do (unless (member ?id explored)
		 (push ?id explored)
		 (setf explored (walk-segment-graph base ?id :explored explored))))
  explored)

(defmethod label-line-cluster ((base list))
  (let ((res (make-hash-table :test 'equal)))
    (for-all (and (?id :line-segment nil)
		  (not (?cluster :contains ?id)))
	     :in base
	     :collect (let ((cluster (sort (walk-segment-graph base ?id) #'string< :key #'symbol-name)))
			(setf (gethash cluster res) t)))
    (loop for clst being the hash-keys of res
       do (let ((id (intern (symbol-name (gensym)))))
	    (push (list id :cluster nil) base)
	    (loop for elem in clst 
	       do (push (list id :contains elem) base))))
    base))

;;;;;;;;;; Generating code on that basis
(defun sanitize (str)
  (string-upcase
   (string-right-trim 
    (list #\newline #\return #\space)
    str)))

(defmethod generate-constants ((base list))
  (for-all (and (?id :sicp-constant nil)
		(?id :value ?const)
		(?line :connects-to ?id)
		(?cluster :contains ?line))
	   :in base :collect `(make-constant ,?const ,?cluster)))

(defmethod generate-internal-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
		   (?txt :sicp-argument nil)
		   (?txt :contents ?arg-name)
		   (?line :connects-to ?txt)
		   (?cluster :contains ?line)
		   (not (and (?cluster :contains ?line2)
			     (?line2 :connects-to ?top)
			     (?top :sicp-top-connection nil))))
	     :in base 
	     :do (progn (push ?cluster res)
			(push (intern (sanitize ?arg-name) :keyword) res)))
    res))

(defmethod generate-top-arguments ((base list) constraint)
  (let ((res))
    (for-all `(and (,constraint :constraint-of ?txt)
		   (?txt :sicp-argument nil)
		   (?txt :contents ?arg-name)
		   (?line :connects-to ?txt)
		   (?line :connects-to ?top)
		   (?top :sicp-top-connection nil)
		   (?top :contents ?top-name))
	     :in base :do (progn (push (intern (sanitize ?top-name)) res)
				 (push (intern (sanitize ?arg-name) :keyword) res)))
    res))

(defmethod generate-arguments ((base list) constraint)
  (append (generate-top-arguments base constraint)
	  (generate-internal-arguments base constraint)))

(defmethod generate-constraints ((base list))
  (for-all (and (?id :sicp-constraint nil)
		(?id :constraint-of ?txt)
		(?txt :sicp-constraint-name nil)
		(?txt :contents ?name))
	   :in base
	   :collect (case (intern (sanitize ?name) :keyword)
		      (:* `(make-multiplier ,@(generate-arguments base ?id)))
		      (:+ `(make-adder ,@(generate-arguments base ?id))))))

(defmethod generate-internal-connections ((base list))
  (for-all (and (?id :cluster nil)
		(not (and (?id :contains ?line)
			  (?line :connects-to ?elem)
			  (?elem :sicp-top-connection nil))))
	   :in base :collect `(,?id (make-connector))))

(defmethod generate-component-factory ((component-name symbol) (args list) (base list))
  `(defun ,component-name ,args
     (let ,(generate-internal-connections base)
       ,@(generate-constraints base)
       ,@(generate-constants base)
       'ok)))

(defmethod get-component-name ((base list))
  (intern (first (for-all (and (?id :diagram-name nil)
			       (?id :value ?name))
			  :in base :collect (sanitize ?name)))))

(defmethod get-top-level-names ((base list))
  (for-all (and (?id :sicp-top-connection nil)
		(?id :contents ?name))
	   :in base :collect (sanitize ?name)))

(defmethod generate-repl-app ((base list))
  (let* ((component-name (get-component-name base))
	 (top-level-names (get-top-level-names base))
	 (top-level-global-vars
	  (mapcar (lambda (v) (intern (format nil "*~a*" v))) 
		  top-level-names)))
    `(progn ,@(loop for v in top-level-global-vars 
		 for n in top-level-names
		 collect `(defvar ,v (make-connector))
		 collect `(make-probe ,n ,v))
	    ,(generate-component-factory 
	      component-name (mapcar #'intern top-level-names) base)
	    (,component-name ,@top-level-global-vars))))

(defmethod generate-web-handler ((base list))
  (let* ((component-name (get-component-name base))
	 (uri-sym (intern (format nil "CONSTRAINT/~a" component-name)))
	 (top-level-names (mapcar #'intern (get-top-level-names base))))
    
    `(define-closing-handler (,uri-sym) ,(loop for n in top-level-names collect `(,n :number))
       "Hello!")))

(defmethod generate-code ((target (eql :repl)) (base list))
  (generate-repl-app base))

(defmethod generate-code ((target (eql :web)) (base list))
  (generate-web-handler base))
