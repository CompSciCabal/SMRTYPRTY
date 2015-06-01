;;;; sicp-constraints.asd

(asdf:defsystem #:sicp-constraints
  :serial t
  :description "Describe sicp-constraints here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fact-base)
  :components ((:file "package")
	       (:file "compiler")
               (:file "sicp-constraints")))

