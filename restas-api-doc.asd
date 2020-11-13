;;;; restas-api-doc.asd

(asdf:defsystem #:restas-api-doc
  :description "Describe restas-api-doc here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (restas 3bmd cl-json #+nil spinneret)
  :components ((:file "package")
               (:file "restas-api-doc")))
