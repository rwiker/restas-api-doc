;;;; restas-api-doc.lisp

(in-package #:restas-api-doc)

(defun collect-route-info (routes-traits)
  (sort (loop for symbol being the hash-key of routes-traits
              using (hash-value traits)
              for documentation = (documentation symbol 'function)
              for method = (gethash :method traits)
              for content-type = (gethash :content-type traits)
              for template = (gethash :template traits)
              collect (list template method content-type documentation))
        #'string<= :key #'car))

(defun collect-api-doc/vhost (host port)
  (let ((vhost (find-vhost (cons host port))))
    (when vhost
      (loop for module in (slot-value vhost 'modules)
            for package = (slot-value module 'package)
            nconc (collect-api-doc/module package)))))

(defun collect-api-doc/module (package)
  (let* ((package (find-package package))
         (package-documentation (documentation package t))
         (routes-traits (pkgmodule-traits-routes package))
         (name (package-name package))
         (route-info (ignore-errors (collect-route-info routes-traits))))
    (list (list name package-documentation route-info))))

(defun markdown (string)
  (with-output-to-string (s)
    (3bmd:parse-string-and-print-to-stream string s)))

(defun encode-url-to-stream (stream url colon-p at-p)
  (declare (ignore colon-p at-p))
  (cl-base64:string-to-base64-stream url stream :uri t))

(defvar *doc-collection* nil)

(defun get-doc-collection ()
  (or *doc-collection*
      (collect-api-doc/vhost (hunchentoot:acceptor-address hunchentoot:*acceptor*)
                             (hunchentoot:acceptor-port hunchentoot:*acceptor*))))
  
(defun render-index/markdown ()
  (let ((doc (get-doc-collection)))
    (markdown 
     (with-output-to-string (stream)
       (dolist (module doc)
         (destructuring-bind (package documentation routes)
             module
           (format stream "#### ~a~%~%~a~%~%~{* [`/~a`](/api-doc/route=('~:*~/restas-api-doc::encode-url-to-stream/'))~^~%~}"
                   (string-downcase (package-name package))
                   documentation
                   (mapcar (lambda (route-info)
                             (first route-info))
                           routes))))))))

(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defun render-index/spinneret ()
  (let ((doc (get-doc-collection)))
    (with-page (:title "API Index")
      (dolist (module doc)
        (destructuring-bind (package documentation routes)
            module
          (:h4 (string-downcase (package-name package)))
          (:p (if documentation (:raw (markdown documentation) "")))
          (:ul 
           (dolist (route routes)
             (let ((url (first route)))
               (:li (:a :href (format nil "/api-doc/route=('~a')" (cl-base64:string-to-base64-string url :uri t))
                     (concatenate 'string "/" url)))))))))))

(defvar *render-index-fun* 'render-index/markdown)

(define-route api-doc/top ("/" :method :get :content-type "text/html")
  "Index page for the REST API documentation."
  (funcall *render-index-fun*))

(defun render-route/markdown (route)
  (let ((doc (get-doc-collection))
        (template #+nil (reduce (lambda (a b) (concatenate 'string a "/" b)) route)
                  #-nil route))
    (let ((route-info
           (loop for module-info in doc
                 for module-routes = (third module-info)
                 for route-info = (find template module-routes  :test #'string= :key #'first)
                 when route-info
                 return route-info)))
      (destructuring-bind (template method content-type documentation)
          route-info
        (markdown
         (format nil "#### Route:~%~%`~a`~%~%##### Method:~%~%~a~%~%##### Returns:~%~%~a~%~%##### Description:~%~%~a"
                 template method content-type (or documentation "")))))))

(defun render-route/spinneret (route)
  (let ((doc (get-doc-collection))
        (template #+nil (reduce (lambda (a b) (concatenate 'string a "/" b)) route)
                  #-nil route))
    (let ((route-info
           (loop for module-info in doc
                 for module-routes = (third module-info)
                 for route-info = (find template module-routes  :test #'string= :key #'first)
                 when route-info
                 return route-info)))
      (destructuring-bind (template method content-type documentation)
          route-info
        (with-page (:title (format nil "Route: ~a" template))
          (:h4 "Route:")
          (:code template)
          (:h5 "Method:")
          (:p method)
          (:h5 "Returns:")
          (:p content-type)
          (:h5 "Description:")
          (:p (if documentation (:raw (markdown documentation)) "")))))))

(defvar *render-route-fun* 'render-route/spinneret)

(define-route api-doc/* ("route=(':(route)')")
  (:sift-variables (route (lambda (route) (cl-base64:base64-string-to-string route :uri t))))
  "Handler for documentation for specific REST API endpoints."
  (if (null route)
    (funcall *render-index-fun*)
    (funcall *render-route-fun* route)))

