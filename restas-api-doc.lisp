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
        (lambda (a b)
          (cond ((string< (first a) (first b))
                 a)
                ((string> (first a) (first b))
                 b)
                (t
                 (if (string>= (second a) (second b))
                   a
                   b))))))

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
           (format stream "#### Module ~a~%~%~a~%~%"
                   (string-downcase (package-name package))
                   documentation)
           (dolist (route routes)
             (destructuring-bind (template method content-type documentation)
                 route
               (declare (ignore content-type documentation))
               (let ((doc-uri (format nil "~a/~a" method (cl-base64:string-to-base64-string template :uri t))))
                 (format stream "* ~a [/~a](~a)~%" method template doc-uri))))))))))

#+nil
(defmacro with-page ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

#+nil
(defun render-index/spinneret ()
  (let ((doc (get-doc-collection)))
    (with-page (:title "API Index")
      (dolist (module doc)
        (destructuring-bind (package documentation routes)
            module
          (:h4 "Module" (string-downcase (package-name package)))
          (:p (if documentation (:raw (markdown documentation) "")))
          (:ul 
           (dolist (route routes)
             (destructuring-bind (template method content-type documentation)
                 route
               (declare (ignore content-type documentation))
               (:li (:span method " ")
                    (:a :href (format nil "~a/~a" method (cl-base64:string-to-base64-string template :uri t))
                        (concatenate 'string "/" template)))))))))))

(defvar *render-index-fun* 'render-index/markdown)

(define-route api-doc/top ("/" :method :get :content-type "text/html")
  "Index page for the REST API documentation."
  (funcall *render-index-fun*))

(defun get-route-info (doc method template)
  (loop for module-info in doc
        for module-routes = (third module-info)
        for route-info = (find-if (lambda (item)
                                    (destructuring-bind (r-template r-method r-content-type r-documentation)
                                        item
                                      (declare (ignore r-content-type r-documentation))
                                      (when
                                          (and (string= method r-method)
                                               (string= template r-template))
                                        item)))
                                  module-routes)
        when route-info
        return route-info))

(defun render-route/markdown (method template)
  (let ((doc (get-doc-collection)))
    (let ((route-info (get-route-info doc method template)))
      (destructuring-bind (template method content-type documentation)
          route-info
        (markdown
         (format nil "#### Route:~%~%`~a`~%~%##### Method:~%~%~a~%~%##### Returns:~%~%~a~%~%##### Description:~%~%~a"
                 template method content-type (or documentation "")))))))

#+nil
(defun render-route/spinneret (method template)
  (let ((doc (get-doc-collection)))
    (let ((route-info (get-route-info doc method template)))
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

(defvar *render-route-fun* 'render-route/markdown)

(define-route api-doc/* ("/:(method)/:(route)")
  (:sift-variables (route (lambda (route) (cl-base64:base64-string-to-string route :uri t))))
  "Handler for documentation for specific REST API endpoints."
  (funcall *render-route-fun* method route))

#||
(setf *render-index-fun* 'render-index/markdown
      *render-route-fun* 'render-route/markdown)
(setf *render-index-fun* 'render-index/spinneret
      *render-route-fun* 'render-route/spinneret)

||#
