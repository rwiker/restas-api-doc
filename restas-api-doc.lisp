;;;; restas-api-doc.lisp

(in-package #:restas-api-doc)

(defvar *route-example-bindings*
  (make-hash-table :test 'eq)
  "Mapping from symbol denoting routes one or more example uris for the route.
Each example uri may be either a string or a lambda expression that takes no
parameters.")

(defun find-route (symbol)
  (let ((root-module
         (loop for module = *module* then parent
               for parent = (restas::module-parent module)
               while parent
               finally (return module))))
    (loop with hash-table = (slot-value root-module 'restas::routes)
          for route being the hash-value of hash-table
          when (eq (route-symbol route) symbol)
          return route)))

(defun render-example-uri (symbol)
  (multiple-value-bind (bindings found-p)
      (gethash symbol *route-example-bindings*)
    (and found-p
         (let* ((route (find-route symbol))
                (template (routes:route-template route)))
           (make-route-url template
                           (loop for (key val . nil) on bindings by 'cddr
                                 nconc (list key (if (and (symbolp val)
                                                          (fboundp val))
                                                   (funcall val)
                                                   val))))))))

(restas:define-declaration :example-uri :route (declarations target traits)
  (assert (and (evenp (length declarations))))
  (assert (every (lambda (k) (and (symbolp k) (keywordp k)))
                 (loop for k in declarations by 'cddr
                       collect k)))
  (setf (gethash target *route-example-bindings*)
        declarations))

(defmethod as-url-component ((template string))
  template)

(defmethod as-url-component ((template symbol))
  (format nil "{~a}" (cl-json:lisp-to-camel-case (symbol-name template))))

(defmethod as-url-component ((template routes:concat-template))
  (apply 'concatenate 'string (mapcar 'as-url-component (routes:template-data template))))

(defmethod as-url-component ((template routes:variable-template))
  (as-url-component (routes:template-data template)))

(defmethod as-url-component ((template routes:wildcard-template))
  "*")

(defmethod as-url-component ((template list))
  (mapcar 'as-url-component template))

(defun as-url (what)
  (let ((uri (make-instance 'puri:uri)))
    (setf (puri:uri-parsed-path uri)
          (cons :absolute (as-url-component what)))
    (puri:render-uri uri nil)))

(defun route-compare (a b)
  (destructuring-bind (template-a method-a &rest rest-a)
      a
    (declare (ignore rest-a))
    (destructuring-bind (template-b method-b &rest rest-b)
        b
      (declare (ignore rest-b))
      (let ((url/a (as-url template-a))
            (url/b (as-url template-b)))
        (cond ((string-lessp url/a url/b)
               t)
              ((and (string-equal url/a url/b)
                    (string-lessp (string method-a) (string method-b)))
               t)
              (t
               nil))))))

(defun sort-route-data (route-data)
  (sort route-data 'route-compare))

(defmethod routes:proxy-route-target ((route t))
  route)

(defun collect-route-info (routes)
  (loop for route in routes
        for real-route = (routes:proxy-route-target route)
        for symbol = (route-symbol real-route)
        for documentation = (documentation symbol 'function)
        for method = (restas::route-required-method real-route)
        for content-type = (cadr (member :content-type (restas::route-headers real-route)))
        for template = (routes:route-template real-route)
        collect (list template method content-type documentation symbol)))

(defun collect-api-doc/vhost (host port)
  (let ((vhost (find-vhost (cons host port))))
    (when vhost
      (loop for module in (slot-value vhost 'modules)
            nconc (collect-api-doc/module module)))))

(defun collect-api-doc/module (module)
  (let* ((package (slot-value module 'package))
         (package-documentation (documentation package t))
         (routes (slot-value module 'restas::routes))
         (filtered-routes (loop for route being the hash-value of routes
                                when (or t (eq (restas::route-module route) module))
                                collect route))
         (route-info (collect-route-info filtered-routes)))
    (list (list package package-documentation route-info))))

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
           (dolist (route (sort-route-data routes))
             (destructuring-bind (template method content-type documentation symbol)
                 route
               (declare (ignore content-type documentation symbol))
               (let ((doc-uri (format nil "~a/~a" method (cl-base64:string-to-base64-string (as-url template) :uri t))))
                 (format stream "* ~a [~a](~a)~%" method (hunchentoot:url-decode (as-url template)) doc-uri))))))))))

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
           (dolist (route (sort-route-data routes))
             (destructuring-bind (template method content-type documentation symbol)
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
                                    (destructuring-bind (r-template r-method r-content-type r-documentation r-symbol)
                                        item
                                      (declare (ignore r-content-type r-documentation r-symbol))
                                      (when
                                          (and (string= method r-method)
                                               (string= template (as-url r-template)))
                                        item)))
                                  module-routes)
        when route-info
        return route-info))

(defun render-route/markdown (method template)
  (let ((doc (get-doc-collection)))
    (let ((route-info (get-route-info doc method template)))
      (destructuring-bind (template method content-type documentation symbol)
          route-info
        (let ((example-uri (render-example-uri symbol)))
          (concatenate
           'string
           (markdown
            (format nil "#### Route:~%~%`~a`~%~%##### Method:~%~%~a~%~%##### Returns:~%~%~a~%~%##### Description:~%~%~a"
                    (hunchentoot:url-decode (as-url template)) method content-type (or documentation "")))
           (when example-uri
             (if (eq method :get)
                 (format nil "~%~%Try it: <a href=\"~a\">~:*~a</a>" example-uri)
                 (format nil "~%~%Example: <b>~a</b>" example-uri)))))))))

#+nil
(defun render-route/spinneret (method template)
  (let ((doc (get-doc-collection)))
    (let ((route-info (get-route-info doc method template)))
      (destructuring-bind (template method content-type documentation)
          route-info
        (with-page (:title (format nil "Route: ~a" (as-url template)))
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
