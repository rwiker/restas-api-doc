;;;; package.lisp

(restas:define-module #:restas-api-doc
  (:use #:cl #:restas)
  (:import-from #:restas #:pkgmodule-traits-routes #:find-vhost
                #:package #:modules)
  (:export #:*render-index-fun*
           #:*render-route-fun*
           #:*doc-collection*
           #:collect-api-doc))
