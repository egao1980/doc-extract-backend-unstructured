(defpackage #:doc-extract-backend-unstructured
  (:use #:cl #:doc-extract-protocol)
  (:export            #:+unstructured-partition-path+
           #:+unstructured-priority+
           #:+unstructured-formats+

           #:unstructured-backend
           #:make-unstructured-backend
           #:use-unstructured-backend
           #:unstructured-backend-endpoint
           #:unstructured-backend-strategy
           #:unstructured-backend-http-fn

           #:unstructured-http-request
           #:unstructured-http-request-p
           #:unstructured-http-request-method
           #:unstructured-http-request-url
           #:unstructured-http-request-strategy
           #:unstructured-http-request-filename
           #:unstructured-http-request-content-type
           #:unstructured-http-request-octets
           #:unstructured-http-request-fields

           #:unstructured-http-error
           #:unstructured-http-error-status
           #:unstructured-http-error-url
           #:unstructured-http-error-body

           #:partition-url
           #:decode-unstructured-json
           #:element-field
           #:elements->document
           #:elements->sections
           #:make-curl-http-fn))

(in-package #:doc-extract-backend-unstructured)
