(in-package #:doc-extract-backend-unstructured)

(define-condition unstructured-http-error (doc-extract-error)
  ((status :initarg :status :reader unstructured-http-error-status
           :initform nil)
   (url :initarg :url :reader unstructured-http-error-url :initform nil)
   (body :initarg :body :reader unstructured-http-error-body :initform nil))
  (:report (lambda (c s)
             (format s "unstructured HTTP error~@[ ~a~]~@[ at ~a~]~@[: ~a~]"
                     (unstructured-http-error-status c)
                     (unstructured-http-error-url c)
                     (or (doc-extract-error-message c)
                         (unstructured-http-error-body c))))))
