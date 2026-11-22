(defsystem "doc-extract-backend-unstructured"
  :version "0.1.1"
  :description "Unstructured-API HTTP backend for doc-extract-protocol (http-protocol multipart; injectable http-fn)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("doc-extract-protocol" (:version "http-protocol" "0.4.1") "encoding-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "json")
               (:file "elements")
               (:file "backend"))
  :in-order-to ((test-op (test-op "doc-extract-backend-unstructured/tests"))))

(defsystem "doc-extract-backend-unstructured/tests"
  :depends-on ("doc-extract-backend-unstructured" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test")
               (:file "http-fn-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
