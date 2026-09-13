(in-package #:doc-extract-backend-unstructured/tests)

(defun fixture-path ()
  (asdf:system-relative-pathname
   "doc-extract-backend-unstructured"
   "tests/fixtures/elements.json"))

(defun fixture-json ()
  (uiop:read-file-string (fixture-path)))

(defun fixture-elements ()
  (decode-unstructured-json (fixture-json)))

(defun mock-http-fn (&optional (body (fixture-json)))
  (lambda (req)
    (list 200 body
          '(("content-type" . "application/json"))
          req)))

(defun %b (&rest args)
  (apply #'make-unstructured-backend
         (append (list :http-fn (mock-http-fn)) args)))

(deftest decode-fixture-json
  (let ((els (fixture-elements)))
    (ok (listp els))
    (ok (= 9 (length els)))
    (ok (equal "Title" (element-field (second els) "type")))
    (ok (equal "Quarterly Report" (element-field (second els) "text")))))

(deftest section-inference-from-elements
  (let* ((doc (elements->document (fixture-elements) :format :pdf
                                  :source "sample.pdf"))
         (blocks (doc-extract-protocol:extracted-document-blocks doc)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (equal "demiurge-doc/1"
               (doc-extract-protocol:extracted-document-schema-version doc)))
    (ok (equal "Quarterly Report"
               (doc-extract-protocol:document-metadata-title
                (doc-extract-protocol:extracted-document-metadata doc))))
    (ok (equal "sample.pdf"
               (doc-extract-protocol:document-metadata-filename
                (doc-extract-protocol:extracted-document-metadata doc))))
    (ok (equal "eng"
               (doc-extract-protocol:document-metadata-language
                (doc-extract-protocol:extracted-document-metadata doc))))
    (ok (= 3 (length blocks)))
    (ok (typep (first blocks) 'doc-extract-protocol:text-block))
    (ok (eq :furniture (doc-extract-protocol:block-layer (first blocks))))
    (ok (typep (second blocks) 'doc-extract-protocol:section))
    (ok (equal "Quarterly Report"
               (doc-extract-protocol:section-title (second blocks))))
    (ok (typep (third blocks) 'doc-extract-protocol:section))
    (ok (equal "Outlook"
               (doc-extract-protocol:section-title (third blocks))))
    (let ((kids (doc-extract-protocol:section-children (second blocks))))
      (ok (= 3 (length kids)))
      (ok (typep (first kids) 'doc-extract-protocol:text-block))
      (ok (search "Revenue" (doc-extract-protocol:text-block-text (first kids))))
      (ok (typep (second kids) 'doc-extract-protocol:list-block))
      (ok (= 2 (length (doc-extract-protocol:list-block-items (second kids)))))
      (ok (typep (third kids) 'doc-extract-protocol:table-block))
      (ok (= 2 (doc-extract-protocol:table-block-rows (third kids))))
      (ok (= 2 (doc-extract-protocol:table-block-cols (third kids)))))
    (let ((kids (doc-extract-protocol:section-children (third blocks))))
      (ok (= 2 (length kids)))
      (ok (search "cautious" (doc-extract-protocol:text-block-text (first kids))))
      (ok (typep (second kids) 'doc-extract-protocol:image-block)))))

(deftest extract-document-via-mock-http-fn
  (let* ((seen nil)
         (b (make-unstructured-backend
             :endpoint "http://unstructured.test:8000"
             :strategy "fast"
             :http-fn (lambda (req)
                        (push req seen)
                        (fixture-json))))
         (doc (doc-extract-protocol:extract-document b "Revenue grew." :format :txt)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (= 1 (length seen)))
    (let ((req (first seen)))
      (ok (unstructured-http-request-p req))
      (ok (eq :post (unstructured-http-request-method req)))
      (ok (search "/general/v0/general" (unstructured-http-request-url req)))
      (ok (equal "http://unstructured.test:8000/general/v0/general"
                 (unstructured-http-request-url req)))
      (ok (equal "fast" (unstructured-http-request-strategy req)))
      (ok (equal "upload.txt" (unstructured-http-request-filename req)))
      (ok (plusp (length (unstructured-http-request-octets req)))))
    (ok (search "Revenue"
                (doc-extract-protocol:document-text doc)))))

(deftest extract-sections-nests-under-titles
  (let ((secs (doc-extract-protocol:extract-sections (%b) "x" :format :pdf)))
    (ok (= 2 (length secs)))
    (ok (equal "Quarterly Report"
               (doc-extract-protocol:section-title (first secs))))
    (ok (search "Revenue" (doc-extract-protocol:section-text (first secs))))
    (ok (search "North America" (doc-extract-protocol:section-text (first secs))))
    (ok (equal "Outlook"
               (doc-extract-protocol:section-title (second secs))))
    (ok (search "cautious" (doc-extract-protocol:section-text (second secs))))))

(deftest extract-text-and-metadata
  (let ((b (%b)))
    (ok (search "Quarterly Report"
                (doc-extract-protocol:extract-text b "x" :format :pdf)))
    (let ((md (doc-extract-protocol:extract-metadata b "x" :format :pdf)))
      (ok (eq :pdf (getf md :format)))
      (ok (equal "Quarterly Report" (getf md :title)))
      (ok (equal "sample.pdf" (getf md :filename))))))

(deftest normalize-document-from-fixture-json
  (let ((doc (doc-extract-protocol:normalize-document
              (make-unstructured-backend)
              (fixture-json)
              :format :pdf)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (ok (search "cautious" (doc-extract-protocol:document-text doc)))))

(deftest partition-url-accepts-base-or-full
  (ok (equal "http://127.0.0.1:8000/general/v0/general"
             (partition-url "http://127.0.0.1:8000")))
  (ok (equal "http://127.0.0.1:8000/general/v0/general"
             (partition-url "http://127.0.0.1:8000/")))
  (ok (equal "http://host/general/v0/general"
             (partition-url "http://host/general/v0/general"))))

(deftest missing-http-fn-signals
  (let ((b (make-unstructured-backend)))
    (ok (signals (doc-extract-protocol:extract-document b "x" :format :txt)
                 'doc-extract-protocol:doc-extract-error))))

(deftest missing-http-fn-use-value
  (let ((b (make-unstructured-backend))
        (got nil))
    (handler-bind ((doc-extract-protocol:doc-extract-error
                    (lambda (c)
                      (use-value (elements->document (fixture-elements)) c))))
      (setf got (doc-extract-protocol:extract-document b "x" :format :txt)))
    (ok (doc-extract-protocol:extracted-document-p got))))

(deftest missing-http-fn-retry
  (let ((b (make-unstructured-backend))
        (attempts 0)
        (got nil))
    (handler-bind ((doc-extract-protocol:doc-extract-error
                    (lambda (c)
                      (incf attempts)
                      (setf (unstructured-backend-http-fn b) (mock-http-fn))
                      (invoke-restart
                       (find-if (lambda (r)
                                  (and (restart-name r)
                                       (string= (restart-name r) "RETRY")))
                                (compute-restarts c))))))
      (setf got (doc-extract-protocol:extract-document b "x" :format :txt)))
    (ok (doc-extract-protocol:extracted-document-p got))
    (ok (= 1 attempts))))

(deftest http-error-signals
  (let ((b (make-unstructured-backend
            :http-fn (lambda (req)
                       (declare (ignore req))
                       (list 502 "bad gateway")))))
    (ok (signals (doc-extract-protocol:extract-document b "x" :format :txt)
                 'unstructured-http-error))))

(deftest http-error-use-value
  (let ((b (make-unstructured-backend
            :http-fn (lambda (req)
                       (declare (ignore req))
                       (list 500 "{\"detail\":\"boom\"}"))))
        (got nil))
    (handler-bind ((unstructured-http-error
                    (lambda (c)
                      (use-value (fixture-json) c))))
      (setf got (doc-extract-protocol:extract-document b "x" :format :txt)))
    (ok (doc-extract-protocol:extracted-document-p got))))

(deftest register-extractor-priority
  (ok (= 20 +unstructured-priority+))
  (ok (typep (doc-extract-protocol:find-extractor :eml)
             'unstructured-backend))
  (ok (typep (doc-extract-protocol:find-extractor :epub)
             'unstructured-backend))
  (ok (typep (doc-extract-protocol:find-extractor :png)
             'unstructured-backend))
  (ok (typep (doc-extract-protocol:find-extractor :pdf)
             'unstructured-backend))
  (ok (typep (doc-extract-protocol:find-extractor :rtf)
             'unstructured-backend)))

(deftest live-unstructured-partition
  (let ((url (uiop:getenv "DOC_EXTRACT_UNSTRUCTURED_URL")))
    (if (and url (plusp (length url)))
        (let* ((b (make-unstructured-backend
                   :endpoint url
                   :strategy "fast"
                   :http-fn (make-curl-http-fn)))
               (doc (doc-extract-protocol:extract-document
                     b "Hello unstructured." :format :txt)))
          (ok (doc-extract-protocol:extracted-document-p doc))
          (ok (plusp (length (doc-extract-protocol:document-text doc)))))
        (skip "DOC_EXTRACT_UNSTRUCTURED_URL not set"))))
