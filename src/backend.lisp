(in-package #:doc-extract-backend-unstructured)

(defparameter +unstructured-partition-path+ "/general/v0/general"
  "Unstructured-API partition endpoint (open-source unstructured-api container).")

(defparameter +unstructured-priority+ 20
  "Default register-extractor priority.

   Colocated HTML/office backends register at 10. A planned docling backend
   should register around 50. Corporate profile: docling (50) > unstructured
   (20) > pdfium. Personal profile should re-register this backend at 5 so
   colocated/pdfium win on shared formats.")

(defparameter +unstructured-formats+
  '(:pdf :docx :doc :xlsx :xls :pptx :ppt :html :htm
    :eml :msg :epub :rtf :odt :odp :ods
    :png :jpg :jpeg :tiff :tif :bmp :heic
    :md :txt :xml :csv :rst
    "application/pdf"
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    "application/msword"
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    "application/vnd.ms-excel"
    "application/vnd.openxmlformats-officedocument.presentationml.presentation"
    "application/vnd.ms-powerpoint"
    "text/html"
    "message/rfc822"
    "application/epub+zip"
    "application/rtf"
    "application/vnd.oasis.opendocument.text"
    "image/png"
    "image/jpeg"
    "image/tiff"
    "text/markdown"
    "text/plain")
  "Formats this backend self-registers for.")

(defparameter +default-endpoint+ "http://127.0.0.1:8000")

(defstruct unstructured-http-request
  (method :post)
  url
  strategy
  filename
  content-type
  octets
  fields)

(defclass unstructured-backend (doc-extract-backend)
  ((endpoint :initarg :endpoint :accessor unstructured-backend-endpoint
             :initform +default-endpoint+)
   (strategy :initarg :strategy :accessor unstructured-backend-strategy
             :initform "auto")
   (http-fn :initarg :http-fn :accessor unstructured-backend-http-fn
            :initform nil))
  (:documentation
   "HTTP client for unstructured-api. POST multipart to /general/v0/general.
    Inject HTTP-FN (request → JSON body) so unit tests never hit the network."))

(defun make-unstructured-backend (&key endpoint strategy http-fn)
  (make-instance 'unstructured-backend
                 :endpoint (or endpoint +default-endpoint+)
                 :strategy (or strategy "auto")
                 :http-fn http-fn))

(defun use-unstructured-backend (&rest args &key &allow-other-keys)
  (setf *doc-extract-backend* (apply #'make-unstructured-backend args)))

(defun partition-url (endpoint)
  (let ((base (string-right-trim '(#\/) (or endpoint +default-endpoint+))))
    (if (search +unstructured-partition-path+ base)
        base
        (concatenate 'string base +unstructured-partition-path+))))

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defun %utf8-octets (string)
  (if (and (every (lambda (c) (< (char-code c) 128)) string)
           (stringp string))
      (map '(simple-array (unsigned-byte 8) (*)) #'char-code string)
      (let ((out (make-array (length string)
                             :element-type '(unsigned-byte 8)
                             :adjustable t
                             :fill-pointer 0)))
        (loop for i from 0 below (length string)
              for cp = (char-code (char string i))
              do (cond
                   ((< cp #x80)
                    (vector-push-extend cp out))
                   ((< cp #x800)
                    (vector-push-extend (logior #xC0 (ash cp -6)) out)
                    (vector-push-extend (logior #x80 (logand cp #x3F)) out))
                   ((< cp #x10000)
                    (vector-push-extend (logior #xE0 (ash cp -12)) out)
                    (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand cp #x3F)) out))
                   (t
                    (vector-push-extend (logior #xF0 (ash cp -18)) out)
                    (vector-push-extend (logior #x80 (logand (ash cp -12) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand (ash cp -6) #x3F)) out)
                    (vector-push-extend (logior #x80 (logand cp #x3F)) out))))
        (coerce out '(simple-array (unsigned-byte 8) (*))))))

(defun %format-extension (format)
  (let ((fmt (canonicalize-format format)))
    (if fmt
        (string-downcase (symbol-name fmt))
        "bin")))

(defun %content-type-for (format)
  (let ((fmt (canonicalize-format format)))
    (case fmt
      ((:html :htm) "text/html")
      ((:txt :md :rst :csv) "text/plain")
      ((:xml) "application/xml")
      ((:pdf) "application/pdf")
      ((:png) "image/png")
      ((:jpg :jpeg) "image/jpeg")
      ((:eml) "message/rfc822")
      (t "application/octet-stream"))))

(defun %path-like-p (source)
  (and (or (pathnamep source)
           (and (stringp source)
                (not (find #\Newline source))
                (< (length source) 4096)
                (or (position #\/ source) (position #\. source))))
       (probe-file source)))

(defun %coerce-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8))
     (coerce source '(simple-array (unsigned-byte 8) (*))))
    (pathname
     (with-open-file (in source :element-type '(unsigned-byte 8)
                            :if-does-not-exist :error)
       (let ((buf (make-array (file-length in)
                              :element-type '(unsigned-byte 8))))
         (read-sequence buf in)
         buf)))
    (string
     (if (%path-like-p source)
         (%coerce-octets (pathname source))
         (%utf8-octets source)))))

(defun %infer-format (source format)
  (or format
      (when (pathnamep source)
        (canonicalize-format source))
      (when (and (stringp source) (%path-like-p source))
        (canonicalize-format source))
      :txt))

(defun %infer-filename (source format)
  (cond
    ((pathnamep source) (file-namestring source))
    ((and (stringp source) (%path-like-p source))
     (file-namestring (pathname source)))
    (t (format nil "upload.~a" (%format-extension format)))))

(defun unstructured-request->http-request (req)
  "REQ → an http-protocol:HTTP-REQUEST: a multipart/form-data POST with the
   strategy (and any extra FIELDS) as form fields and the document as the
   `files` part. http-protocol quotes every name and filename (RFC 7578), so
   neither the file name nor the strategy can add parts, headers, or read a
   local file.

   http-fn itself always receives UNSTRUCTURED-HTTP-REQUEST, so injected
   mocks see the plain request."
  (check-type req unstructured-http-request)
  (http-protocol:make-http-request
   :method :post
   :url (unstructured-http-request-url req)
   :form-data (append
               (list (cons "strategy" (unstructured-http-request-strategy req)))
               (unstructured-http-request-fields req))
   :files (list (cons "files"
                      (http-protocol:make-http-file
                       (unstructured-http-request-octets req)
                       :filename (unstructured-http-request-filename req)
                       :content-type (unstructured-http-request-content-type req)
                       :content-length (length (unstructured-http-request-octets req))
                       :field-name "files")))))

(defun %response-status (response)
  (cond
    ((integerp response) response)
    ((and (consp response) (integerp (car response))) (car response))
    ((typep response 'http-protocol:http-response)
     (or (http-protocol:response-status response) 200))
    (t 200)))

(defun %response-body (response)
  (cond
    ((and (consp response) (integerp (car response)))
     (cadr response))
    ((typep response 'http-protocol:http-response)
     (http-protocol:response-body response))
    (t response)))

(defun %body-string (body)
  "Response BODY as text: octets and streams are decoded as UTF-8."
  (cond
    ((stringp body) body)
    ((streamp body) (%body-string (http-protocol:slurp-octets body)))
    ((and (vectorp body) (not (stringp body))
          (plusp (length body))
          (typep (aref body 0) '(unsigned-byte 8)))
     (encoding-protocol:decode (coerce body '(vector (unsigned-byte 8)))
                               :encoding :utf-8))
    (t body)))

(defun build-partition-request (backend source &key format)
  (let ((fmt (%infer-format source format)))
    (make-unstructured-http-request
     :method :post
     :url (partition-url (unstructured-backend-endpoint backend))
     :strategy (unstructured-backend-strategy backend)
     :filename (%infer-filename source fmt)
     :content-type (%content-type-for fmt)
     :octets (%coerce-octets source)
     :fields nil)))

(defun %ensure-http-fn (backend)
  (or (unstructured-backend-http-fn backend)
      (restart-case
          (error 'doc-extract-error
                 :message "unstructured-backend has no http-fn — inject one or use make-http-protocol-http-fn")
        (retry ()
          :report "Retry after installing http-fn"
          (%ensure-http-fn backend)))))

(defun %invoke-http (backend req)
  (let ((fn (%ensure-http-fn backend)))
    (funcall fn req)))

(defun %elements-from-response (backend req response)
  (let ((status (%response-status response))
        (body (%response-body response)))
    (when (and (integerp status) (>= status 400))
      (restart-case
          (error 'unstructured-http-error
                 :status status
                 :url (unstructured-http-request-url req)
                 :body (%body-string body)
                 :message (format nil "unstructured HTTP ~a" status))
        (use-value (value)
          :report "Use a supplied element list or JSON"
          (return-from %elements-from-response
            (decode-unstructured-json value)))
        (retry ()
          :report "Retry the partition POST"
          (return-from %elements-from-response
            (%elements-from-response backend req (%invoke-http backend req))))))
    (decode-unstructured-json body)))

(defun partition (backend source &key format)
  "POST SOURCE to /general/v0/general and return the element list."
  (tagbody
   :again
     (return-from partition
       (let ((req (build-partition-request backend source :format format)))
         (restart-case
             (%elements-from-response backend req (%invoke-http backend req))
           (use-value (value)
             :report "Use a supplied element list or JSON"
             (decode-unstructured-json value))
           (retry ()
             :report "Retry partition"
             (go :again)))))))

(defun make-http-protocol-http-fn (&key backend client)
  "http-fn that POSTs the partition request through http-protocol:SEND.

   BACKEND defaults to http-protocol:*HTTP-BACKEND* at call time (load
   http-backend-async with event-backend-libuv for the stack client); CLIENT
   to *HTTP-CLIENT* or a fresh client for BACKEND. Returns (status body)
   with the body as octets."
  (lambda (req)
    (let* ((backend (or backend
                        http-protocol:*http-backend*
                        (error 'doc-extract-error
                               :message "http-protocol:*http-backend* is nil — load an http backend (e.g. http-backend-async)")))
           (client (or client
                       http-protocol:*http-client*
                       (http-protocol:make-http-client backend)))
           (response (http-protocol:send backend client
                                         (unstructured-request->http-request req)))
           (body (http-protocol:response-body response)))
      (list (or (http-protocol:response-status response) 200)
            (if (streamp body) (http-protocol:slurp-octets body) body)))))

(defmethod extract-metadata :around ((backend unstructured-backend) source
                                     &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied metadata plist"
      value)
    (retry ()
      :report "Retry extract-metadata"
      (extract-metadata backend source :format format))))

(defmethod extract-sections :around ((backend unstructured-backend) source
                                     &key format)
  (restart-case (call-next-method)
    (use-value (value)
      :report "Use a supplied section list"
      value)
    (retry ()
      :report "Retry extract-sections"
      (extract-sections backend source :format format))))

(defmethod extract-document ((backend unstructured-backend) source &key format)
  (elements->document (partition backend source :format format)
                      :format format :source source))

(defmethod normalize-document ((backend unstructured-backend) raw &key format)
  (elements->document (decode-unstructured-json raw)
                      :format format :source raw))

(defmethod extract-text ((backend unstructured-backend) source &key format)
  (elements-plain-text (partition backend source :format format)))

(defmethod extract-metadata ((backend unstructured-backend) source &key format)
  (let* ((elements (partition backend source :format format))
         (doc (elements->document elements :format format :source source))
         (md (extracted-document-metadata doc)))
    (list :format (or format :unstructured)
          :title (and md (document-metadata-title md))
          :filename (and md (document-metadata-filename md))
          :language (and md (document-metadata-language md)))))

(defmethod extract-sections ((backend unstructured-backend) source &key format)
  (elements->sections (partition backend source :format format)
                      :format format :source source))

(register-extractor 'unstructured-backend
                    :formats +unstructured-formats+
                    :priority +unstructured-priority+)
