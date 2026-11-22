(in-package #:doc-extract-backend-unstructured/tests)

;;; doc-extract-backend-unstructured#1: the upload goes through http-protocol
;;; multipart, not curl -F, so names and strategy values are plain data.

(defclass capture-backend (http-protocol:http-backend)
  ((requests :initform nil :accessor capture-requests)
   (reply :initarg :reply :reader capture-reply))
  (:default-initargs :name "capture"))

(defmethod http-protocol:send ((backend capture-backend) client request &key)
  (declare (ignore client))
  (push request (capture-requests backend))
  (make-instance 'http-protocol:http-response
                 :status 200
                 :headers (make-hash-table :test #'equal)
                 :body (encoding-protocol:encode (capture-reply backend) :encoding :utf-8)
                 :url (http-protocol:http-request-url request)
                 :request request))

(defun %wire-text (request)
  "The multipart body http-protocol would send for REQUEST, as UTF-8 text."
  (multiple-value-bind (body extra clen) (http-protocol:prepare-request-body request)
    (declare (ignore extra))
    (let ((octets (http-protocol:slurp-octets body)))
      (values (encoding-protocol:decode octets :encoding :utf-8) (length octets) clen))))

(defun %lines (text)
  (loop with start = 0
        for pos = (search (format nil "~C~C" #\Return #\Newline) text :start2 start)
        collect (subseq text start (or pos (length text)))
        while pos
        do (setf start (+ pos 2))))

(defun %evil-request ()
  (doc-extract-backend-unstructured::make-unstructured-http-request
   :method :post
   :url "http://127.0.0.1:8000/general/v0/general"
   ;; curl -F would read /etc/passwd for "@..." and "<...".
   :strategy "@/etc/passwd"
   :filename (format nil "a.pdf;type=text/html;filename=x\"~C~CX-Evil: 1" #\Return #\Newline)
   :content-type "application/pdf"
   :octets (encoding-protocol:encode "%PDF-1.4" :encoding :utf-8)
   :fields (list (cons "languages" "<~/.ssh/id_rsa"))))

(deftest http-fn-uses-http-protocol
  (let* ((reply "[{\"type\":\"Title\",\"text\":\"Grüße 日本\",\"metadata\":{}}]")
         (backend (make-instance 'capture-backend :reply reply))
         (fn (make-http-protocol-http-fn :backend backend))
         (result (funcall fn (%evil-request))))
    (ok (= 200 (first result)))
    (ok (= 1 (length (capture-requests backend))) "sent once through SEND")
    (let ((els (decode-unstructured-json (second result))))
      (ok (string= "Grüße 日本" (element-field (first els) "text"))
          "response body decoded as UTF-8"))))

(deftest http-fn-values-cannot-rewrite-upload
  (let ((request (unstructured-request->http-request (%evil-request))))
    (multiple-value-bind (text n clen) (%wire-text request)
      (let* ((lines (%lines text))
             (disp (remove-if-not (lambda (l) (search "Content-Disposition:" l)) lines)))
        (ok (= n clen))
        (ok (= 3 (length disp)) "strategy, languages, files — no extra part")
        (ok (member "@/etc/passwd" lines :test #'string=)
            "strategy sent literally as the field value")
        (ok (member "<~/.ssh/id_rsa" lines :test #'string=)
            "extra field sent literally")
        (ok (notany (lambda (l) (string= l "X-Evil: 1")) lines) "no forged header")
        (ok (= 1 (count-if (lambda (l) (search "Content-Type:" l)) lines))
            "only the file part's Content-Type")
        (ok (member "Content-Type: application/pdf" lines :test #'string=))
        (ok (search "filename=\"a.pdf;type=text/html;filename=x%22%0D%0AX-Evil: 1\""
                    (third disp))
            "filename quoted, quote and CRLF escaped")
        (ok (search "%PDF-1.4" text) "file octets come from the request, not disk")))))

(deftest partition-through-http-protocol
  (let* ((backend (make-instance 'capture-backend
                                 :reply "[{\"type\":\"NarrativeText\",\"text\":\"ok\",\"metadata\":{}}]"))
         (b (make-unstructured-backend
             :strategy "@/etc/passwd"
             :http-fn (make-http-protocol-http-fn :backend backend)))
         (doc (doc-extract-protocol:extract-document b "hello" :format :txt)))
    (ok (doc-extract-protocol:extracted-document-p doc))
    (let ((text (%wire-text (first (capture-requests backend)))))
      (ok (search (format nil "name=\"strategy\"~C~C~C~C@/etc/passwd"
                          #\Return #\Newline #\Return #\Newline)
                  text)))))
