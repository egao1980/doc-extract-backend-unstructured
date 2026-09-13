(in-package #:doc-extract-backend-unstructured)

;;; Minimal RFC 8259 decoder for Unstructured element JSON.
;;; Soft-uses json-protocol:DECODE when *JSON-BACKEND* is bound.

(defun %skip-ws (string start)
  (loop for i from start below (length string)
        unless (member (char string i) '(#\Space #\Tab #\Newline #\Return))
          return i
        finally (return (length string))))

(defun %parse-json-string (string start)
  (unless (and (< start (length string)) (char= (char string start) #\"))
    (error 'doc-extract-error :message "expected JSON string"))
  (let ((out (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for i from (1+ start) below (length string)
          for c = (char string i)
          do (case c
               (#\"
                (return (values (copy-seq out) (1+ i))))
               (#\\
                (when (>= (1+ i) (length string))
                  (error 'doc-extract-error :message "unterminated JSON escape"))
                (let ((e (char string (1+ i))))
                  (cond
                    ((char= e #\u)
                     (when (> (+ i 6) (length string))
                       (error 'doc-extract-error :message "truncated \\u escape"))
                     (vector-push-extend
                      (code-char (parse-integer string :start (+ i 2)
                                                :end (+ i 6) :radix 16))
                      out)
                     (incf i 5))
                    (t
                     (vector-push-extend
                      (case e
                        (#\" #\")
                        (#\\ #\\)
                        (#\/ #\/)
                        (#\b #\Backspace)
                        (#\f #\Page)
                        (#\n #\Newline)
                        (#\r #\Return)
                        (#\t #\Tab)
                        (t e))
                      out)
                     (incf i)))))
               (t (vector-push-extend c out)))
          finally (error 'doc-extract-error :message "unterminated JSON string"))))

(defun %parse-json-number (string start)
  (let* ((end start)
         (len (length string)))
    (when (and (< end len) (char= (char string end) #\-))
      (incf end))
    (loop while (and (< end len) (digit-char-p (char string end)))
          do (incf end))
    (when (and (< end len) (char= (char string end) #\.))
      (incf end)
      (loop while (and (< end len) (digit-char-p (char string end)))
            do (incf end)))
    (when (and (< end len) (member (char string end) '(#\e #\E)))
      (incf end)
      (when (and (< end len) (member (char string end) '(#\+ #\-)))
        (incf end))
      (loop while (and (< end len) (digit-char-p (char string end)))
            do (incf end)))
    (when (= end start)
      (error 'doc-extract-error :message "expected JSON number"))
    (values (read-from-string string t nil :start start :end end) end)))

(defun %parse-json-object (string start)
  (let ((i (%skip-ws string start))
        (table (make-hash-table :test 'equal)))
    (unless (and (< i (length string)) (char= (char string i) #\{))
      (error 'doc-extract-error :message "expected JSON object"))
    (setf i (%skip-ws string (1+ i)))
    (when (and (< i (length string)) (char= (char string i) #\}))
      (return-from %parse-json-object (values table (1+ i))))
    (loop
      (multiple-value-bind (key j) (%parse-json-string string i)
        (setf i (%skip-ws string j))
        (unless (and (< i (length string)) (char= (char string i) #\:))
          (error 'doc-extract-error :message "expected colon in JSON object"))
        (multiple-value-bind (val k) (%parse-json-value string (1+ i))
          (setf (gethash key table) val
                i (%skip-ws string k))))
      (cond
        ((and (< i (length string)) (char= (char string i) #\,))
         (setf i (%skip-ws string (1+ i))))
        ((and (< i (length string)) (char= (char string i) #\}))
         (return (values table (1+ i))))
        (t (error 'doc-extract-error
                  :message "expected comma or end of JSON object"))))))

(defun %parse-json-array (string start)
  (let ((i (%skip-ws string start))
        (acc nil))
    (unless (and (< i (length string)) (char= (char string i) #\[))
      (error 'doc-extract-error :message "expected JSON array"))
    (setf i (%skip-ws string (1+ i)))
    (when (and (< i (length string)) (char= (char string i) #\]))
      (return-from %parse-json-array (values nil (1+ i))))
    (loop
      (multiple-value-bind (val j) (%parse-json-value string i)
        (push val acc)
        (setf i (%skip-ws string j)))
      (cond
        ((and (< i (length string)) (char= (char string i) #\,))
         (setf i (%skip-ws string (1+ i))))
        ((and (< i (length string)) (char= (char string i) #\]))
         (return (values (nreverse acc) (1+ i))))
        (t (error 'doc-extract-error
                  :message "expected comma or end of JSON array"))))))

(defun %parse-json-value (string start)
  (let ((i (%skip-ws string start)))
    (when (>= i (length string))
      (error 'doc-extract-error :message "unexpected end of JSON"))
    (let ((c (char string i)))
      (cond
        ((char= c #\")
         (%parse-json-string string i))
        ((or (char= c #\-) (digit-char-p c))
         (%parse-json-number string i))
        ((char= c #\{)
         (%parse-json-object string i))
        ((char= c #\[)
         (%parse-json-array string i))
        ((and (<= (+ i 4) (length string))
              (string= string "true" :start1 i :end1 (+ i 4)))
         (values t (+ i 4)))
        ((and (<= (+ i 5) (length string))
              (string= string "false" :start1 i :end1 (+ i 5)))
         (values nil (+ i 5)))
        ((and (<= (+ i 4) (length string))
              (string= string "null" :start1 i :end1 (+ i 4)))
         (values nil (+ i 4)))
        (t (error 'doc-extract-error
                  :message (format nil "invalid JSON at ~d" i)))))))

(defun %octets-to-string (octets)
  (map 'string #'code-char octets))

(defun %try-json-protocol (string)
  (let* ((pkg (find-package :json-protocol))
         (decode (and pkg (find-symbol "DECODE" pkg)))
         (backend (and pkg (find-symbol "*JSON-BACKEND*" pkg))))
    (when (and decode (fboundp decode) backend (boundp backend)
               (symbol-value backend))
      (ignore-errors (funcall decode string)))))

(defun decode-unstructured-json (source)
  "SOURCE is a JSON string, octet vector, already-parsed list, or hash-table."
  (cond
    ((stringp source)
     (or (%try-json-protocol source)
         (nth-value 0 (%parse-json-value source 0))))
    ((or (listp source) (hash-table-p source))
     source)
    ((and (vectorp source) (not (stringp source)))
     (if (and (plusp (length source))
              (typep (aref source 0) '(unsigned-byte 8)))
         (decode-unstructured-json (%octets-to-string source))
         source))
    (t
     (error 'doc-extract-error
            :message (format nil "cannot decode unstructured JSON from ~s"
                             (type-of source))))))
