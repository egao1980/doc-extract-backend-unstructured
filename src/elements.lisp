(in-package #:doc-extract-backend-unstructured)

(defun %as-list (value)
  (cond
    ((null value) nil)
    ((listp value) value)
    ((and (vectorp value) (not (stringp value)))
     (coerce value 'list))
    (t (list value))))

(defun element-field (element key)
  "Read KEY (string or keyword) from a hash-table or alist element."
  (let* ((s (string-downcase (string key)))
         (kw (intern (string-upcase s) :keyword)))
    (cond
      ((hash-table-p element)
       (or (gethash s element)
           (gethash key element)
           (gethash kw element)
           (gethash (string key) element)))
      ((listp element)
       (or (cdr (assoc s element :test #'equal))
           (cdr (assoc key element :test #'equal))
           (getf element kw)
           (cdr (assoc kw element)))))))

(defun element-type-name (element)
  (let ((tp (element-field element "type")))
    (when tp
      (string-downcase (string tp)))))

(defun element-text (element)
  (or (element-field element "text") ""))

(defun element-metadata (element)
  (or (element-field element "metadata") nil))

(defun %meta (metadata key)
  (and metadata (element-field metadata key)))

(defun %coord-system-from (metadata)
  (let ((coords (%meta metadata "coordinates")))
    (when coords
      (let ((system (%meta coords "system"))
            (w (%meta coords "layout_width"))
            (h (%meta coords "layout_height")))
        (make-coord-system
         :origin :top-left
         :units (if (and system (search "pixel" (string-downcase (string system))))
                    :pixels
                    :pixels)
         :layout-width w
         :layout-height h)))))

(defun %bbox-from (metadata)
  (let* ((coords (%meta metadata "coordinates"))
         (points (and coords (%meta coords "points"))))
    (when points
      (%as-list points))))

(defun %page-from (metadata)
  (let ((n (%meta metadata "page_number")))
    (cond
      ((integerp n) n)
      ((and (numberp n) (not (complexp n))) (round n))
      ((stringp n) (parse-integer n :junk-allowed t))
      (t nil))))

(defun %provenance (element)
  (let ((md (element-metadata element)))
    (when md
      (let ((page (%page-from md))
            (bbox (%bbox-from md))
            (cs (%coord-system-from md)))
        (when (or page bbox cs)
          (list (make-provenance-entry :page page :bbox bbox
                                       :coord-system cs)))))))

(defun %element-id (element)
  (let ((id (or (element-field element "element_id")
                (element-field element "element-id"))))
    (when (and id (plusp (length (string id))))
      (string id))))

(defun %strip-tags (html)
  (let ((out (make-array (length html) :element-type 'character
                         :fill-pointer 0)))
    (loop with i = 0
          with n = (length html)
          while (< i n)
          do (let ((c (char html i)))
               (if (char= c #\<)
                   (let ((gt (position #\> html :start i)))
                     (setf i (if gt (1+ gt) n)))
                   (progn
                     (vector-push c out)
                     (incf i)))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) (copy-seq out))))

(defun %html-table-rows (html)
  (let ((rows '())
        (start 0))
    (loop
      (let ((tr (search "<tr" html :start2 start :test #'char-equal)))
        (unless tr
          (return (nreverse rows)))
        (let* ((gt (position #\> html :start tr))
               (end (and gt (search "</tr>" html :start2 gt :test #'char-equal))))
          (unless (and gt end)
            (return (nreverse rows)))
          (let ((inner (subseq html (1+ gt) end))
                (cells '())
                (cstart 0)
                (headers 0))
            (loop
              (let ((th (search "<th" inner :start2 cstart :test #'char-equal))
                    (td (search "<td" inner :start2 cstart :test #'char-equal)))
                (let ((open (cond
                              ((and th td) (min th td))
                              (th th)
                              (td td)
                              (t nil))))
                  (unless open
                    (return))
                  (let* ((header-p (eql open th))
                         (gt (position #\> inner :start open))
                         (close-tag (if header-p "</th>" "</td>"))
                         (close (and gt (search close-tag inner :start2 gt
                                                :test #'char-equal))))
                    (unless (and gt close)
                      (return))
                    (when header-p (incf headers))
                    (push (list :text (%strip-tags (subseq inner (1+ gt) close))
                                :header-p header-p)
                          cells)
                    (setf cstart (+ close (length close-tag)))))))
            (push (nreverse cells) rows)
            (setf start (+ end 5))))))))

(defun %make-table-block (element)
  (let* ((md (element-metadata element))
         (html (and md (%meta md "text_as_html")))
         (rows (and html (plusp (length html)) (%html-table-rows html)))
         (text (element-text element))
         (prov (%provenance element))
         (id (%element-id element)))
    (cond
      (rows
       (let* ((ncols (loop for r in rows maximize (length r)))
              (nrows (length rows))
              (cells '()))
         (loop for r in rows
               for ri from 0
               do (loop for cell in r
                        for ci from 0
                        do (push (make-table-cell
                                  :row ri :col ci
                                  :header-p (getf cell :header-p)
                                  :content (list (make-text-block
                                                  :text (getf cell :text)
                                                  :kind :para)))
                                 cells)))
         (make-table-block :id id :rows nrows :cols ncols
                           :cells (nreverse cells) :provenance prov)))
      ((and text (plusp (length text)))
       (make-table-block :id id :rows 1 :cols 1 :provenance prov
                         :cells (list (make-table-cell
                                       :row 0 :col 0
                                       :content (list (make-text-block
                                                       :text text
                                                       :kind :para))))))
      (t
       (make-table-block :id id :provenance prov)))))

(defun %make-list-item (element)
  (make-text-block :id (%element-id element)
                   :text (element-text element)
                   :kind :para
                   :provenance (%provenance element)))

(defun %make-text (element &key (kind :para) layer)
  (apply #'make-text-block
         :id (%element-id element)
         :text (element-text element)
         :kind kind
         :provenance (%provenance element)
         (when layer (list :layer layer))))

(defun %make-image (element)
  (make-image-block :id (%element-id element)
                    :alt (element-text element)
                    :ocr-text (element-text element)
                    :provenance (%provenance element)))

(defun %make-code (element)
  (make-code-block :id (%element-id element)
                   :text (element-text element)
                   :provenance (%provenance element)))

(defun %append-child (section-or-nil roots block)
  (if section-or-nil
      (setf (section-children section-or-nil)
            (append (or (section-children section-or-nil) nil)
                    (list block)))
      (push block roots))
  roots)

(defun %furniture-type-p (type)
  (member type '("header" "footer" "page-header" "page-footer"
                 "pageheader" "pagefooter")
          :test #'string=))

(defun elements->document (elements &key format source)
  "Map an Unstructured element list to EXTRACTED-DOCUMENT.
   Title opens a section; NarrativeText / ListItem / Table nest under the
   last title. Consecutive ListItems become one list-block."
  (let ((elements (%as-list (if (and (hash-table-p elements)
                                     (not (element-type-name elements)))
                                (or (element-field elements "elements")
                                    (error 'doc-extract-error
                                           :format format
                                           :message (or (element-field elements "detail")
                                                        "unstructured response is not an element list")))
                                elements)))
        (roots '())
        (current nil)
        (pending-items '())
        (pages '())
        (title nil)
        (filename nil)
        (language nil))
    (labels ((flush-list ()
               (when pending-items
                 (let ((lb (make-list-block :items (nreverse pending-items))))
                   (setf roots (%append-child current roots lb)
                         pending-items nil))))
             (note-meta (el)
               (let ((md (element-metadata el)))
                 (when md
                   (unless filename
                     (setf filename (%meta md "filename")))
                   (unless language
                     (let ((langs (%meta md "languages")))
                       (setf language (if (consp langs)
                                          (car langs)
                                          langs))))
                   (let ((p (%page-from md)))
                     (when (and p (not (member p pages)))
                       (push p pages))))))
             (nest (block)
               (setf roots (%append-child current roots block))))
      (dolist (el elements)
        (note-meta el)
        (let ((type (or (element-type-name el) "")))
          (cond
            ((string= type "title")
             (flush-list)
             (let ((sec (make-section-block
                         :id (%element-id el)
                         :title (element-text el)
                         :level 1
                         :provenance (%provenance el))))
               (unless title
                 (setf title (element-text el)))
               (push sec roots)
               (setf current sec)))
            ((string= type "narrativetext")
             (flush-list)
             (nest (%make-text el :kind :para)))
            ((string= type "listitem")
             (push (%make-list-item el) pending-items))
            ((string= type "table")
             (flush-list)
             (nest (%make-table-block el)))
            ((%furniture-type-p type)
             (flush-list)
             (push (%make-text el :kind :para :layer :furniture) roots))
            ((member type '("image" "figure" "picture") :test #'string=)
             (flush-list)
             (nest (%make-image el)))
            ((member type '("code" "codesnippet" "formula") :test #'string=)
             (flush-list)
             (nest (%make-code el)))
            (t
             (flush-list)
             (when (plusp (length (element-text el)))
               (nest (%make-text el :kind :para)))))))
      (flush-list)
      (let* ((src-name (cond
                         ((pathnamep source) (file-namestring source))
                         ((and (stringp source)
                               (position #\. source :from-end t)
                               (< (length source) 256)
                               (not (find #\Newline source)))
                          (file-namestring (pathname source)))
                         (t nil)))
             (doc (make-extracted-document
                   :metadata (make-document-metadata
                              :title title
                              :filename (or filename src-name)
                              :language language
                              :mimetype (when format
                                          (format nil "~(~a~)" format)))
                   :blocks (nreverse roots)
                   :pages (mapcar (lambda (n) (make-page-info :number n))
                                  (sort (copy-list pages) #'<)))))
        (ensure-ids doc)
        doc))))

(defun %block-tree-text (block)
  (let ((parts '()))
    (labels ((walk (b)
               (let ((txt (block-plain-text b)))
                 (when (plusp (length txt))
                   (push txt parts)))
               (dolist (c (block-children b))
                 (walk c))))
      (if (typep block 'section)
          (dolist (c (section-children block))
            (walk c))
          (walk block)))
    (format nil "~{~a~^~%~}" (nreverse parts))))

(defun document->extracted-sections (doc)
  (loop for b in (extracted-document-blocks doc)
        when (typep b 'section)
          collect (make-extracted-section
                   :title (or (section-title b) "")
                   :level (or (section-level b) 1)
                   :text (%block-tree-text b))))

(defun elements->sections (elements &key format source)
  (document->extracted-sections
   (elements->document elements :format format :source source)))

(defun elements-plain-text (elements)
  (format nil "~{~a~^~%~}"
          (loop for el in (%as-list elements)
                for text = (element-text el)
                when (plusp (length text))
                  collect text)))
