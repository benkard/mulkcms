(in-package #:mulkcms)

(defvar +lang-ns+
    "http://matthias.benkard.de/software/language-markup/1.0")

(defun in-tag-list-p (string tag-list)
  (ppcre:scan (format nil "\\b~A\\b" string) tag-list))


(defun language-filter-sub-xmls (language node)
  (if (listp node)
      (destructuring-bind (name attrs &rest subnodes)
          node
        (let ((langs (find `("langs" . ,+lang-ns+)
                           attrs :key #'first :test #'equal))
              (unlangs (find `("unlangs" . ,+lang-ns+)
                             attrs :key #'first :test #'equal)))
          (if (or (and (null langs) (null unlangs))
                  (and langs   (in-tag-list-p language (second langs)))
                  (and unlangs (not (in-tag-list-p language (second unlangs)))))
              (list (list* name
                           attrs
                           (mapcan (lambda (x)
                                     (language-filter-sub-xmls language x))
                                   subnodes)))
              nil)))
      (list node)))

(defun language-filter-xmls (language node)
  (first (language-filter-sub-xmls language node)))


;; Test:
#+(or)
(defvar *test-xml* "<html xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:m=\"http://matthias.benkard.de/software/language-markup/1.0\">
  <body>
    <h1 m:langs=\"la\">Documentatio</h1>
    <h1 m:langs=\"en fr\">Documentation</h1>
    <h1 m:unlangs=\"la fr en\">Dokumentation</h1>
  </body>
</html>")

#+(or)
(let ((language "la"))                  ;or one of "de", "en", "fr"
  (with-output-to-string (stream)
    (let ((node (cxml:parse *test-xml* (cxml-xmls:make-xmls-builder))))
      (cxml-xmls:map-node (cxml:make-character-stream-sink stream)
                          (language-filter-xmls language node)))))



;; Don't be fooled by the length of this function.  It does
;; significantly more than the xmls version above!
(defun language-filter-stp (language stp)
  (let ((wantp (lambda (node)
                 (or (not (typep node 'stp:element))
                     (stp:with-attributes ((langs   "langs"   +lang-ns+)
                                           (unlangs "unlangs" +lang-ns+))
                         node
                       (or (and (null langs) (null unlangs))
                           (and langs   (in-tag-list-p language langs))
                           (and unlangs (not (in-tag-list-p language unlangs))))))))
        (remove-lang-attributes
         (lambda (node)
                                  
           (when (typep node 'stp:element)
             (let ((langs   (stp:find-attribute-named node "langs"   +lang-ns+))
                   (unlangs (stp:find-attribute-named node "unlangs" +lang-ns+)))
               (when langs
                 (stp:remove-attribute node langs))
               (when unlangs
                 (stp:remove-attribute node unlangs)))))))
    (dolist (unwanted-element (stp:filter-recursively (complement wantp) stp))
      (stp:delete-child unwanted-element (stp:parent unwanted-element)))
    (stp:map-recursively remove-lang-attributes (stp:document-element stp))
    (stp:map-extra-namespaces
     (lambda (prefix uri)
       (when (string= uri +lang-ns+)
         (stp:remove-extra-namespace (stp:document-element stp) prefix)))
     (stp:document-element stp))
    stp))



;; Test:
#+(or)
(let ((language "la"))  ;or one of "de", "en", "fr"
  (with-output-to-string (stream)
    (let ((node (cxml:parse *test-xml* (stp:make-builder))))
      (stp:serialize (language-filter-stp language node)
                     (cxml:make-character-stream-sink stream)))))



