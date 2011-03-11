(in-package #:mulkcms)

(defparameter *database-connection-spec*
  (list *database-name* *database-user* *database-password* *database-host*
        :pooled-p t :use-ssl :try))

(unless (member "html-human-date" *template-formatters* :key #'car :test #'equal)
  (setq *template-formatters*
        (list* (cons "html-human-date" 'format-human-date)
               (cons "html-short-human-date" 'format-short-human-date)
               (cons "html-iso-date"   'format-iso-date)
               (cons "article-html"    'format-article)
               *template-formatters*)))


(defun call-with-db (thunk)
  (call-with-connection *database-connection-spec* thunk))

(defmacro with-db (&body body)
  `(call-with-db (lambda () ,@body)))

(defun find-template (template-name)
  (first (directory (make-pathname :name template-name
                                   :type :wild
                                   :directory *templates*))))

(defun format-short-human-date (date)
  ;; FIXME
  "(some date)")

(defun format-human-date (date)
  ;; FIXME
  "(some date)")

(defun format-iso-date (date)
  ;; FIXME
  "(some date)")

(defun template (template-name)
  (parse-template-string (read-file-into-string (find-template template-name))))

(defun format-article (article-params)
  (let ((article-template (template "article")))
    (expand-template article-template article-params)))

(defun paramify-article (revision-data &optional (comments nil commentary-p))
  (destructuring-bind (rid article date title content author format status
                       global-id &rest args)
      revision-data
    (declare (ignore args rid))
    (list :publishing-date date
          :title title
          :body content
          ;;FIXME
          :link ""
          :commentary (if commentary-p (list :comments comments) nil)
          :edit-link ""
          :edit-button-label "Edit"
          :comment-feed ""
          :comment-feed-label "Comment feed"
          :comments-label "Comments"
          :comments-link ""
          :comments-heading "Comments")))

(defun format-comment-content (text)
  ;; Taken from Mulkblog.
  (with-html-output-to-string (out)
    (loop for last-position = 0 then (cadr matches)
          for matches = (ppcre:all-matches "(\\n|\\r|\\r\\n)(\\n|\\r|\\r\\n)+" text)
            then (cddr matches)
          while (not (endp matches))
          do (htm (:p (esc (subseq text last-position (car matches)))))
          finally
            (htm (:p (esc (subseq text last-position)))))))

#-portable-mulkcms
(defun make-characteristic-lists (characteristics &aux (first1-p t) (first2-p t))
  (with-output-to-string (out)
    (format out "(ARRAY[")
    (dolist (characteristic-list characteristics)
      (if first1-p
          (setq first1-p nil)
          (format out ", "))
      (format out "ROW(ARRAY[")
      (dolist (ch characteristic-list)
        (if first2-p
            (setq first2-p nil)
            (format out ", "))
        (format out "ROW(~A, ~A)::characteristic"
                (sql-escape (car ch))
                (sql-escape (cdr ch))))
      (format out "])"))
    (format out "]::characteristic_list[])")))

#-portable-mulkcms
(defun find-article-revisions (article characteristics)
  (query (format nil "SELECT article_revisions_for_characteristics($1, ~A)"
                 (make-characteristic-lists characteristics))
         article
         :column))

#-portable-mulkcms
(defun find-all-revisions (characteristics &optional constraints)
  (query
   (format nil
           "SELECT (most_recent_revision(r)).*
              FROM (SELECT article_revisions_for_characteristics(a.id, ~A) AS revision
                      FROM articles a)
                   AS mr
              JOIN article_revisions r ON r.id = mr.revision
             GROUP BY article
            HAVING ~A
             ORDER BY (oldest_revision(r)).date"
           (make-characteristic-lists characteristics)
           (or constraints "true"))))


#+portable-mulkcms
(defun find-article-revisions (article characteristics &optional fixed-characteristics)
  ;; CHARACTERISTICS --- a proper list.
  ;;
  ;; CHARACTERISTICS is a list of lists of (key . value) pairs.  Each component list
  ;; is a precedence list of characteristics that are to be considered fallbacks for
  ;; each other and that will be tried in order.
  (let* ((fixed-characteristics-conditions
          (mapcar (lambda (x)
                    (format nil "AND EXISTS
                                   (SELECT 1
                                      FROM article_revision_characteristics
                                     WHERE revision = article_revisions.id
                                       AND characteristic = ~A
                                       AND value = ~A)"
                            (sql-escape (car x))
                            (sql-escape (cdr x))))
                  fixed-characteristics))
         (query (format nil
                        "SELECT id
                           FROM article_revisions
                          WHERE article = $1~
                          ~{~&~A~}
                          ORDER BY date DESC"
                        fixed-characteristics-conditions)))
    (when-let ((revisions (query query article :column)))
      (if (consp characteristics)
          (progn
            (dolist (potential-fixed-characteristic (first characteristics))
              (when-let ((more-specific-revisions
                          (find-article-revisions article
                                                  (rest characteristics)
                                                  (cons potential-fixed-characteristic
                                                        fixed-characteristics))))
                (return-from find-article-revisions more-specific-revisions)))
            revisions)
          revisions))))

;;;; Alternative definition.
;;
;; #+portable-mulkcms
;; (defun find-article-revisions (article characteristics)
;;   ;; CHARACTERISTICS --- a proper list.
;;   ;;
;;   ;; CHARACTERISTICS is a list of lists of (key . value) pairs.  Each component list
;;   ;; is a precedence list of characteristics that are to be considered fallbacks for
;;   ;; each other and that will be tried in order.
;;   (labels ((find-more-specific-revisions (revisions characteristics)
;;              (let ((alternatives (first characteristics))
;;                    (rest (rest characteristics)))
;;                (dolist (alternative alternatives)
;;                  (when-let ((potential-revisions
;;                              (remove-if-not (lambda (x)
;;                                               (query "SELECT 1
;;                                                       FROM article_revision_characteristics
;;                                                       WHERE revision = $1
;;                                                         AND characteristic = $2                                                     AND value = $3"
;;                                                      x
;;                                                      (car alternative)
;;                                                      (cdr alternative)
;;                                                      :column))
;;                                             revisions)))
;;                    (if-let ((subresults (find-more-specific-revisions potential-revisions rest)))
;;                      (return-from find-more-specific-revisions subresults)
;;                      (return-from find-more-specific-revisions potential-revisions)))))))
;;     (let ((revisions (query "SELECT id FROM article_revisions
;;                               WHERE article = $1
;;                               ORDER BY date DESC"
;;                             article
;;                             :column)))
;;       (if characteristics
;;           (find-more-specific-revisions revisions characteristics)
;;           revisions))))

(defun paramify-comment (comment-revision-data)
  (destructuring-bind (crid comment date content author format status
                       article-revision &rest args)
      comment-revision-data
    (declare (ignore args crid article-revision status format comment))
    (list :publishing-date date
          :body (format-comment-content content)
          :author author
          ;;FIXME
          :link ""
          :edit-link ""
          :edit-button-label "Edit"
          :generic-commenter-name "Someone")))

(defprepared find-journal-articles
  "SELECT article
     FROM article_revisions
    WHERE status IN ('published', 'syndicated')
    GROUP BY article
   HAVING EXISTS (SELECT 1 FROM article_aliases
                   WHERE article = article_revisions.article
                     AND alias LIKE 'journal/%')
    ORDER BY min(date) DESC"
  :column)

(defun find-journal-archive-request-handler (path full-p &optional action characteristics)
  (declare (ignore action))
  (when (or (string= path "journal")
            (string= path "journal/"))
    (lambda ()
      (with-db
        (let* (#+portable-mulkcms
               (articles (find-journal-articles))
               #+portable-mulkcms
               (revisions (remove-if #'null
                                     (mapcar (lambda (x)
                                               (find-article-params x characteristics))
                                             articles)))
               #-portable-mulkcms
               (revisions
                (mapcar #'paramify-article
                        (find-all-revisions characteristics
                                            "EXISTS (SELECT 1
                                                       FROM article_aliases
                                                      WHERE article = r.article
                                                        AND alias LIKE 'journal/%')")))
               (displayed-revisions (if full-p revisions (subseq revisions 0 10)))
               (page-skeleton (template "page_skeleton"))
               (page-template (template "journal_page"))
               (template-params (list :title *site-name*
                                      :root *base-uri*
                                      :site-name *site-name*
                                      :site-subtitle ""
                                      :link ""
                                      :full-archive-link ""
                                      :full-archive-label "Full archive (slow!)"))
               (head (expand-template page-template (list* :head t
                                                           :articles displayed-revisions
                                                           :minor-articles revisions
                                                           template-params)))
               (body (expand-template page-template (list* :body t
                                                           :articles displayed-revisions
                                                           :minor-articles revisions
                                                           template-params))))
          (expand-template page-skeleton (list :title *site-name*
                                               :head head
                                               :body body)))))))

(defun find-article-params (article characteristics &optional commentary-p)
  (let* ((revisions (find-article-revisions article characteristics))
         (comment-data (if commentary-p
                           (query "SELECT id FROM comments WHERE article = $1"
                                  article
                                  :column)
                           (list)))
         (comment-revision-data
          (remove-if #'null
                     (mapcar (lambda (cid)
                               (first
                                (query "SELECT *
                                          FROM comment_revisions 
                                         WHERE comment = $1
                                           AND status IN ('approved', 'trusted')
                                         ORDER BY date DESC"
                                       cid
                                       :lists)))
                             comment-data)))
         (comments (mapcar #'paramify-comment comment-revision-data))
         (revision (first revisions))
         (revision-data (query "SELECT * FROM article_revisions WHERE id = $1"
                               revision
                               :row)))
    (cond ((null revision-data)
           nil)
          (commentary-p
           (paramify-article revision-data comments))
          (t
           (paramify-article revision-data)))))


(defun find-article-request-handler (path &optional action characteristics)
  (with-db
    (when-let ((article (query "SELECT article FROM article_aliases
                                 WHERE alias = $1"
                               path
                               :single)))
      (lambda ()
        (with-db
          (let* ((page-template-name (query "SELECT page_template FROM articles
                                               JOIN article_types
                                                 ON articles.type = article_types.id
                                              WHERE articles.id = $1"
                                            article
                                            :single!))
                 (article-params (find-article-params article characteristics))
                 (page-skeleton (template "page_skeleton"))
                 (page-template (template page-template-name))
                 (template-params (list :title (getf article-params :title)
                                        :root *base-uri*
                                        :site-name *site-name*
                                        :site-subtitle ""
                                        :link ""))
                 (head (expand-template page-template (list* :head t
                                                             :articles (list article-params)
                                                             template-params)))
                 (body (expand-template page-template (list* :body t
                                                             :articles (list article-params)
                                                             template-params))))
            (expand-template page-skeleton (list :title (getf article-params :title)
                                                 :head head
                                                 :body body))))))))
