(in-package #:mulkcms)

(defparameter *database-connection-spec*
  (list *database-name* *database-user* *database-password* *database-host*
        :pooled-p t :use-ssl :try))

(unless (member "html-human-date" *template-formatters* :key #'car :test #'equal)
  (setq *template-formatters*
        (list* (cons "html-human-date" 'format-human-date)
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

(defun find-mulkcms-request-handler (path &optional action characteristics)
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
                 (revisions (query "SELECT *
                                      FROM article_revisions
                                     WHERE article = $1
                                    ORDER BY date DESC"
                                   article
                                   :lists))
                 (comment-data (query "SELECT id FROM comments WHERE article = $1"
                                      article
                                      :column))
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
                 (revision-data (first revisions))
                 (page-skeleton (template "page_skeleton"))
                 (page-template (template page-template-name))
                 (template-params (list :title (fourth revision-data)
                                        :root *base-uri*
                                        :site-name *site-name*
                                        :site-subtitle ""
                                        :link ""))
                 (article-params (paramify-article revision-data comments))
                 (head (expand-template page-template (list* :head t
                                                             :articles (list article-params)
                                                             template-params)))
                 (body (expand-template page-template (list* :body t
                                                             :articles (list article-params)
                                                             template-params))))
            (expand-template page-skeleton (list :title (fourth revision-data)
                                                 :head head
                                                 :body body))))))))
