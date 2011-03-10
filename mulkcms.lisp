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

(defun find-mulkcms-request-handler (path &optional action)
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
                 (revisions (query "SELECT author, date, format, status,
                                           global_id, title, content
                                      FROM article_revisions
                                     WHERE article = $1
                                    ORDER BY date DESC"
                                   article
                                   :lists))
                 (revision-data (first revisions))
                 (page-skeleton (template "page_skeleton"))
                 (page-template (template page-template-name)))
            (destructuring-bind (author date format status global-id title content)
                revision-data
              (let* ((template-params (list :title title
                                            :root *base-uri*
                                            :site-name *site-name*
                                            :site-subtitle ""
                                            :link ""
                                            ;; Article stuff
                                            ))
                     (article-params (list :publishing-date date
                                           :title title
                                           :body content
                                           ;;FIXME
                                           :link ""
                                           :commentary nil
                                           :edit-link ""
                                           :edit-button-label "Edit"
                                           :comment-feed ""
                                           :comment-feed-label "Comment feed"
                                           :comments-label "Comments"
                                           :comments-link ""
                                           :comments-heading "Comments"))
                     (head (expand-template page-template (list* :head t
                                                                 :articles (list article-params)
                                                                 template-params)))
                     (body (expand-template page-template (list* :body t
                                                                 :articles (list article-params)
                                                                 template-params))))
                (expand-template page-skeleton (list :title title
                                                     :head head
                                                     :body body))))))))))
