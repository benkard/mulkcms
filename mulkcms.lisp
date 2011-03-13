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


(defun find-canonical-article-alias (article)
  (query "SELECT alias FROM article_aliases WHERE article = $1 LIMIT 1"
         article
         :single))

(defun link-to (action &key comment-id article-id (absolute nil))
  ;; Taken from Mulkblog.
  (with-output-to-string (out)
    (format out "~A" (if absolute *base-uri* ""))
    (symbol-macrolet ((article-base (find-canonical-article-alias article-id)))
      (multiple-value-call
          #'(lambda (&rest args) (apply #'format out args))
        (ecase action
          (:index "")
          (:full-index "/?full")
          (:view-atom-feed (values "/feed"))
          (:view-comment-feed (cond (article-id (values "/~A?comment-feed" article-base))
                                    (t "/comment-feed")))
          (:view (cond (comment-id (values "/~A#comment-~D" article-base comment-id))
                       (article-id (values "/~A" article-base))
                       (t "/")))
          (:view-comments (values "/~A#comments" article-base))
          ((:edit :preview) (values "/~A?edit" article-base))
          (:post-comment (values "/~A" article-base))
          (:trackback (values "/~A?trackback" article-base))
          (:view-atom-entry (values "/~A?atom" article-base))
          (:save (values "/~A?save" article-base))
          (:moderation-page "/moderate")
          (:css "/journal.css")
          (:prettify.css "/prettify/prettify.css")
          (:prettify.js "/prettify/prettify.js")
          (:prettify-lisp.js "/prettify/lang-lisp.js")
          (:pingback "/rpc"))))))

(defun call-with-db (thunk)
  (call-with-connection *database-connection-spec* thunk))

(defmacro with-db (&body body)
  `(call-with-db (lambda () ,@body)))

(defun find-template (template-name)
  (first (directory (make-pathname :name template-name
                                   :type :wild
                                   :directory *templates*))))

(defun format-short-human-date (date)
  (multiple-value-bind (year month day hour minute second millisecond)
      (decode-timestamp date)
    (declare (ignore second millisecond))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year month day hour minute)))

(defun format-human-date (date)
  ;; FIXME
  (multiple-value-bind (year month day hour minute second millisecond)
      (decode-timestamp date)
    (declare (ignore second millisecond))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
            year month day hour minute)))

(defun format-iso-date (date)
  (multiple-value-bind (year month day hour minute second millisecond)
      (decode-timestamp date)
    (declare (ignore millisecond))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun template (template-name)
  (parse-template-string (read-file-into-string (find-template template-name))))

(defun format-article (article-params)
  (let ((article-template (template "article")))
    (expand-template article-template article-params)))

(defun paramify-article-data (revision-data &optional (comments nil commentary-p))
  (destructuring-bind (rid article date title content author format status
                       global-id
                       &optional publishing-date comment-num
                       &rest args)
      revision-data
    (declare (ignore args format author))
    (list :publishing-date publishing-date
          :revision rid
          :last-updated-date date
          :title title
          :body content
          :article-id article
          :global-id global-id
          :status status
          :link (link-to :view :article-id article)
          :commentary (if commentary-p (list :comments comments) nil)
          :comment-submission (when commentary-p
                                (list :fields (list (list :field-id "name"
                                                          :field-label "Name"))
                                      :body-label "Message"
                                      :submit-button-label "Submit"
                                      :title "Submit a comment"
                                      :notes "<p>NOTE: Do something.</p>"
                                      :action (link-to :post-comment :article-id article)))
          :edit-link (link-to :edit :article-id article)
          :edit-button-label "Edit"
          :comment-feed (link-to :view-comment-feed :article-id article)
          :comment-feed-label "Comment feed"
          :comments-label (case comment-num
                            (0 "no comments")
                            (1 "1 comment")
                            (otherwise (format nil "~D comments" comment-num)))
          :comments-link (link-to :view-comments :article-id article)
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
            "SELECT (most_recent_revision(r)).*, d.publishing_date, c.comment_count
               FROM (SELECT article_revisions_for_characteristics(a.id, ~A)
                            AS revision
                       FROM articles a)
                    AS mr
               JOIN article_revisions        r ON r.id = mr.revision
               JOIN article_publishing_dates d ON d.article = r.article
               JOIN article_comment_counts   c ON c.article = r.article
              GROUP BY r.article, d.publishing_date, c.comment_count
             HAVING ~A
              ORDER BY d.publishing_date DESC"
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
    (declare (ignore args crid status format))
    (destructuring-bind (author-name author-website)
        (query "SELECT name, website FROM users WHERE id = $1" author :row)
      (let ((article (query "SELECT article FROM article_revisions WHERE id = $1"
                            article-revision
                            :single!)))
        (list :publishing-date date
              :body (format-comment-content content)
              :author author-name
              ;;FIXME
              :author-link (if (and author-website (not (equal author-website "")))
                               author-website
                               nil)
              :link (link-to :view :article-id article :comment-id comment)
              :edit-link ""
              :edit-button-label "Edit"
              :generic-commenter-name "Someone")))))

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
                                      :full-archive-label "Full archive (slow!)"
                                      :archive-title "Older posts"
                                      :archive-table-caption "Posts by date"
                                      :archive-title-label "Title"
                                      :archive-date-label "Date"
                                      :archive-comments-label "Comments"))
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

(defun paramify-article (revision-data &optional commentary-p comments)
  (let* ()
    (cond ((null revision-data)
           nil)
          (commentary-p
           (paramify-article-data revision-data comments))
          (t
           (paramify-article-data revision-data)))))

(defun find-article-params (article characteristics &optional commentary-p)
  (let* ((comment-data (if commentary-p
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
         (comment-num (length comments))  ;FIXME
         (revisions (find-article-revisions article characteristics))
         (revision (first revisions))
         (revision-data (query "SELECT *
                                  FROM article_revisions
                                 WHERE id = $1"
                               revision
                               :row))
         (publishing-date (query "SELECT min(date)
                                    FROM article_revisions
                                   WHERE article = $1
                                     AND status IN ('published', 'syndicated')"
                                 article
                                 :single)))
    (cond ((null revision-data)
           nil)
          (t
           (paramify-article (append revision-data (list publishing-date
                                                         comment-num))
                             commentary-p
                             comments)))))

(defun expand-page (page-template title template-params)
  (let ((page-skeleton (template "page_skeleton"))
        (head (expand-template page-template
                               (list* :head t
                                      template-params)))
        (body (expand-template page-template
                               (list* :body t
                                      template-params))))
    (expand-template page-skeleton
                     (list :title title
                           :head head
                           :body body))))

(defun find-article-request-handler (path params &optional action characteristics)
  (with-db
    (when-let ((article (query "SELECT article FROM article_aliases
                                 WHERE alias = $1"
                               path
                               :single)))
      (ecase action
        (:edit
         (lambda ()
           (with-db
             (with-transaction ()
               (let* ((revision (if (assoc "save" params :test #'equal)
                                    (query "INSERT INTO article_revisions(article, title, content, author, format, status)
                                               VALUES ($1, $2, $3, $4, $5, $6)
                                            RETURNING *"
                                           article
                                           (cdr (assoc "title" params :test #'equal))
                                           (cdr (assoc "content" params :test #'equal))
                                           1 ;FIXME
                                           "html"
                                           (if (hunchentoot:post-parameter "publish-p")
                                               "syndicated"
                                               "draft")
                                           :row)
                                    (query "SELECT * FROM article_revisions
                                                  WHERE id = $1
                                                    AND article = $2"
                                           (parse-integer
                                            (cdr (assoc "revision"
                                                        params
                                                        :test #'equal)))
                                           article
                                           :row)))
                      (article-params (paramify-article revision))
                      (editor-template (template "edit_page")))
                 (assert (not (null revision)))
                 (when (assoc "save" params :test #'equal)
                   (print (parse-integer (cdr (assoc "revision"
                                                     params
                                                     :test #'equal))))
                   (query "INSERT INTO article_revision_parenthood(parent, child)
                                VALUES ($1, $2)"
                          (parse-integer (cdr (assoc "revision"
                                                     params
                                                     :test #'equal)))
                          (first revision)
                          :none)
                   (query "INSERT INTO article_revision_characteristics(revision, characteristic, value)
                                SELECT $2, characteristic, value
                                  FROM article_revision_characteristics
                                 WHERE revision = $1"
                          (parse-integer (cdr (assoc "revision"
                                                     params
                                                     :test #'equal)))
                          (first revision)
                          :none))
                 (expand-page editor-template
                              (getf article-params :title)
                              (list :article article-params
                                    :title (getf article-params :title)
                                    :root *base-uri*
                                    :site-name *site-name*
                                    :site-subtitle ""
                                    :link (link-to :edit :article-id article)
                                    :save-button-label "Save"
                                    :publish-flag-label "Publish"
                                    :title-label "Title"
                                    :content-label "Content")))))))
        (:view
         (lambda ()
           (with-db
             (let* ((page-template-name (query "SELECT page_template FROM articles
                                               JOIN article_types
                                                 ON articles.type = article_types.id
                                              WHERE articles.id = $1"
                                               article
                                               :single!))
                    (article-params (find-article-params article characteristics t))
                    (page-template (template page-template-name))
                    (template-params (list :title (getf article-params :title)
                                           :root *base-uri*
                                           :site-name *site-name*
                                           :site-subtitle ""
                                           :link "")))
               (expand-page page-template
                            (getf article-params :title)
                            (list* :articles (list article-params)
                                   template-params))))))))))



(defun keywordify (thing)
  (intern (string-upcase (format nil "~A" thing)) "KEYWORD"))


(defun find-request-handler (path params)
  (or (find-journal-archive-request-handler
       path
       (assoc "full" params :test #'equal)
       (cond ((assoc "feed" params :test #'equal) :view-feed)
             (t                                   :view)))
      (find-article-request-handler
       path
       params
       (cond ((assoc "edit"         params :test #'equal) :edit)
             ((assoc "comment-feed" params :test #'equal) :view-comment-feed)
             ((assoc "atom"         params :test #'equal) :view-atom-entry)
             (t                                           :view)))))
