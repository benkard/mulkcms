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


(defun make-uuid ()
  ;; Taken from Mulkblog.
  "Generate a version 4 UUID according to RFC 4122, section 4.4."
  (format nil "~(~8,'0x-~4,'0x-~4,'0x-~2,'0x~2,'0x-~12,'0x~)"
          (random #x100000000)                ;time_low
          (random #x10000)                    ;time_mid
          (logior #b0100000000000000
                  (logand #b0000111111111111
                          (random #x10000)))  ;time_hi_and_version
          (logior #b10000000
                  (logand #b00111111
                          (random #x100)))    ;clock_seq_hi_and_reserved
          (random #x100)                      ;clock_seq_low
          (random #x1000000000000)))


(defun hashcash-hash-validp (text)
  (let* ((stripped-text (ppcre:regex-replace-all "\\s+" text ""))
         (digest (ironclad:digest-sequence 'ironclad:sha256 (flexi-streams:string-to-octets stripped-text :external-format :utf8))))
    (every #'zerop (subseq digest 0 2))))


(defun akismet-login ()
  ;; Taken from Mulkblog.
  (drakma:http-request "http://rest.akismet.com/1.1/verify-key"
                       :protocol :http/1.0
                       :method :post
                       :user-agent "MulkCMS/0.1.0"
                       :parameters `(("key" . ,*wordpress-key*)
                                     ("blog" . ,*base-uri*))))


(defun akismet-check-comment (body author-name author-website user-agent submitter-ip)
  ;; Taken from Mulkblog.
  (drakma:http-request
   (format nil
           "http://~A.rest.akismet.com/1.1/comment-check"
           *wordpress-key*)
   :protocol :http/1.0
   :method :post
   :user-agent "Mulk Journal/0.0.1"
   :parameters `(("blog" . ,*base-uri*)
                 ("user_ip" . ,submitter-ip)
                 ("user_agent" . ,user-agent)
                 ("comment_type" . "comment")
                 ("comment_author" . ,author-name)
                 ("comment_author_url" . ,author-website)
                 ("comment_content" . ,body))))


(defun use-akismet-p ()
  (and (boundp '*wordpress-key*) *wordpress-key* t))

(defun spamp/akismet (&rest comment-data)
  ;; Taken from Mulkblog.
  (when (use-akismet-p)
    (ignore-errors
      (akismet-login)
      (string= "true" (apply #'akismet-check-comment comment-data)))))


(defun requested-characteristics ()
  nil)


(defun call-with-cache (path last-update thunk)
  (let* ((chars       (requested-characteristics))
         (charstring  (prin1-to-string chars))
         (charbytes   (flexi-streams:string-to-octets
                       charstring
                       :external-format :utf-8))
         (charhash    (ironclad:digest-sequence 'ironclad:crc32 charbytes))
         (charhashnum (- #x80000000
                         (logior (ash (elt charhash 0) 24)
                                 (ash (elt charhash 1) 16)
                                 (ash (elt charhash 2)  8)
                                 (ash (elt charhash 3)  0))))
         (cached-data (query "SELECT content, date
                                FROM cached_pages
                               WHERE characteristic_hash = $1
                                 AND alias = $2"
                              charhashnum
                              path
                              :row)))
    (if (and cached-data (simple-date:time< last-update (second cached-data)))
        (first cached-data)
        (let ((generated-content (funcall thunk)))
          (query "INSERT INTO cached_pages(characteristic_hash, alias, content)
                    VALUES ($1, $2, $3)"
                 charhashnum
                 path
                 generated-content
                 :none)
          generated-content))))


(defmacro with-cache ((path last-update) &body body)
  `(call-with-cache ,path ,last-update (lambda () ,@body)))


(defun find-canonical-article-alias (article)
  (query "SELECT alias FROM article_aliases WHERE article = $1 LIMIT 1"
         article
         :single))


(defun link-to (action &key comment-id article-id revision-id (absolute nil))
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
          ((:edit :preview) (cond (revision-id (values "/~A?edit&revision=~D" article-base revision-id))
                                  (t           (values "/~A?edit" article-base))))
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
                                                          :field-label "Name")
                                                    (list :field-id "website"
                                                          :field-label "Website")
                                                    (list :field-id "email"
                                                          :field-label "E-Mail"))
                                      :body-label "Message"
                                      :submit-button-label "Submit"
                                      :title "Submit a comment"
                                      :notes (format
                                              nil
                                              "<p><strong>Note:</strong>
                                              <span
                                              class='spam-detection-info'>~A</span>
                                              Comment format is plain
                                              text.  Use blank lines to
                                              separate paragraphs.</p>"
                                              (if
                                               (use-akismet-p)
                                               "This website uses <span
                                                class='spam-detection-method'><a
                                                href=\"http://akismet.com/\">Akismet</a></span>
                                                for spam detection.
                                                E-mail addresses are
                                                never disclosed to
                                                anyone (including
                                                Akismet) other than the
                                                site owner."
                                               ""))
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
                       article-revision submitter-ip user-agent
                       &rest args)
      comment-revision-data
    (declare (ignore args crid status format submitter-ip user-agent))
    (destructuring-bind (author-name author-website)
        (query "SELECT name, website FROM users WHERE id = $1"
               author
               :row)
      (let ((article (query "SELECT article
                               FROM article_revisions
                              WHERE id = $1"
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
  (when (member path '("journal" "journal/"
                       "feed" "feed/"
                       "journal/feed" "journal/feed")
                :test #'string=)
    (lambda ()
      (with-db
        (with-cache (path (query "SELECT max(date) FROM article_revisions" :single))
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
                 (displayed-revisions (if full-p revisions (subseq revisions 0 10))))
            (cond
              ((member path '("journal" "journal/") :test #'string=)
               (let* ((page-skeleton (template "page_skeleton"))
                      (page-template (template "journal_page"))
                      (template-params
                       (list :title *site-name*
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
                      (head (expand-template
                             page-template
                             (list* :head t
                                    :articles displayed-revisions
                                    :minor-articles revisions
                                    template-params)))
                      (body (expand-template
                             page-template
                             (list* :body t
                                    :articles displayed-revisions
                                    :minor-articles revisions
                                    template-params))))
                 (expand-template page-skeleton (list :title *site-name*
                                                      :head head
                                                      :body body))))
              ((member path '("feed" "feed/" "journal/feed" "journal/feed/")
                       :test #'string=)
               (let* ((authors
                       (query "SELECT DISTINCT name
                               FROM users
                               JOIN article_revisions
                                 ON author = users.id"
                              :plist))
                      (last-updated
                       (query "SELECT max(date)
                               FROM article_revisions
                              WHERE status = 'syndicated'"
                              :single))
                      (template-params
                       (list :title *site-name*
                             :last-updated-date last-updated
                             :base-uri *base-uri*
                             :subtitle ""
                             :global-id *feed-global-id*
                             :authors authors
                             :feed-uri (link-to :view-atom-feed)
                             :articles revisions)))
                 (expand-template (template "article_feed")
                                  template-params))))))))))

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

(defmacro with-authorization ((user-id-var &rest options) &body body)
  `(call-with-authorization (lambda (,user-id-var) ,@body)
                            ,@options))

(defun call-with-authorization (thunk &key require)
  (multiple-value-bind (user-name password)
      (hunchentoot:authorization)
    (with-db
      (let ((user-id (query (format nil
                                    "SELECT id
                                       FROM users u
                                       JOIN passwords p ON u.id = p.user
                                      WHERE p.password = $2
                                        AND u.name = $1
                                        AND ~A"
                                    (ecase require
                                      ((nil) "true")
                                      ((:admin) "u.status = 'admin'")
                                      ((:trusted) "u.status IN ('trusted', 'admin')")
                                      ((:approved) "u.status IN ('approved', 'trusted', 'admin')")))
                            user-name
                            password
                            :single)))
        (if user-id
            (funcall thunk user-id)
            (hunchentoot:require-authorization "MulkCMS"))))))

(defun parse-row-or-array (string)
  (let ((output (list)))
    (with-input-from-string (in string)
      (read-char in)
      (loop for char = (read-char in nil nil nil)
            while (and char (not (member char (list #\} #\)))))
            do (push (with-output-to-string (out)
                       (if (char= char #\")
                           (loop for c = (read-char in)
                                 if (char= c #\\)
                                   do (format out "~C" (read-char in))
                                 else if (member c (list #\"))
                                   do (if (char= (peek-char nil in) #\")
                                          (progn
                                            (format out "\"")
                                            (read-char in))
                                          (progn
                                            (read-char in)
                                            (return)))
                                 else 
                                   do (format out "~C" c))
                           (progn
                             (format out "~C" char)
                             (loop for c = (read-char in)
                                   until (member c (list #\) #\, #\}))
                                   do (format out "~C" c)))))
                     output)))
    (nreverse output)))

(defun parse-row (string)
  (assert (and (char= (char string 0)                    #\()
               (char= (char string (1- (length string))) #\))))
  (parse-row-or-array string))

(defun parse-array (string)
  (assert (and (char= (char string 0)                    #\{)
               (char= (char string (1- (length string))) #\})))
  (parse-row-or-array string))

(defun find-article-summary-handler (path params &optional action characteristics)
  (declare (ignore characteristics action))
  (when (string= path "admin/articles")
    (lambda ()
      (with-authorization (user-id :require :admin)
        (with-db
          (labels ((paramify-revision-row (row article-id)
                     (destructuring-bind (id title date characteristics)
                         row
                       (list :id id
                             :title title
                             :date date
                             :link (link-to :edit :article-id article-id :revision-id id)
                             :characteristics (parse-array characteristics))))
                   (paramify-article-row (row)
                     (destructuring-bind (id revisions aliases)
                         row
                       (let ((revision-data (mapcar (lambda (x)
                                                      (paramify-revision-row x id))
                                                    (mapcar #'parse-row
                                                            (parse-array revisions)))))
                         (list :id id
                               :aliases aliases
                               :revisions revision-data
                               :revision-num (length revision-data))))))
            (when (assoc "add-alias" params :test #'equal)
              (with-transaction ()
                (query "INSERT INTO article_aliases(article, alias) VALUES ($1, $2)"
                       (parse-integer (cdr (assoc "article" params :test #'equal)))
                       (cdr (assoc "alias" params :test #'equal))
                       :none)))
            (when (assoc "create-article" params :test #'equal)
              (with-transaction ()
                (let ((article-id (query "INSERT INTO articles(type) VALUES (1) RETURNING id"
                                         :single!)))
                  (query "INSERT INTO article_revisions(article, title, content, author, format, status, global_id)
                               VALUES ($1, '', '', $2, 'html', 'draft', $3)"
                         article-id
                         user-id
                         (format nil "urn:uuid:~A" (make-uuid))))))
            (let* ((articles (query "SELECT a.id,
                                            array_agg(DISTINCT
                                                      ROW(r.id,
                                                          r.title,
                                                          r.date,
                                                          r.characteristics)),
                                            array_agg(DISTINCT aa.alias)
                                       FROM articles a
                                       LEFT OUTER JOIN article_aliases aa
                                         ON aa.article = a.id
                                       LEFT OUTER JOIN article_branch_tips bt
                                         ON bt.article = a.id
                                       LEFT OUTER JOIN
                                         (SELECT r.id    AS id,
                                                 r.title AS title,
                                                 r.date  AS date,
                                                 array_agg(DISTINCT
                                                           ROW(ch.characteristic, 
                                                               ch.value))
                                                   AS characteristics
                                            FROM article_revisions r
                                            LEFT OUTER JOIN article_revision_characteristics ch
                                              ON ch.revision = r.id
                                           GROUP BY r.id, r.title, r.date)
                                         AS r
                                         ON bt.revision = r.id
                                       LEFT OUTER JOIN article_publishing_dates pd
                                         ON pd.article = a.id
                                      GROUP BY a.id, pd.publishing_date
                                      ORDER BY pd.publishing_date DESC"
                                    :rows))
                   (article-data (mapcar #'paramify-article-row articles)))
              (expand-page (template "article_summary_page")
                           "Articles"
                           (list :id-label "Article"
                                 :aliases-label "Aliases"
                                 :branch-label "Branch"
                                 :branch-title-label "Title"
                                 :characteristics-label "Characteristics"
                                 :date-label "Date"
                                 :create-button-label "Add article"
                                 :add-alias-label "+"
                                 :articles article-data)))))))))

(defun find-article-request-handler (path params &optional action characteristics)
  (with-db
    (when-let ((article (query "SELECT article FROM article_aliases
                                 WHERE alias = $1"
                               path
                               :single)))
      (ecase action
        (:edit
         (lambda ()
           (with-authorization (user-id :require :admin)
             (with-db
               (with-transaction ()
                 (let* ((revision (if (assoc "save" params :test #'equal)
                                      (query "INSERT INTO article_revisions(article, title, content, author, format, status)
                                                 VALUES ($1, $2, $3, $4, $5, $6)
                                              RETURNING *"
                                             article
                                             (cdr (assoc "title" params :test #'equal))
                                             (cdr (assoc "content" params :test #'equal))
                                             user-id
                                             "html"
                                             (if (assoc "publish-p"
                                                        params
                                                        :test #'equal)
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
                                      :content-label "Content"))))))))
        (:view
         (lambda ()
           (with-db
             (let* ((page-template-name (query "SELECT page_template FROM articles
                                                  JOIN article_types
                                                    ON articles.type = article_types.id
                                                 WHERE articles.id = $1"
                                               article
                                               :single!))
                    (article-params (find-article-params article
                                                         characteristics
                                                         t))
                    (page-template (template page-template-name))
                    (template-params (list :title (getf article-params :title)
                                           :root *base-uri*
                                           :site-name *site-name*
                                           :site-subtitle ""
                                           :link ""))
                    (submission-notice nil))
               (when (assoc "post-comment" params :test #'equal)
                 (let* ((name     (cdr (assoc "name"            params :test #'equal)))
                        (website  (cdr (assoc "website"         params :test #'equal)))
                        (email    (cdr (assoc "email"           params :test #'equal)))
                        (body     (cdr (assoc "body"            params :test #'equal)))
                        (article  (cdr (assoc "article"         params :test #'equal)))
                        (revision (cdr (assoc "revision"        params :test #'equal)))
                        (tkey     (cdr (assoc "transaction-key" params :test #'equal)))
                        (salt     (cdr (assoc "salt"            params :test #'equal)))
                        (spam-p   (if tkey
                                      (or (null salt)
                                          (not (hashcash-hash-validp
                                                (format nil "~A:~A:~A" body tkey salt))))
                                      (spamp/akismet body name website
                                                     (hunchentoot:real-remote-addr)
                                                     (hunchentoot:user-agent)))))
                   (with-transaction ()
                     (let ((comment (query "INSERT INTO comments(article, global_id)
                                              VALUES ($1, $2)
                                            RETURNING id"
                                           article
                                           (format nil "urn:uuid:~A" (make-uuid))
                                           :single!))
                           (author  (query "INSERT INTO users(name, status, email, website)
                                              VALUES ($1, 'visitor', $2, $3)
                                            RETURNING id"
                                           name
                                           email
                                           website
                                           :single!)))
                       (when tkey
                         (query "INSERT INTO used_transaction_keys(key) VALUES ($1)"
                                tkey
                                :none))
                       (query "INSERT INTO comment_revisions(comment, content, author, format, status, article_revision, submitter_ip, submitter_user_agent)
                                 VALUES ($1, $2, $3, 'text', $4, $5, $6, $7)"
                              comment
                              body
                              author
                              (if spam-p
                                  "pending"
                                  "spam")
                              revision
                              (hunchentoot:real-remote-addr)
                              (hunchentoot:user-agent)
                              :none)
                       (setq submission-notice
                             (cond
                               (spam-p
                                (list
                                 :content "<p><strong>Warning:</strong></p>

                                           <p>Your message could not be
                                           verified as non-spam.  If
                                           JavaScript is enabled in your
                                           browser, it may be broken in
                                           some way.  In this case,
                                           please disable JavaScript
                                           support and try again.
                                           Otherwise, feel free to
                                           contact one of the site
                                           administrators, who will be
                                           able to manually approve your
                                           comment.</p>"
                                 :message-type "warning"))
                               (t
                                (list
                                 :content "<p><strong>Note:</strong></p>

                                           <p>Your message has been received and
                                           classified as non-spam.  It has thus
                                           been put into the moderation queue and
                                           is now awaiting approval by one of the
                                           site's administrators.</p>"
                                 :message-type "success-message"))))))))
               (expand-page page-template
                            (getf article-params :title)
                            (list* :articles (list article-params)
                                   :info-messages (if submission-notice
                                                      (list submission-notice)
                                                      nil)
                                   template-params))))))))))



(defun find-transaction-key-handler (path)
  (when (string= path "RPC/generate-transaction-key")
    (lambda ()
      (with-db
        (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
        (format nil "~D" (query "SELECT nextval('transaction_key_seq')" :single!))))))


(defun keywordify (thing)
  (intern (string-upcase (format nil "~A" thing)) "KEYWORD"))


(defun find-request-handler (path params)
  (or (find-article-summary-handler
       path
       params)
      (find-journal-archive-request-handler
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
             (t                                           :view)))
      (find-transaction-key-handler path)))
