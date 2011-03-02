(in-package #:mulkcms)

(defparameter *database-connection-spec*
  (list *database-name* *database-user* *database-password* *database-host*))

(unless (member "html" *template-formatters* :key #'car :test #'equal)
  (push `("html" . ,(lambda (x) (cl-who:escape-string (string x))))
        *template-filters*)
  (push `("html-attr-value" . ,(lambda (x) (cl-who:escape-string (string x))))
        *template-filters*))

