(in-package #:mulkcms-hunchentoot)

(defvar *acceptor*)

(defun dispatch-static-file-request (request)
  (let* ((relative-path (subseq (script-name request) 1))
         (file (merge-pathnames relative-path *static-files*)))
    (and (probe-file file)
         ;; For security (otherwise paths like "/../mulkcms.lisp" or
         ;; "//boot/initrd.img" would be handled by sending the
         ;; requested file...):
         (starts-with-subseq (namestring (truename *static-files*))
                             (namestring (truename file)))
         (not (directory-pathname-p file))
         (lambda () (handle-static-file file)))))

(defun handle-authorization-page ()
  (hunchentoot:require-authorization "MulkCMS"))

(defun dispatch-mulkcms-request (request)
  (let* ((relative-path (subseq (script-name request) 1))
         (mulkcms::*use-ssl-p* (equal (header-in* :x-use-ssl)
                                      "true"))
         (mulkcms::*real-remote-addr*
          (hunchentoot:real-remote-addr))
         (mulkcms::*user-agent*
          (hunchentoot:user-agent))
         (mulkcms::*request-method*
          (hunchentoot:request-method*))
         (mulkcms::*headers*
          (hunchentoot::headers-in*)))
    (multiple-value-bind
          (mulkcms::*user-name* mulkcms::*password*)
        (hunchentoot:authorization)
      (let ((handler (mulkcms::find-request-handler relative-path
                                                    (append (get-parameters*)
                                                            (post-parameters*))
                                                    (header-in* :accept-language))))
        (and handler
             (lambda ()
               (let ((result (funcall handler)))
                 (typecase result
                   (cons
                    (when-let (content-type (getf result :content-type))
                      (setf (hunchentoot:content-type*) content-type))
                    (when-let (headers (getf result :headers))
                      (dolist (header headers)
                        (setf (hunchentoot:header-out (car header))
                              (cdr header))))
                    (when-let (return-code (getf result :return-code))
                      (setf (hunchentoot:return-code*) return-code)
                      ;;(hunchentoot:abort-request-handler)
                      )
                    (getf result :body))
                   (t
                    result)))))))))

(defun setup-handlers ()
  (setq *dispatch-table*
        (list* 'dispatch-mulkcms-request
               'dispatch-static-file-request
               *dispatch-table*))
  (setq *default-handler*
        (lambda ()
          (setf (return-code*) +http-not-found+))))

(defun start-server ()
  (setq hunchentoot:*hunchentoot-default-external-format*
        (flexi-streams:make-external-format :utf-8))
  (setq hunchentoot:*default-content-type*
        "text/html; charset=utf-8")
  (setup-handlers)
  (setq *random-state* (make-random-state t))
  (setq *acceptor* (make-instance 'hunchentoot:easy-acceptor
                      :port *server-port*
                      :address *server-address*))
  (setq mulkcms::*authorization-page-handler* #'handle-authorization-page)
  (hunchentoot:start *acceptor*)
  *acceptor*)

(defun run-server ()
  (let ((acceptor (start-server)))
    (unwind-protect
         (loop
           (sleep 86400))
      (stop acceptor :soft t))))
