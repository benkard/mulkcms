(in-package #:mulkcms-hunchentoot)

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

(defun dispatch-mulkcms-request (request)
  (let* ((relative-path (subseq (script-name request) 1)))
    (mulkcms::find-request-handler relative-path (append (get-parameters*) (post-parameters*)))))

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
  (hunchentoot:start (make-instance 'hunchentoot:acceptor
                        :port *server-port*
                        :address *server-address*)))
