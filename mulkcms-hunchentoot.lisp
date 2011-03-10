(in-package #:mulkcms-hunchentoot)

(define-easy-handler handle-admin-request (action)
  ;; XXX
  )

(defun dispatch-static-file-request (request)
  ;; FIXME Can use paths like "/../mulkcms.lisp" or "//boot/initrd.img".
  ;; That's bad.
  (let* ((relative-path (subseq (script-name request) 1))
         (file (merge-pathnames relative-path *static-files*)))
    (and (probe-file file) (lambda () (handle-static-file file)))))

(defun dispatch-mulkcms-request (request)
  (let* ((relative-path (subseq (script-name request) 1)))
    (mulkcms::find-mulkcms-request-handler relative-path)))

(defun setup-handlers ()
  (setq *dispatch-table*
        (list* 'dispatch-static-file-request
               (create-prefix-dispatcher "/admin" 'handle-admin-request)
               'dispatch-mulkcms-request
               *dispatch-table*))
  (setq *default-handler* 'handle-mulkcms-request))

(defun start-server ()
  (setq hunchentoot:*hunchentoot-default-external-format*
        (flexi-streams:make-external-format :utf-8))
  (setup-handlers)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor
                        :port *server-port*
                        :address *server-address*)))
