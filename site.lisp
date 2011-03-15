;;;; -*- mode: lisp; coding: utf-8 -*-

(in-package #:mulkcms)

(defparameter *site-name* "MulkCMS Example Site Beta for Web 2.0")

(defparameter *database-host* "127.0.0.1")
(defparameter *database-name* "mulkcms")
(defparameter *database-user* "mulkcms")
(defparameter *database-password* "")

;; Set the following to your feed ID.  This may, for example, be
;; something like "urn:uuid:00000000-0000-0000-0000-000000000000" where
;; the dummy zero UUID has been replaced with a unique UUID for your
;; specific website.
(defparameter *feed-global-id* "")

(defparameter *static-files* "/var/mulkcms/static-files/")
(defparameter *templates* "/var/mulkcms/templates/")

(defparameter *base-uri* "http://127.0.0.1:9201/")

(defparameter *server-address* "127.0.0.1")
(defparameter *server-port* 9201)

;; Set the following to your Wordpress key if you want to use Akismet.
;; Otherwise, set it to NIL or comment the line out.
(defparameter *wordpress-key* nil)
