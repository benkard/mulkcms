;;;; -*- mode: lisp; coding: utf-8 -*-
;;; Copyright 2011, Matthias Andreas Benkard.


(defsystem mulkcms
  :serial t
  :version "0.0.1"
  :depends-on (:cl-who :cl-json :alexandria :postmodern :split-sequence
               :cl-ppcre :cl-fad :cxml :closure-html :json-template)
  :components ((:file "site")
               (:file "package")
               (:file "mulkcms")))


(defsystem mulkcms-hunchentoot
  :serial t
  :version "0.0.1"
  :depends-on (:mulkcms :hunchentoot)
  :components ((:file "mulkcms-hunchentoot")))
