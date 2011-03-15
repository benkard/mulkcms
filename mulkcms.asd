;;;; -*- mode: lisp; coding: utf-8 -*-
;;; Copyright 2011, Matthias Andreas Benkard.


(asdf:defsystem mulkcms
  :author "Matthias Andreas Benkard"
  :description ""
  :license "Affero GPL 3.0"
  :serial t
  :version "0.0.1"
  :depends-on (:cl-who :cl-json :alexandria :postmodern :split-sequence
               :cl-ppcre :cl-fad :cxml :closure-html :json-template :cxml-stp
               :ironclad :flexi-streams :drakma :puri)
  :components ((:file "package")
               (:file "site")
               (:file "mulkcms")
               (:file "lingva")))
