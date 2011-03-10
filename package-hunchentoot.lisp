(cl:defpackage #:mulkcms-hunchentoot
  (:use #:common-lisp #:hunchentoot #:cl-who #:cl-ppcre #:alexandria #:cl-fad
        #:mulkcms)
  (:nicknames #:mulkcms-ht)
  (:shadow #:copy-file #:copy-stream))
