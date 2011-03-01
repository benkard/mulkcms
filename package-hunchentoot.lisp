(cl:defpackage #:mulkcms-hunchentoot
  (:use #:common-lisp #:hunchentoot #:cl-who #:cl-ppcre #:alexandria #:cl-fad
        #:mulkcms)
  (:shadow #:copy-file #:copy-stream))
