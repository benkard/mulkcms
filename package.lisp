(cl:defpackage #:mulkcms
  (:use #:common-lisp #:split-sequence #:alexandria #:cl-fad #:cl-who
        #:cl-ppcre #:postmodern)
  (:shadow #:copy-file #:copy-stream))
