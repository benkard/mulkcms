(asdf:defsystem mulkcms-hunchentoot
  :serial t
  :version "0.0.1"
  :depends-on (:mulkcms :hunchentoot)
  :components ((:file "package-hunchentoot")
               (:file "mulkcms-hunchentoot")))
