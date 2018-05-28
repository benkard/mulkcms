(asdf:defsystem mulkcms-ht
  :version "0.0.1"
  :depends-on (:mulkcms-hunchentoot)
  :components ()

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "mulkcms-ht"
  :entry-point "mulkcms-hunchentoot:run-server")
