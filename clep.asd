(in-package :cl-user)
(defpackage clep-asd
  (:use :cl :asdf))
(in-package :clep-asd)

(defsystem clep
  :depends-on (:uiop)
  :components ((:module "src"
                :components
                ((:file "clep"))))
  :in-order-to ((test-op (test-op clep-test))))
