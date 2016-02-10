#|
  This file is a part of clep project.
|#

(in-package :cl-user)
(defpackage clep-test-asd
  (:use :cl :asdf))
(in-package :clep-test-asd)

(defsystem clep-test
  :depends-on (:clep
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clep"))))
  :description "Test system for clep"
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
