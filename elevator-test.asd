#|
  This file is a part of elevator project.
  Copyright (c) 2015 Julien Rousé (julien.rouse@gmail.com)
|#

(in-package :cl-user)
(defpackage elevator-test-asd
  (:use :cl :asdf))
(in-package :elevator-test-asd)

(defsystem elevator-test
  :author "Julien Rousé"
  :license "WTFPL"
  :depends-on (:elevator
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "elevator"))))
  :description "Test system for elevator"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
