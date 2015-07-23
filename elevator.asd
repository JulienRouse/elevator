#|
  This file is a part of elevator project.
  Copyright (c) 2015 Julien Rousé (julien.rouse@gmail.com)
|#

#|
  Author: Julien Rousé (julien.rouse@gmail.com)
|#

(in-package :cl-user)
(defpackage elevator-asd
  (:use :cl :asdf))
(in-package :elevator-asd)

(defsystem elevator
  :version "0.1"
  :author "Julien Rousé"
  :license "WTFPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "elevator"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op elevator-test))))
