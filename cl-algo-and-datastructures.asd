#|
  This file is a part of cl-algo-and-datastructures project.
  Copyright (c) 2015 Julien Rousé (julien.rouse@gmail.com)
|#

#|
  Little collection of algorithms and data structures

  Author: Julien Rousé (julien.rouse@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-algo-and-datastructures-asd
  (:use :cl :asdf))
(in-package :cl-algo-and-datastructures-asd)

(defsystem cl-algo-and-datastructures
  :version "0.1"
  :author "Julien Rousé"
  :license "WTFPL"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-algo-and-datastructures"))))
  :description "Little collection of algorithms and data structures"
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
  :in-order-to ((test-op (test-op cl-algo-and-datastructures-test))))
