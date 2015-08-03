#|
  This file is a part of cl-algo-and-datastructures project.
  Copyright (c) 2015 Julien Rousé (julien.rouse@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-algo-and-datastructures-test-asd
  (:use :cl :asdf))
(in-package :cl-algo-and-datastructures-test-asd)

(defsystem cl-algo-and-datastructures-test
  :author "Julien Rousé"
  :license "WTFPL"
  :depends-on (:cl-algo-and-datastructures
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-algo-and-datastructures"))))
  :description "Test system for cl-algo-and-datastructures"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
