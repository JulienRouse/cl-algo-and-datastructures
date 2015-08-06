(in-package :cl-user)
(defpackage static-set
  (:use :cl))
(in-package :static-set)

;; lets try the CLOS


(defclass <static-set> ()
  ((table 
    :initform (make-hash-table :test 'equal)
    :reader table
    :documentation "The hash-table holding the elements")
   (count 
    :initform 0
    :reader elt-count
    :documentation "The number of element in the set")))

;;utility function, probably defined in alexandria
(defun hash-keys (hash-table)
  "Return a list of the key of the table"
  (loop for key being the hash-keys of hash-table collect key))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))
;;ref https://en.wikipedia.org/wiki/Set_%28abstract_data_type%29

(defgeneric build (<set> &rest rest)
  (:documentation "Build a set with the parameter specified
Throw an error if the set was already built"))

(defgeneric emptyp (<set>)
  (:documentation "Return true if the set doesnt contain any element, nil otherwise"))

(defgeneric elementp (<set> elt)
  (:documentation "Return true if elt is in the specified set, nil otherwise"))

(defgeneric size (<set>)
  (:documentation "Return the number of element in the set"))

(defgeneric enumerate (<set>)
  (:documentation "Return a list containing the elements of the specified set, in no particular order"))

(defgeneric build-from (<set> sequence)
  (:documentation "Build a set from a specified sequence")) 

;;

(defmethod size ((<set> <static-set>))
  (elt-count <set>))

(defmethod emptyp ((<set> <static-set>) )
  (eql (size <set>) 0))

(defmethod elementp ((<set> <static-set>) elt)
  (eql (gethash elt (slot-value <set> 'table)) t))

(defmethod build ((<set> <static-set>) &rest rest )
  (if (not (emptyp <set>))
      (princ "A set is already built")
      (map 'list 
	   #'(lambda (x)
	       (incf (slot-value <set> 'count) 1)
	       (setf (gethash x (slot-value <set> 'table)) t))
	   (flatten rest)))))
		 
(defmethod enumerate ((<set> <static-set>))
  (hash-keys (table <set>)))

(defmethod build-from ((<set> <static-set>) sequence)
  (if (typep sequence 'sequence)
      (build <set> (map 'list #'identity sequence))
      (print "build-from needs a sequence as 2nd argument")))
