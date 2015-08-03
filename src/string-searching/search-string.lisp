(in-package :cl-user)
(defpackage search-string
  (:nicknames ss)
  (:use :cl)
  (:export :algo-naive))
(in-package :search-string)

;; naive
(defun algo-naive (haystack needle)
  "Naive algorithm for searching a string in another string."
  (let ((res nil)
	(size-needle (length needle))
	(size-haystack (length haystack)))
    (dotimes (i (1+ (- size-haystack size-needle)))
      (when
	  (string= haystack 
		   needle 
		   :start1 i 
		   :end1 (+ i size-needle))
	(push i res)))
    (reverse res)))

;; rabin-karp
#|
(defun hash-string (word &key (a 48271) (n (1- (expt 2 31))) )
  "Hash a string. Auxiliary function to roll-hash-string"
  (let ((res 0))
    (dotimes (i (length word))
      (incf res 
	    (mod
	     (*
	      (char-code (aref word i))              ;value in ascii of the i-th
	      (expt a (- (length word) (1+ i))))   ;char of word
	     n)))
      (setf res (mod res n))
      res))

(defun make-rolling-hash (bound)
  "following http://www.mit.edu/~kevinz/6.006/rolling-hashes.pdf,
usage: (defvar rh (make-rolling-hash <bound>))
       (funcall rh rolling-hash-value)
       (funcall rh rolling-hash-append (char-code <character>))
       (funcall rh rolling-hash-shift (char-code <character>))"
  (let ((h 0)
	(n 0)
	(b bound)
	(e 1)
	(p 27644437)) ;;change it to a prime number between 2^62 and 2^63
    #'(lambda (operation &optional (elt -1))
	(ecase operation
	  (rolling-hash-value
	   h)
	  (rolling-hash-append
	   (progn
	     (when (< elt 0)
	       (print "error, need another parameter"))
	     (let* ((eprime (mod (* e b) p))
		    (eltprime elt) 
		    (hprime (mod (+ h (* eltprime eprime)) p)))
	       (setf e eprime)
	       (setf h hprime)
	       (setf n (incf n 1))
	       (list h n b e))))
	  (rolling-hash-shift
	   (progn
	     (let* ((hprime (mod (/ (- h elt) b) p))
		    (eprime (/ b e)))
	       (setf h hprime)
	       (setf e eprime)
	       (setf n (decf n 1))
	       (list h n b e))))))))


(defun new-hash (rolling-hash text)
  "Hash the text.
Return the hash value"
  (loop for e across text do
       (funcall rolling-hash 'rolling-hash-append (char-code e)))
  (funcall rolling-hash 'rolling-hash-value))

(defun roll-hash (rolling-hash old new)
  "Roll the hash so that the old char is replaced
by the new and the hash changes accordingly.
Return the hash value"     
  (funcall rolling-hash 'rolling-hash-shift old)
  (funcall rolling-hash 'rolling-hash-append new)
  (funcall rolling-hash 'rolling-hash-value))

|#


(defun algo-rabin-karp (haystack  needle  &optional (bound 256))
  (let* ((size-haystack (length haystack))
	 (size-needle   (length needle))
	 (rh-needle     (make-rolling-hash bound))
	 (rh-haystack   (make-rolling-hash bound))
	 (needle-hash   (new-hash rh-needle needle))
	 (haystack-hash (new-hash rh-haystack (subseq haystack 0 (length needle))))
	 (res nil))
    (when (and (eql needle-hash haystack-hash) (string= needle haystack :start2 0 :end2 size-needle))
      (push 0 res)
    (loop for i from size-needle to (1+ (- size-haystack size-needle))  
       do 
	 (setf haystack-hash 
	       (roll-hash rh-haystack 
			  (char-code (aref haystack (- i size-needle))) 
			  (char-code (aref haystack i))))
	 (when (and (eql needle-hash haystack-hash) 
		    (string= needle 
			     haystack 
			     :start2 (- i size-needle)
			     :end2   i))
	   (push i res)))
    (reverse res))))










		  

