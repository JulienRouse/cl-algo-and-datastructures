(in-package :cl-user)
(defpackage search-string
  (:nicknames ss)
  (:use :cl)
  (:export :algo-naive
	   :algo-rabin-karp
	   :algo-kmp))
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

(defun hash-string (word &key (a 48271) (modulo 32416190071))
  "Hash a string. Auxiliary function to roll-hash-string"
  (let ((res 0))
    (dotimes (i (length word))
      (incf res 
	    (mod
	     (*
	     (char-code (aref word i))              
	     (mod (expt a (- (length word) (1+ i))) modulo))   
	    modulo)))
    (setq res (mod res modulo))))

(defun make-rolling-hash (&optional (base 27644437) (modulo 32416190071))
  "usage: (defvar rh (make-rolling-hash))
       (funcall rh rolling-hash-value)
       (funcall rh rolling-hash-new :text string)
       (funcall rh rolling-hash-roll :elt (char-code <character>))"
  (let ((h 0)
 	(n 0)
	(haystack ""))
	;(p prime)) ;;change it to a prime number between 2^62 and 2^63
    #'(lambda (operation &key (elt -1) (text ""))
	(ecase operation
	  (rolling-hash-value
	   h)
	  (rolling-hash-new
	   (progn
	     (setf h (hash-string text :a base :modulo modulo))
	     (setf n (incf n (1- (length text))))
	     (setf haystack  text )
	     (list h n haystack)))
	  (rolling-hash-roll
	   (progn
	     (let* ((oldLetter (mod 
				(* (char-code (aref haystack 0)) 
				   (expt base n)) 
				modulo)) 
		    (newHash (+ 
			      (* base (- h oldLetter)) 
			      elt)))  
	       (setf h (mod newHash modulo))
	       (setf haystack 
		     (format nil "~A~A" (subseq haystack 1) (code-char elt)))
	       (list h n haystack)))))))))


(defun new-hash (rolling-hash text)
  "Hash the text.
Return the hash value"
  (funcall rolling-hash 'rolling-hash-new :text text)
  (funcall rolling-hash 'rolling-hash-value))

(defun roll-hash (rolling-hash new)
  "Roll the hash so that the old char is replaced
by the new and the hash changes accordingly.
Return the hash value"     
  (funcall rolling-hash 'rolling-hash-roll :elt (char-code new))
  (funcall rolling-hash 'rolling-hash-value))



;;TODO use a set/bloom filter instead of a single word to search
(defun algo-rabin-karp (haystack  needle  &key (base 27644437) (modulo 32416190071))
  "Rabin-Karp implementation with a rolling hash"
  (let* ((size-haystack (length haystack))
	 (size-needle   (length needle))
	 (rh-needle     (make-rolling-hash base modulo))
	 (rh-haystack   (make-rolling-hash base modulo))
	 (needle-hash   (new-hash rh-needle needle))
	 (haystack-hash (new-hash rh-haystack (subseq haystack 0 (length needle))))
	 (res nil))
    (when (and (eql needle-hash haystack-hash) (string= needle haystack :start2 0 :end2 size-needle))
      (push 0 res))
    (loop for i from size-needle to (1- size-haystack)  
       do 
	 (setf haystack-hash 
	       (roll-hash rh-haystack  
			  (aref haystack i)))
	 (when (and (eql needle-hash haystack-hash) 
		    (string= needle 
			     haystack 
			     :start2 (- (1+ i) size-needle)
			     :end2   (1+ i)))
	   (push (- (1+ i) size-needle)  res)))
    (reverse res))))


;;Knuth-Morris-Pratt

(defun kmp-table (needle table)
  "Precomputing for the kmp algorithm
table:vector
needle:string
return void 
side-effect: modify table"
  (let ((pos 2)
	(cnd 0))
  (setf (aref table 0) -1)
  (setf (aref table 1) 0)
  (loop 
     while(< pos (length needle))
     do
       (cond
	 ((eql (elt needle (1- pos))
	       (elt needle cnd))
	  (incf cnd 1)
	  (setf (aref table pos) cnd)
	  (incf pos 1))
	 ((> cnd 0)
	  (setf cnd (aref table cnd)))
	 (t
	  (setf (aref table pos) 0)
	  (incf pos 1))))))
    



(defun algo-kmp (needle haystack)
  "
needle:string
haystack:string
return:list
side-effect:void"
  (let ((m 0)
	(i 0)
	(table (make-array (length needle)))
	(res nil))
    (kmp-table needle table)
    (loop
       while (< (+ m i) (length haystack))
       do
	 (cond
	   ((eql (elt needle i) (elt haystack (+ m i)))
	    (cond 
	      ((eql i (1- (length needle)))
	       (push m res)
	       (setf i 0)
	       (incf m 1))
	      (t
	       (incf i 1))))
	   (t 
	    (cond 
	      ((> (elt table i) -1)
	       (incf m (- i (elt table i)))
	       (setf i (elt table i)))
	      (t
	       (setf i 0)
	       (incf m 1))))))
    (reverse res)))

;; Boyer-Moore-Horsepool

(defun preprocess (needle)
"Proprocess for the Boyer-Moore-Horsepool algorithm
needle:string
return:vector[255] of int"
  (declare (optimize (debug 3) (safety 3)))
  (let ((size-needle   (length needle))
	(table (make-array 256 :element-type 'integer)))
    (loop 
       for i from 0 to 255 
       do
	 (setf (elt table i) size-needle))
    (loop
       for i from 0 to (- size-needle 2)
       do
	 (setf 
	  (aref 
	   table 
	   (char-code (char needle i)))
	  (- size-needle 1 i)))
    table))

(defun algo-boyer-moore-horsepool (needle haystack)
  (let ((table (preprocess needle))
	(size-needle (length needle))
	(size-haystack (length haystack))
	(skip 0)
	(i  0)
	(res nil))
    (loop  
       while (>= (- size-haystack skip) size-needle)
       do
	 (setf i (1- size-needle))
	 (loop 
	    while (and (> i -1) (eql (char haystack (+ skip i)) (char needle i)))
	    do
	      (when (eql i 0)
		(push skip res))
	      (decf i 1))
	 (incf skip (aref 
		     table 
		     (char-code (char haystack (1- (+ skip size-needle)))))))
    (reverse res)))
 
