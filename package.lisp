(defpackage creole
  (:use :common-lisp)
  (:export string-to-octets
	   octets-to-string
	   octets-encoding-error ;; TODO
	   octet-decoding-error  ;; TODO
	   illegal-octet-error
	   illegal-character-error
	   *external-formats*
	   *default-external-format*))
(in-package :creole)

(defparameter *data-dir* 
  (make-pathname 
   :directory (pathname-directory 
	       (merge-pathnames #P"data/" *load-pathname*))))
						       
(deftype octet () '(unsigned-byte 8))
(deftype simple-octets () '(simple-array octet))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))