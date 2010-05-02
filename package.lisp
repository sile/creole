(defpackage creole
  (:use :common-lisp)
  (:export string-to-octets
	   octets-to-string
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
(deftype array-index () '(integer 0 #.(1- array-total-size-limit)))
(deftype simple-characters () '(simple-array character (*)))

(deftype optimize-hack-array-index () #+SBCL '(integer 0 100) 
                                      #-SBCL 'array-index)

(defvar *fastest* '(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))

