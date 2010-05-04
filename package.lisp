(defpackage creole
  (:use :common-lisp)
  (:export string-to-octets
	   octets-to-string
	   *external-formats*
	   *default-external-format*))
(in-package :creole)

(defvar *data-dir* 
  (make-pathname 
   :directory (pathname-directory 
	       (merge-pathnames #P"data/" *load-pathname*))))
						       
(deftype octet () '(unsigned-byte 8))
(deftype simple-octets () '(simple-array octet))
(deftype simple-characters () '(simple-array character (*)))
(deftype array-index () '(integer 0 #.(1- array-total-size-limit)))
(deftype optimize-hack-array-index () #+SBCL '(integer 0 100) 
                                      #-SBCL 'array-index)

(defparameter *fastest*   '(optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
(defparameter *interface* '(optimize (speed 3) (debug 1) (safety 1) (compilation-speed 0)))