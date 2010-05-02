(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline get-decode-trie general-octets-to-string))

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(defvar *decode-tries* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun get-decode-trie (external-format)
  (a.if #1=(gethash (external-format-key external-format) *decode-tries*)
      it
    (setf #1# (load-trie 
	       (merge-pathnames
		(external-format-filename external-format)
		(merge-pathnames #P"decode/" *data-dir*))))))

(defun general-octets-to-string (octets trie)
  (declare #.*fastest*
	   (simple-octets octets))
  (let* ((len (length octets))
	 (buf (make-array len :element-type 'character))
	 (tail-pos -1)
	 (i 0)
	 (char nil))
    (declare (fixnum tail-pos i))
    (loop (setf (values char i) (to-unicode octets i trie))

	  (setf (aref buf (incf tail-pos)) (or char +UNKNOWN-CHAR+))
	  (when (>= i len)
	    (return)))
    (subseq buf 0 (1+ tail-pos))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun octets-to-string (octets &key (external-format *default-external-format*) (safe t))
  (declare #.*fastest*)
  (check-type octets simple-octets)
  (locally
   (declare (simple-octets octets))
   (case (external-format-key external-format)
	 (:|utf-8| (utf8-octets-to-string octets safe))
	 (:|utf-16be| (utf16-octets-to-string octets :be))
	 (:|utf-16le| (utf16-octets-to-string octets :le))
	 (t (general-octets-to-string octets (get-decode-trie external-format))))))
