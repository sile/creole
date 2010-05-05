(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline get-decode-trie general-octets-to-string)
	 (ftype (function (simple-octets &key (:external-format t)
                                              (:start array-index)
					      (:end   array-index)) 
			  (values simple-characters boolean)) octets-to-string))

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

(defun general-octets-to-string (octets start end trie)
  (let* ((buf (make-array (- end start) :element-type 'character))
	 (tail-pos -1)
	 (i start)
	 (legal-octets? t)
	 (char nil))
    (declare (fixnum tail-pos i))
    (loop (setf (values char i) (to-unicode octets i trie))

	  (setf (aref buf (incf tail-pos)) (or char 
					       (progn (setf legal-octets? nil)
						      +UNKNOWN-CHAR+)))
	  (when (>= i end)
	    (return)))
    (values (subseq buf 0 (1+ tail-pos)) legal-octets?)))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun octets-to-string (octets &key (external-format *default-external-format*)
				     (start 0)
				     (end (length octets)))
  (declare #.*interface*)
  #-SBCL (check-type octets simple-octets)
  (let ((end (min end (length octets))))
    (declare #.*fastest*)
    (when (> start end)
      (return-from octets-to-string (values (string "") t)))
    (case (external-format-key external-format)
      (:|utf-8| (utf8-octets-to-string octets start end))
      (:|utf-16be| (utf16-octets-to-string octets start end :be))
      (:|utf-16le| (utf16-octets-to-string octets start end :le))
      (t (general-octets-to-string octets start end (get-decode-trie external-format))))))