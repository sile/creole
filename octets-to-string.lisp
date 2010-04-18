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

(defun illegal-octet-error (octets position)
  (declare (ignore octets))
  (error "Illegal character starting at byte position ~D." position))

(defun general-octets-to-string (octets trie replace-fn)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (simple-octets octets)
	   (function replace-fn))
  (let* ((len (length octets))
	 (buf (make-array len :element-type 'character))
	 (tail-pos -1)
	 (i 0)
	 (char nil))
    (declare (fixnum tail-pos i))
    (loop (setf (values char i) (to-unicode octets i trie))

	  (when (null char)
	    (multiple-value-bind (new-char #1=consuming-octets-count) 
				 (funcall replace-fn octets (1- i))
	      (check-type new-char character)
	      (check-type #1# (or null positive-fixnum))
	      (setf char new-char)
	      (when #1#
		(incf i (1- (the positive-fixnum #1#))))))

	  (setf (aref buf (incf tail-pos)) char)
	  (when (>= i len)
	    (return)))
    (subseq buf 0 (1+ tail-pos))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun octets-to-string (octets &key (external-format *default-external-format*)
				     (replace-fn #'illegal-octet-error))
  (ensure-function replace-fn)
  (check-type octets simple-octets)
  
  (case (external-format-key external-format)
    (:|utf-8| (utf8-octets-to-string octets replace-fn))
    (:|utf-16be| (utf16be-octets-to-string octets replace-fn))
    (:|utf-16le| (utf16le-octets-to-string octets replace-fn))
    (t (general-octets-to-string octets 
				 (get-decode-trie external-format) 
				 replace-fn))))