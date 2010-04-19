(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline get-encode-table string-to-octets legal-string-to-octets)
	 (ftype (function (string &key (:external-format t) (:replace-fn t)) simple-octets) string-to-octets))

;;;;;;;;;;;;;;;;;;;;
;;; special variable
(defvar *encode-tables* (make-hash-table :test #'eq))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun load-encode-table (external-format)
  (with-open-file (in (merge-pathnames
		       (external-format-filename external-format)
		       (merge-pathnames #P"encode/" *data-dir*)))
    (let* ((table (make-array char-code-limit :initial-element nil))
	   (max-code 
	    (loop FOR (unicode . octets) IN (read in)
	      DO
	        (setf (aref table unicode) (coerce octets '(vector octet)))
	      MAXIMIZE unicode)))
      (subseq table 0 (1+ max-code)))))
    
(defun get-encode-table (external-format)
  (a.if #1=(gethash (external-format-key external-format) *encode-tables*)
      it
    (setf #1# (load-encode-table external-format))))

(defun illegal-character-error (char-code)
  (error "Unable to encode character ~D." char-code))

(defun legal-string-to-octets (string octets-length table)
  (let ((buf (make-array octets-length :element-type 'octet)))
    (loop WITH i OF-TYPE fixnum = -1
	  FOR ch ACROSS string 
	  FOR octets = (svref table (char-code ch)) DO
      (loop FOR o ACROSS (the simple-octets octets) DO
        (setf (aref buf (incf i)) o)))
    buf))

(defun illegal-string-to-octets (string table replace-fn)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (simple-string string)
	   (function replace-fn)
	   (simple-vector table))
  (let ((buf (make-array (length string) :fill-pointer 0
					 :adjustable t
					 :element-type 'octet))
	(code-limit (length table)))
    (loop FOR i OF-TYPE fixnum FROM 0 BELOW (length string)
	  FOR ch = (char string i)
	  FOR cd = (char-code ch)
	  FOR octets = (and (< cd code-limit) (svref table cd)) DO
      (when (null octets)
	(let ((new-octets (funcall replace-fn cd)))
	  (check-type new-octets simple-octets)
	  (setf octets new-octets)))

      (loop FOR o ACROSS (the simple-octets octets) DO
        (vector-push-extend o buf)))
    (coerce buf '(vector octet))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun string-to-octets (string &key (external-format *default-external-format*)
				     (replace-fn #'illegal-character-error))
  (declare (optimize (speed 3) (debug 1) (safety 0))
	   (string string))
  (ensure-function replace-fn)
  (ensure-simple-string string)
  
  (case (external-format-key external-format)
    (:|utf-8| (utf8-string-to-octets string))
    (:|utf-16be| (utf16-string-to-octets string :be replace-fn))
    (:|utf-16le| (utf16-string-to-octets string :le replace-fn))
    (t 
     (let* ((table (get-encode-table external-format))
	    (code-limit (length (the simple-vector table)))
	    (len (loop WITH len OF-TYPE fixnum = 0
		       FOR ch ACROSS string 
		       FOR cd = (char-code ch)
		       FOR octets = (and (< cd code-limit) (svref table cd))
	           DO (when (null octets)
			(return -1))
		      (incf len (length (the simple-octets octets)))
		   FINALLY (return len))))
       (if (/= len -1)
	   (legal-string-to-octets string len table)
	 (illegal-string-to-octets string table replace-fn))))))