(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline get-encode-table legal-string-to-octets illegal-string-to-octets)
	 (ftype (function (string &key (:external-format t)
				       (:start array-index)
				       (:end   array-index))
			  (values simple-octets boolean)) string-to-octets)
	 (ftype (function (t) simple-vector) get-encode-table))

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

(defun legal-string-to-octets (string octets-length table start end)
  (let ((buf (make-array octets-length :element-type 'octet))
	(i -1))
    (each-char-code (code string :start start :end end :return (values buf t))
      (loop FOR o ACROSS (the simple-octets (svref table code)) DO
        (setf (aref buf (incf (the fixnum i))) o)))))

(defun illegal-string-to-octets (string octets-length table start end)
  (let ((buf (make-array octets-length :element-type 'octet))
	(code-limit (length table))
	(i -1))
    (each-char-code (code string :start start :end end :return (values buf nil))
      (let ((octets (or (and (< code code-limit) (svref table code))
			+UNKNOWN-OCTETS+)))
	(loop FOR o ACROSS (the simple-octets octets) DO
	  (setf (aref buf (incf (the fixnum i))) o))))))

;;;;;;;;;;;;;;;;;;;;;
;;; external function
(defun string-to-octets (string &key (external-format *default-external-format*)
				     (start 0)
				     (end (length string)))
  (declare #.*interface*)
  (ensure-simple-characters string
   (let ((end (min end (length string))))
     (declare #.*fastest*)
     (when (> start end)
       (return-from string-to-octets (values (coerce '() '(vector octet)) t)))
     
     (case (external-format-key external-format)
       (:|utf-8| (utf8-string-to-octets string start end))
       (:|utf-16be| (utf16-string-to-octets string start end :be))
       (:|utf-16le| (utf16-string-to-octets string start end :le))
       (t 
	(let* ((table (get-encode-table external-format))
	       (code-limit (length table))
	       (including-illegal-character? nil)
	       (len 0))
	  (declare (fixnum len))
	  (each-char-code (code string :start start :end end)
	    (let ((octets (or (and (< code code-limit) (svref table code))
			      (progn (setf including-illegal-character? t)
				     +UNKNOWN-OCTETS+))))
	      (incf len (length (the simple-octets octets)))))
	  (if including-illegal-character?
	      (illegal-string-to-octets string len table start end)
	    (legal-string-to-octets string len table start end))))))))