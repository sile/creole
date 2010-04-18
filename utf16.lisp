(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline low-surrogate high-surrogate 
		 decode-surrogate-pair to-utf16le-code to-utf16be-code))

;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary function
(defun low-surrogate (code)
  (+ #xDC00 (ldb (byte 10 0) code)))

(defun high-surrogate (code)
  (+ #xD800 (- (ldb (byte 11 10) code) #b1000000)))

(defun to-utf16be-code (octets start)
  (+ (ash (aref octets (+ start 0)) 8)
     (ash (aref octets (+ start 1)) 0)))

(defun to-utf16le-code (octets start)
  (+ (ash (aref octets (+ start 0)) 0)
     (ash (aref octets (+ start 1)) 8)))

(defun decode-surrogate-pair (high low)
  (code-char (+ #x10000
		(ash (ldb (byte 10 0) high) 10)
		(ash (ldb (byte 10 0) low)  00))))

;;;;;;;;;;;;;;;;;;;;
;;; string => octets
(defmacro utf16-string-to-octets (string endian)
  (symbol-macrolet ((p1 (if (eq endian :be) 8 0))
		    (p2 (if (eq endian :be) 0 8)))
    `(let ((octets (make-array (* 4 (length ,string))
			       :element-type 'octet))
	   (i -1))
       (declare (fixnum i))
       (loop FOR char ACROSS string 
	     FOR code = (char-code char) DO
         (if (< code #x10000)
	     (setf (aref octets (incf i)) (ldb (byte 8 ,p1) code)
		   (aref octets (incf i)) (ldb (byte 8 ,p2) code))
	   (let ((low  (low-surrogate code))
		 (high (high-surrogate code)))
	     (setf (aref octets (incf i)) (ldb (byte 8 ,p1) high)
		   (aref octets (incf i)) (ldb (byte 8 ,p2) high)
		   (aref octets (incf i)) (ldb (byte 8 ,p1) low)
		   (aref octets (incf i)) (ldb (byte 8 ,p2) low)))))
       (subseq octets 0 (1+ i)))))

;;;;;;;;;;;;;;;;;;;;
;;; octets => string
(defun utf16be-octets-to-string (octets replace-fn &aux (octets-len (length octets)))
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0))
	   (simple-octets octets)
	   (function replace-fn))
  (let ((buf (make-array octets-len :element-type 'character))
	(pos -1)
	(limit (if (evenp octets-len) octets-len (1- octets-len))))
    (declare (fixnum pos))
    
    (macrolet ((with-replace ((new-char) &body body)
                 `(multiple-value-bind (,new-char #1=consuming-octets-count)
				      (funcall replace-fn octets (- i 2))
                   (check-type ,new-char character)
		   (check-type #1# (or null positive-fixnum))
		   (setf surrogate nil
			 i (+ (- i 2) (the positive-fixnum (or #1# 1))))
		   ,@body)))
		   
      (loop WITH surrogate = nil
	    FOR i fixnum FROM 0 BELOW limit BY 2
	    FOR code = (to-utf16be-code octets i)
        DO
	(cond ((<= #xDC00 code #xDFFF)
	       (if (null surrogate)
		   (with-replace (new-char)
		     (setf (aref buf (incf pos)) new-char))
		 (setf (aref buf (incf pos)) (decode-surrogate-pair surrogate code)
		       surrogate nil)))
	      (surrogate 
	       (with-replace (new-char)
	         (setf (aref buf (incf pos)) new-char)))
	      ((<= #xD800 code #xDBFF) 
	       (setf surrogate code))
	      (t 
	       (setf (aref buf (incf pos)) (code-char code)))))
      (subseq buf 0 (1+ pos)))))

(defun utf16le-octets-to-string (octets replace-fn &aux (octets-len (length octets)))
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0))
	   (simple-octets octets)
	   (function replace-fn))
  (let ((buf (make-array octets-len :element-type 'character))
	(pos -1)
	(limit (if (evenp octets-len) octets-len (1- octets-len))))
    (declare (fixnum pos))
    
    (macrolet ((with-replace ((new-char) &body body)
                 `(multiple-value-bind (,new-char #1=consuming-octets-count)
				      (funcall replace-fn octets (- i 2))
                   (check-type ,new-char character)
		   (check-type #1# (or null positive-fixnum))
		   (setf surrogate nil
			 i (+ (- i 2) (the positive-fixnum (or #1# 1))))
		   ,@body)))
		   
      (loop WITH surrogate = nil
	    FOR i fixnum FROM 0 BELOW limit BY 2
	    FOR code = (to-utf16le-code octets i)
        DO
	(cond ((<= #xDC00 code #xDFFF)
	       (if (null surrogate)
		   (with-replace (new-char)
		     (setf (aref buf (incf pos)) new-char))
		 (setf (aref buf (incf pos)) (decode-surrogate-pair surrogate code)
		       surrogate nil)))
	      (surrogate 
	       (with-replace (new-char)
	         (setf (aref buf (incf pos)) new-char)))
	      ((<= #xD800 code #xDBFF) 
	       (setf surrogate code))
	      (t 
	       (setf (aref buf (incf pos)) (code-char code)))))
      (subseq buf 0 (1+ pos)))))
