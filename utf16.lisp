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
    `(let* ((has-surrogate? (each-char-code (cd ,string nil)
                              (when (>= cd #x10000)
				(return t))))
	    (buf-len (* (if has-surrogate? 4 2) (length ,string)))
	    (octets (make-array buf-len :element-type 'octet))
	    (i 0))
       (declare (optimize-hack-array-index i))
       (macrolet ((add-octets (&rest octet-list &aux (n (length octet-list)))
			      (declare (optimize (speed 0)))
			      `(progn ,@(loop FOR i FROM 0 BELOW n 
					      FOR o IN octet-list COLLECT
					      `(setf (aref octets (+ i ,i)) ,o))
				      (incf i ,n))))
         (if (not has-surrogate?)
	     (each-char-code (cd ,string octets)
	       #1=(if (<= #xD800 cd #xDFFF)
		      (add-octets (ldb (byte 8 ,p1) +UNKNOWN-CODE+)
				  (ldb (byte 8 ,p2) +UNKNOWN-CODE+))
		    (add-octets (ldb (byte 8 ,p1) cd)
				(ldb (byte 8 ,p2) cd))))
	   (each-char-code (cd ,string (subseq octets 0 i))
	     (if (< cd #x10000)
		 #1#
	       (let ((low  (low-surrogate  cd))
		     (high (high-surrogate cd)))
		 (add-octets (ldb (byte 8 ,p1) high)
			     (ldb (byte 8 ,p2) high)
			     (ldb (byte 8 ,p1) low)
			     (ldb (byte 8 ,p2) low))))))))))

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