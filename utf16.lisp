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
(defmacro utf16-octets-to-string (octets endian)
  `(let* ((octets-len (length ,octets))
	  (buf-len (ceiling octets-len 2))
	  (buf (make-array buf-len :element-type 'character))
	  (pos -1)
	  (including-illegal-octet? nil))
     (declare (fixnum pos))
     (flet ((add-char (char) (setf (aref buf (incf pos)) char))
	    (add-unk-char () (setf (aref buf (incf pos)) +UNKNOWN-CHAR+
				   including-illegal-octet? t)))
       (declare (inline add-char add-unk-char))
       (loop WITH surrogate = nil
	     FOR i FROM 0 BELOW (1- octets-len) BY 2
	     FOR code = (,(if (eq endian :le) 'to-utf16le-code 'to-utf16be-code) ,octets i)
         DO
	 (cond ((<= #xDC00 code #xDFFF)
		(if (null surrogate)
		    (add-unk-char)
		  (progn 
		    (add-char (decode-surrogate-pair surrogate code))
		    (setf surrogate nil))))
	       (surrogate 
		(add-unk-char)
		(setf surrogate nil))
	       ((<= #xD800 code #xDBFF) 
		(setf surrogate code))
	       (t 
		(add-char (code-char code)))))
       (when (oddp octets-len)
	 (add-unk-char)))
     (values (if (= (1+ pos) buf-len) buf (subseq buf 0 (1+ pos)))
	     (not including-illegal-octet?))))