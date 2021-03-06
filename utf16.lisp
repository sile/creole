(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline low-surrogate high-surrogate 
		 to-utf16le-code to-utf16be-code))

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

;; see: http://d.hatena.ne.jp/sile/20100502/1272815686
(defmacro decode-surrogate-pair (high low)
  `(code-char (+ #x10000
		 (ash (ldb (byte 10 0) ,high) 10)
		 (ash (ldb (byte 10 0) ,low)  00))))

;;;;;;;;;;;;;;;;;;;;
;;; string => octets
(defmacro utf16-string-to-octets (charseq endian)
  (symbol-macrolet ((p1 (if (eq endian :be) 8 0))
		    (p2 (if (eq endian :be) 0 8)))
    `(let* ((has-surrogate? (each-code (cd ,charseq :return nil)
                              (when (>= cd #x10000)
				(return t))))
	    (buf-len (* (if has-surrogate? 4 2) (charseq:length ,charseq)))
	    (legal-octets? t)
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
	     (each-code (cd ,charseq :return (values octets legal-octets?))
	       #1=(progn
		    (when (<= #xD800 cd #xDFFF)
		      (setf cd +UNKNOWN-CODE+
			    legal-octets? nil))
		    (add-octets (ldb (byte 8 ,p1) cd)
				(ldb (byte 8 ,p2) cd))))
	   (each-code (cd ,charseq :return (values (subseq octets 0 i) legal-octets?))
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
(defmacro utf16-octets-to-string (octets start end endian)
  `(let* ((octets-len (- ,end ,start))
	  (buf-len (ceiling octets-len 2))
	  (buf (make-array buf-len :element-type 'character))
	  (pos -1)
	  (legal-octets? t))
     (declare (fixnum pos))
     (flet ((add-char (char) (setf (aref buf (incf pos)) char))
	    (add-unk-char () (setf (aref buf (incf pos)) +UNKNOWN-CHAR+
				   legal-octets? nil)))
       (declare (inline add-char add-unk-char))
       (loop WITH surrogate = nil
	     FOR i FROM ,start BELOW (1- ,end) BY 2
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
	     legal-octets?)))