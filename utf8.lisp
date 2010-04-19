(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline bit-off? bit-val 10xxxxxx-p 
		 utf8-octets-to-unicode utf8-octets-to-string
		 utf8-string-to-octets))

;;;;;;;;;;;;
;;; constant
#-SBCL
(defconst-onceonly +UNICODE=>UTF8+
  (let ((table (make-array #x10000)))
    (loop FOR code FROM #x0 BELOW #x100 DO
      (setf (aref table code) 
	    (coerce `(,code) 
		    '(vector octet))))
    (loop FOR code FROM #x100 BELOW #x800 DO
      (setf (aref table code) 
	    (coerce `(,(+ #b11000000 (ldb (byte 5 6) code))
		      ,(+ #b10000000 (ldb (byte 6 0) code)))
		    '(vector octet))))
    (loop FOR code FROM #x800 BELOW #x10000 DO
      (setf (aref table code) 
	    (coerce `(,(+ #b11100000 (ldb (byte 4 12) code))
		      ,(+ #b10000000 (ldb (byte 6 6) code))
		      ,(+ #b10000000 (ldb (byte 6 0) code)))
		    '(vector octet))))
    table))

;;;;;;;;;;;;;;;;;;;;;
;;; internal function
(defun bit-off? (pos octet)
  (not (ldb-test (byte 1 pos) octet)))

(defun bit-val (length octet &optional (shift 0))
  (ash (ldb (byte length 0) octet) shift))

(defun 10xxxxxx-p (octet)
  (= (ldb (byte 2 6) octet) #b10))

(defun utf8-call-replace-fn (replace-fn octets pos)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (fixnum pos)
	   (function replace-fn))
  (multiple-value-bind (new-char #1=consuming-octets-count)
		       (funcall replace-fn octets pos)
    (check-type new-char character)
    (check-type #1# (or null positive-fixnum))
    (values (char-code new-char) 
	    (the positive-fixnum (+ pos (the positive-fixnum (or #1# 1)))))))
		
(defun utf8-octets-to-unicode(octets pos string j replace-fn octets-len &aux (os octets))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (simple-octets os)
	   (fixnum pos j octets-len)
	   ((simple-array character) string))
  (macrolet ((with-validate (num exp)
               `(if (and (< (+ ,num pos) octets-len)
			 ,@(loop FOR i fixnum FROM 1 TO num 
			     COLLECT `(10xxxxxx-p (aref os (+ ,i pos)))))
		    (values ,exp 
			    (the positive-fixnum (+ ,(1+ num) pos)))
		  (utf8-call-replace-fn replace-fn octets pos))))

    (let ((octet (aref os pos)))
      (multiple-value-bind (code new-pos)
	        ;; #b0xxxxxxx
          (cond ((bit-off? 7 octet) (values octet (1+ pos)))
	       
	        ;; #b10xxxxxx
	        ((bit-off? 6 octet) (utf8-call-replace-fn replace-fn os pos)) 
	       
	        ;; #b110xxxxx #b10xxxxxx
	        ((bit-off? 5 octet) (with-validate 1
	                              (+ (bit-val 5 octet 6)  
			  	         (bit-val 6 (aref os (+ 1 pos))))))
	      
		;; #b1110xxxx #b10xxxxxx #b10xxxxxx
		((bit-off? 4 octet) (with-validate 2
		                      (+ (bit-val 4 octet 12) 
					 (bit-val 6 (aref os (+ 1 pos)) 6)
					 (bit-val 6 (aref os (+ 2 pos))))))

		;; #b11110xxx #b10xxxxxx #b10xxxxxx #b10xxxxxx 
		((bit-off? 3 octet) (with-validate 3
		                      (+ (bit-val 3 octet 18) 
					 (bit-val 6 (aref os (+ 1 pos)) 12)
					 (bit-val 6 (aref os (+ 2 pos)) 6)
					 (bit-val 6 (aref os (+ 3 pos))))))
	      
		(t                  (utf8-call-replace-fn replace-fn os pos)))
	  (setf (aref string j) (code-char code))
	  (the fixnum new-pos)))))

;;;;;;;;;;;;;;;;;;;;
;;; octets => string
(defun utf8-octets-to-string (octets replace-fn &aux (len (length octets)))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (simple-octets octets))
  (let ((buf (make-array len :element-type 'character))
	(j -1))
    (declare (fixnum j))
    (do ((i 0 (utf8-octets-to-unicode octets i buf (incf j) replace-fn len)))
	((>= i len) (subseq buf 0 (1+ j)))
      (declare (fixnum i)))))

;;;;;;;;;;;;;;;;;;;;
;;; string => octets
(defun utf8-string-to-octets (string)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0))
	   (simple-string string))
  ;; TODO: 全部自作する
  #+SBCL (sb-ext:string-to-octets string :external-format :utf-8)
  #-SBCL
  (let ((len 0))
    (declare (fixnum len))
    
    (loop FOR ch ACROSS string
	  FOR cd = (char-code ch) DO
      (cond ((< cd #x80)    (incf len 1))
	    ((< cd #x800)   (incf len 2))
	    ((< cd #x10000) (incf len 3))
	    (t              (incf len 4))))

    ;; TODO: if len = str-len
    (let ((octets (make-array len :element-type 'octet))
	  (i -1))
      (declare (fixnum i))
      (loop FOR ch ACROSS string 
	    FOR cd = (char-code ch) DO
        (if (< cd #x10000)
	    (loop FOR o ACROSS (the simple-octets (svref +UNICODE=>UTF8+ cd)) DO
		  (setf (aref octets (incf i)) o))
	  (setf (aref octets (incf i)) (+ #b11110000 (ldb (byte 3 18) cd))
		(aref octets (incf i)) (+ #b10000000 (ldb (byte 6 12) cd))
		(aref octets (incf i)) (+ #b10000000 (ldb (byte 6 6) cd))
		(aref octets (incf i)) (+ #b10000000 (ldb (byte 6 0) cd)))))
      octets)))