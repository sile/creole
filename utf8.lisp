(in-package :creole)

;;;;;;;;;;;
;;; declaim
(declaim (inline bit-off? bit-val 10xxxxxx-p 
		 utf8-octets-length
		 utf8-octets-to-unicode utf8-octets-to-string))

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
  (declare #.*fastest*
	   (simple-octets os)
	   (fixnum pos j octets-len)
	   (simple-characters string))
  (macrolet ((with-validate (num exp)
               `(if (and (< (+ ,num pos) octets-len)
			 ;; TOOD: 一番初めは0ではいけない -> check
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
  (declare #.*fastest*
	   (simple-octets octets))
  (let ((buf (make-array len :element-type 'character))
	(j -1))
    (declare (fixnum j))
    (do ((i 0 (utf8-octets-to-unicode octets i buf (incf j) replace-fn len)))
	((>= i len) (subseq buf 0 (1+ j)))
      (declare (fixnum i)))))

;;;;;;;;;;;;;;;;;;;;
;;; string => octets
(defun utf8-octets-length (string &aux (len 0))
  (each-char-code (cd string len)
    (incf (the array-index len)
	  (cond ((< cd #x80)    1)
		((< cd #x800)   2)
		((< cd #x10000) 3)
		(t              4)))))

(defun utf8-string-to-octets (string)
  (declare #.*fastest*
	   (simple-characters string))
  (let ((octets (make-array (utf8-octets-length string)  :element-type 'octet))
	(i 0))
    (declare (optimize-hack-array-index i))
    (macrolet ((add-octets (&rest octet-list &aux (n (length octet-list)))
                 (declare (optimize (speed 0)))
                 `(progn ,@(loop FOR i FROM 0 BELOW n 
				 FOR o IN octet-list COLLECT
                             `(setf (aref octets (+ i ,i)) ,o))
			 (incf i ,n))))
      (each-char-code (cd string)
        (cond ((< cd #x80)
	       (add-octets cd))
	      ((< cd #x800)
	       (add-octets (+ #b11000000 (ldb (byte 5 6) cd))
			   (+ #b10000000 (ldb (byte 6 0) cd))))
	      ((< cd #x10000)
	       (if (<= #xD800 cd #xDFFF)
		   (add-octets +UNKNOWN-CODE+)
		 (add-octets (+ #b11100000 (ldb (byte 4 12) cd))
			     (+ #b10000000 (ldb (byte 6 6) cd))
			     (+ #b10000000 (ldb (byte 6 0) cd)))))
	      (t
	       (add-octets (+ #b11110000 (ldb (byte 3 18) cd))
			   (+ #b10000000 (ldb (byte 6 12) cd))
			   (+ #b10000000 (ldb (byte 6 6) cd))
			   (+ #b10000000 (ldb (byte 6 0) cd)))))))
    (if (= i (length octets))
	(values octets t)
      (values (subseq octets i) nil))))