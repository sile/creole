(in-package :creole)

(declaim (inline read-little-endian-byte to-signed))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it ,then ,else)))

(defmacro muffle-warn (&body body)
  `(locally 
    #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

;; TODO: なくす
(defmacro ensure-function (function-desginator)
  `(etypecase ,function-desginator
     (function)
     (symbol (setf ,function-desginator (symbol-function ,function-desginator)))))

(defmacro each-char-code ((code string &optional return) &body body)
  (let ((char (gensym)))
    `(loop FOR ,char ACROSS ,string
	   FOR ,code = (char-code ,char) 
       DO ,@body
       FINALLY (return ,return))))

(defmacro ensure-simple-characters (s &body body)
  `(let ((,s (etypecase ,s
               (simple-base-string (make-array (length ,s) 
					       :element-type 'character 
					       :initial-contents ,s))
	       (simple-characters ,s)
	       (string (muffle-warn (copy-seq ,s))))))
     (declare (simple-characters ,s))
     ,@body))
  
(defmacro defconst-onceonly (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun to-signed (num byte-size)
  (if (< num (ash #x80 (* (1- byte-size) 8)))
      num
    (- num (ash #x100 (* (1- byte-size) 8)))))

(defun read-little-endian-byte (stream byte-size)
  (let ((byte 0))
    (dotimes (i byte-size (to-signed byte byte-size))
      (setf (ldb (byte 8 (* i 8)) byte)
	    (the octet (read-byte stream))))))

(defun read-little-endian-bytes (in byte-size count)
  (declare ((integer 1 4) byte-size)
	   (fixnum count))
  (let ((buf (make-array count :element-type `(signed-byte ,(* byte-size 8)))))
    (dotimes (i count buf)
      (setf (aref buf i) (read-little-endian-byte in byte-size)))))