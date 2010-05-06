(in-package :creole)

(declaim (inline read-little-endian-byte to-signed to-simple-characters))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it ,then ,else)))

(defmacro muffle-warn (&body body)
  `(locally 
    #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

(defmacro each-char-code ((code string &key return (start 0) end) &body body)
  (let ((i (gensym)))
    `(do* ((,i ,start (1+ ,i))
	   (,code #1=(char-code (char ,string ,i)) #1#))
	  ((>= ,i ,(or end `(length ,string))) ,return)
       (declare (array-index ,i))
       ,@body)))

(defun to-simple-characters (source start end)
  (let ((dist (make-array (- end start) :element-type 'character)))
    (loop FOR i FROM start BELOW end 
	  FOR j FROM 0 DO
      (setf (aref dist j) (muffle-warn (aref source i))))
    dist))

(defmacro ensure-simple-characters ((s start end) &body body)
  `(multiple-value-bind (,s ,start ,end)
     (etypecase ,s
       (simple-characters (values ,s ,start ,end))
       (string (values (to-simple-characters ,s ,start ,end) ,start ,end)))
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
