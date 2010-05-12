(in-package :creole)

(declaim (inline read-little-endian-byte to-signed to-simple-characters))

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it ,then ,else)))

(defmacro each-code ((code charseq &key return) &body body)
  `(charseq:each (#1=#:char ,charseq ,return t)
     (let ((,code (char-code #1#)))
       ,@body)))

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
