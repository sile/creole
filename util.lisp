(in-package :creole)

(defmacro a.if (exp then else)
  `(let ((it ,exp))
     (if it ,then ,else)))

(defmacro muffle-warn (&body body)
  `(locally 
    #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    ,@body))

(defun read-seq (in size) 
  (let ((buf (make-array size :element-type (stream-element-type in)))) 
    (read-sequence buf in) buf))

(defmacro ensure-function (function-desginator)
  `(etypecase ,function-desginator
     (function)
     (symbol (setf ,function-desginator (symbol-function ,function-desginator)))))

(defmacro ensure-simple-string (string)
  `(etypecase ,string
     (simple-string)
     (string (setf ,string (muffle-warn (coerce ,string 'simple-string))))))
  
(defmacro defconst-onceonly (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
