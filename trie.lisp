(in-package :creole)

(declaim (inline next-index to-unicode))

(defstruct trie
  (base #() :type (simple-array (or character fixnum)) :read-only t)
  (chck #() :type (simple-array (signed-byte 16))      :read-only t))

(defun load-trie (path)
  (with-open-file (in path :element-type 'octet)
    (let ((size (/ (file-length in) 6)))
      (make-trie
       :base (map 'vector (lambda (a) (if (minusp a) (code-char (1- (- a))) a))
		  (read-little-endian-bytes in 4 size))
       :chck (progn (file-position in (* size 4))
		    (read-little-endian-bytes in 2 size))))))

(defun next-index (node code)
  (+ node code))

(defun to-unicode (octets start trie)
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (simple-octets octets)
	   (fixnum start)
	   (trie trie))

  (when (< (aref octets start) #x80)
    (return-from to-unicode (values (code-char (aref octets start)) (1+ start))))

  (symbol-macrolet ((base (trie-base trie))
		    (chck (trie-chck trie)))
    (let ((node (aref base 0)))
      (loop FOR i fixnum FROM start BELOW (length octets) 
	    FOR code OF-TYPE octet = (aref octets i) 
	    FOR idx OF-TYPE fixnum = (next-index (the fixnum node) code) DO
        (setf node (aref base idx))
	
	(unless (= (aref chck idx) code)
	  (return-from to-unicode (values nil (1+ start))))

	(when (characterp node)
	  (return-from to-unicode (values node (1+ i)))))))
  (values nil (1+ start)))