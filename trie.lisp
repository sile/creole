(in-package :creole)

(declaim (inline next-index to-unicode))

(defstruct trie
  (base #() :type (simple-array (or character fixnum)) :read-only t)
  (chck #() :type (simple-array (signed-byte 16))      :read-only t)
  (ascii-compatible? t :type boolean                   :read-only t))

(defun next-index (node code)
  (+ node code))

(defun ascii-compatible? (base chck)
  (let ((root (aref base 0)))
    (loop FOR ascii FROM 0 BELOW #x80
	  FOR idx = (next-index root ascii)
	  FOR child = (aref base idx)
      ALWAYS (and (= (aref chck idx) ascii)
		  (characterp child)
		  (char= child (code-char ascii))))))

(defun load-trie (path)
  (with-open-file (in path :element-type 'octet)
    (let* ((size (/ (file-length in) 6))
	   (base (map 'vector (lambda (a) (if (minusp a) (code-char (1- (- a))) a))
		      (read-little-endian-bytes in 4 size)))
	   (chck (progn (file-position in (* size 4))
			(read-little-endian-bytes in 2 size))))
      (make-trie :base base
		 :chck chck
		 :ascii-compatible? (ascii-compatible? base chck)))))

(defun to-unicode (octets start trie)
  (when (and (trie-ascii-compatible? trie)
	     (< (aref octets start) #x80))
    (return-from to-unicode (values (code-char (aref octets start)) (1+ start))))

  (symbol-macrolet ((base (trie-base trie))
		    (chck (trie-chck trie)))
    (let ((node (aref base 0)))
      (loop FOR i fixnum FROM start BELOW (length octets) 
	    FOR code OF-TYPE octet = (aref octets i) 
	    FOR idx OF-TYPE array-index = (next-index (the fixnum node) code) DO
        (setf node (aref base idx))
	
	(unless (= (aref chck idx) code)
	  (return-from to-unicode (values nil (1+ start))))

	(when (characterp node)
	  (return-from to-unicode (values node (1+ i)))))))
  (values nil (1+ start)))